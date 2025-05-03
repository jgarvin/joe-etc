#!/usr/bin/env python3

from __future__ import annotations
import argparse
import os
import re
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple


def get_proc_cmdline(pid: int) -> str:
    """Read command line from /proc/[pid]/cmdline file."""
    try:
        with open(f"/proc/{pid}/cmdline", "rb") as f:
            # /proc/[pid]/cmdline contains args separated by null bytes
            cmdline = f.read().split(b'\0')
            # Filter out empty strings (the last entry is usually empty)
            cmdline = [arg.decode('utf-8', errors='replace') for arg in cmdline if arg]

            if not cmdline:
                return "unknown"

            cmd_name = os.path.basename(cmdline[0])

            # Format with the binary name and up to 3 arguments for readability
            if len(cmdline) > 1:
                visible_args = cmdline[1:min(4, len(cmdline))]
                args_formatted = " ".join(visible_args)
                if len(cmdline) > 4:
                    args_formatted += " ..."
                return f"{cmd_name} {args_formatted}"

            return cmd_name
    except (FileNotFoundError, PermissionError, ProcessLookupError):
        return "unknown"


def prune_process_tree(processes: Dict[int, Process], ignore_cmds: List[str]) -> Dict[int, Process]:
    """
    Prune processes that match the ignored commands list, connecting their children to their parents.
    """
    if not ignore_cmds:
        return processes

    print(f"Ignoring commands: {', '.join(ignore_cmds)}")

    # Convert ignore_cmds to lowercase for case-insensitive matching
    ignore_cmds = [cmd.lower() for cmd in ignore_cmds]

    # Continue pruning until no more processes are pruned
    pruned_count = 0
    while True:
        # Find processes to prune
        to_prune = []
        for pid, process in processes.items():
            # Process doesn't have any commands
            if not process.commands:
                continue

            # Check if any command matches an ignored command
            for cmd_str in process.commands:
                # Get the first word (the executable name)
                try:
                    cmd_name = cmd_str.split()[0].lower()
                    if cmd_name in ignore_cmds:
                        to_prune.append(pid)
                        break
                except (IndexError, AttributeError):
                    continue  # Skip if we can't parse the command

        if not to_prune:
            break  # No more processes to prune

        pruned_count += len(to_prune)

        # Reconnect children of pruned processes to their grandparents
        for pid in to_prune:
            process = processes[pid]
            parent_pid = process.parent_pid

            # If the process has a parent and children
            if parent_pid is not None and parent_pid in processes:
                parent = processes[parent_pid]

                # Connect each child to the grandparent
                for child_pid in process.children:
                    if child_pid in processes:
                        # Update child's parent to be the grandparent
                        processes[child_pid].parent_pid = parent_pid
                        # Add child to grandparent's children
                        parent.children.add(child_pid)

                # Remove this process from parent's children
                parent.children.discard(pid)

        # Remove pruned processes from the dictionary
        for pid in to_prune:
            del processes[pid]

    if pruned_count > 0:
        print(f"Pruned {pruned_count} processes matching ignored commands")

    return processes


def main() -> None:
    parser = argparse.ArgumentParser(description="Visualize process tree from a command or process using strace")
    parser.add_argument("command", nargs="*", help="The command to run and trace. Either a command or --pid must be provided.")
    parser.add_argument("-p", "--pid", type=int, help="Attach to an existing process ID. Either a command or --pid must be provided.")
    parser.add_argument("-o", "--output", default="process_tree.svg",
                        help="Output SVG file (default: process_tree.svg)")
    parser.add_argument("-i", "--ignore", nargs="+", default=[],
                        help="List of commands to ignore (basename of argv[0])")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Enable verbose debug output")
    args = parser.parse_args()

    # Validate arguments
    if not args.command and args.pid is None:
        parser.error("Either a command or --pid must be provided")
    if args.command and args.pid is not None:
        parser.error("Cannot provide both a command and --pid")

    # Get initial command for PID if attaching to existing process
    initial_cmd = None
    if args.pid:
        initial_cmd = get_proc_cmdline(args.pid)
        print(f"Attached to PID {args.pid} running: {initial_cmd}")

    # Create a temporary directory for strace output
    with tempfile.TemporaryDirectory(delete=False) as tmpdir:
        print(f"Using {tmpdir=}", file=sys.stderr)

        # Run the command with strace, focusing on process-related system calls
        if args.pid:
            strace_cmd = ["strace",
                          "-ff",                    # Follow forks with output into separate files
                          "-tt",                    # Add timestamps with microsecond precision
                          "-e", "trace=process",    # Only trace process-related system calls
                          "-s", "100",              # Capture up to 100 chars of strings for args
                          "-o", f"{tmpdir}/strace.log",
                          "-p", str(args.pid)]
        else:
            strace_cmd = ["strace",
                          "-ff",                    # Follow forks with output into separate files
                          "-tt",                    # Add timestamps with microsecond precision
                          "-e", "trace=process",    # Only trace process-related system calls
                          "-s", "100",              # Capture up to 100 chars of strings for args
                          "-o", f"{tmpdir}/strace.log",
                          *args.command]

        print(f"Running: {' '.join(strace_cmd)}")

        try:
            # Create a process group for strace
            def preexec_fn():
                # Create a new process group
                os.setpgrp()

            # Start strace in its own process group
            strace_process = subprocess.Popen(
                strace_cmd,
                preexec_fn=preexec_fn,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )

            # Get the process group ID (same as the process ID in this case)
            pgid = strace_process.pid

            print(f"Strace running with PGID {pgid}...")

            # Wait for the initial strace process to exit
            strace_process.wait()

            # Give a short delay to ensure all log files are fully written
            time.sleep(0.5)

            # Check if any strace processes are still running with ps
            def check_strace_processes():
                try:
                    # Run ps to check for strace processes
                    ps_result = subprocess.run(
                        ["ps", "-o", "pid,cmd", "--no-headers", "-g", str(pgid)],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True
                    )

                    # If the output is empty, no processes remain
                    if not ps_result.stdout.strip():
                        return False

                    # Check if any strace processes remain
                    remaining = ps_result.stdout.strip().split('\n')
                    if args.verbose:
                        print(f"Remaining processes in group {pgid}: {len(remaining)}")
                        for proc in remaining:
                            print(f"  {proc}")
                    return len(remaining) > 0

                except subprocess.SubprocessError:
                    # If we can't check, assume none are left
                    return False

            # Wait for all strace processes to exit
            max_wait = 10  # seconds
            start_time = time.time()
            while check_strace_processes():
                if time.time() - start_time > max_wait:
                    print("Warning: Some strace processes still running after timeout, continuing anyway")
                    break
                time.sleep(0.2)

            print("All strace processes completed, processing logs...")

        except KeyboardInterrupt:
            print("\nCommand interrupted, attempting to terminate strace processes...")
            try:
                # Try to kill the entire process group
                os.killpg(pgid, signal.SIGTERM)
                time.sleep(0.5)
            except (OSError, NameError):
                pass
            print("Processing logs...")

        # Process the strace logs and generate the process tree
        process_tree = parse_strace_logs(tmpdir, args.verbose)

        # If we attached to a PID, get the actual start time from /proc
        if args.pid:
            # Try to get the process start time from /proc
            start_time = get_proc_start_time(args.pid, args.verbose)

            if args.pid in process_tree:
                # Only add this if the process doesn't already have commands
                if not process_tree[args.pid].commands and initial_cmd and initial_cmd != "unknown":
                    process_tree[args.pid].add_exec(initial_cmd)

                # Set the start time if we got it from /proc and it's earlier than what we have
                if start_time is not None:
                    if process_tree[args.pid].start_time is None or start_time < process_tree[args.pid].start_time:
                        if args.verbose:
                            print(f"Setting PID {args.pid} start time from /proc: {start_time}")
                            process_tree[args.pid].start_time = start_time

            elif initial_cmd and initial_cmd != "unknown":
                # Create the process if it doesn't exist in the tree
                process = Process(args.pid, command=initial_cmd)
                if start_time is not None:
                    process.start_time = start_time
                    if args.verbose:
                        print(f"Setting PID {args.pid} start time from /proc: {start_time}")
                        process_tree[args.pid] = process

        # Prune processes that match the ignore list
        if args.ignore:
            process_tree = prune_process_tree(process_tree, args.ignore)

        # Calculate self times for coloring
        self_times = calculate_self_times(process_tree, args.verbose)

        # Generate the DOT file
        dot_file = generate_dot(process_tree, self_times, args.verbose)

        # Write the DOT file
        dot_path = Path(f"{tmpdir}/process_tree.dot")
        dot_path.write_text(dot_file)

        # Generate the SVG file
        try:
            subprocess.run(["dot", "-Tsvg", str(dot_path), "-o", args.output], check=True)
            print(f"Process tree visualization saved to {args.output}")
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("Error generating SVG. Make sure Graphviz is installed (apt-get install graphviz).")
            sys.exit(1)


class Process:
    def __init__(self, pid: int, command: str | None = None, parent_pid: Optional[int] = None):
        self.pid = pid
        self.commands: List[str] = []
        self.parent_pid = parent_pid
        self.children: Set[int] = set()
        self.start_time: Optional[float] = None
        self.end_time: Optional[float] = None
        if command is not None:
            self.commands.append(command)

    def add_exec(self, command: str) -> None:
        self.commands.append(command)


def extract_command_args(line: str) -> str:
    """Extract a readable command string from an strace execve line."""
    # Extract the binary path
    cmd_match = re.search(r'execve\("([^"]+)"', line)
    if not cmd_match:
        return "unknown"

    cmd_path = cmd_match.group(1)
    cmd_name = os.path.basename(cmd_path)

    # Try to extract arguments
    args_match = re.search(r'execve\("[^"]+", \[(.*?)(, \[|,.*\]$|\])', line)
    if not args_match:
        return cmd_name

    args_str = args_match.group(1)

    # Extract all quoted strings as arguments
    args = []
    for match in re.finditer(r'"((?:\\"|[^"])*)"', args_str):
        args.append(match.group(1))

    # Skip the first arg (usually just the program name again)
    if len(args) > 1:
        # Format with up to 3 arguments for readability
        visible_args = args[1:min(4, len(args))]
        args_formatted = " ".join(visible_args)
        if len(args) > 4:
            args_formatted += " ..."

        return f"{cmd_name} {args_formatted}"

    return cmd_name


def parse_timestamp(timestamp_str: str) -> Optional[float]:
    """Parse a timestamp string into seconds."""
    # Try parsing as HH:MM:SS.ms format
    if ":" in timestamp_str:
        try:
            time_parts = timestamp_str.split(':')
            hours = int(time_parts[0])
            mins = int(time_parts[1])
            secs = float(time_parts[2])
            return (hours * 3600) + (mins * 60) + secs
        except (ValueError, IndexError):
            pass

    # Try parsing as standard float
    try:
        return float(timestamp_str)
    except ValueError:
        pass

    return None


def get_proc_start_time(pid: int, verbose: bool = False) -> Optional[float]:
    """Get process start time from /proc filesystem."""
    try:
        # Read the process stat file
        with open(f"/proc/{pid}/stat", "r") as f:
            stat = f.read()

        # The stat file has fields separated by spaces, but the command field
        # may contain spaces. It's enclosed in parentheses, so we need to handle that.
        cmd_start = stat.find('(')
        cmd_end = stat.rfind(')')

        if cmd_start == -1 or cmd_end == -1:
            if verbose:
                print(f"Warning: Could not parse command from /proc/{pid}/stat")
            return None

        # Extract parts before and after command
        stat_before_cmd = stat[:cmd_start].strip()
        stat_after_cmd = stat[cmd_end+1:].strip()

        # Combine and split all fields
        fields = stat_before_cmd.split() + ["cmd_placeholder"] + stat_after_cmd.split()

        # The start time is the 22nd field
        if len(fields) < 22:
            if verbose:
                print(f"Warning: Not enough fields in /proc/{pid}/stat: {len(fields)}")
            return None

        # Extract start time in clock ticks since boot
        start_time_ticks = int(fields[21])

        # Get boot time
        boot_time = None
        with open("/proc/stat", "r") as f:
            for line in f:
                if line.startswith("btime "):
                    boot_time = int(line.split()[1])
                    break

        if boot_time is None:
            if verbose:
                print("Warning: Could not get boot time from /proc/stat")
            return None

        # Get clock ticks per second (usually 100 on Linux)
        ticks_per_sec = os.sysconf(os.sysconf_names['SC_CLK_TCK'])

        # Convert to seconds since epoch
        start_time_seconds = boot_time + (start_time_ticks / ticks_per_sec)

        if verbose:
            print(f"Process {pid} start time: {start_time_seconds} " +
                  f"({start_time_ticks} ticks since boot at {boot_time}, " +
                  f"{ticks_per_sec} ticks/sec)")

        return start_time_seconds

    except (FileNotFoundError, ProcessLookupError, ValueError, OSError) as e:
        if verbose:
            print(f"Error getting start time for PID {pid}: {e}")
        return None


def parse_strace_logs(log_dir: str, verbose: bool = False) -> Dict[int, Process]:
    processes: Dict[int, Process] = {}

    # First, find all the log files
    log_files = list(Path(log_dir).glob("strace.log.*"))
    if verbose:
        print(f"Found {len(log_files)} log files")

    # Create Process objects for each PID
    for log_file in log_files:
        try:
            pid = int(log_file.name.split(".")[-1])
            # Explicitly set parent_pid to None
            processes[pid] = Process(pid, parent_pid=None)
        except ValueError:
            continue

    # Parse each log file to find parent-child relationships and execs
    for log_file in log_files:
        try:
            pid = int(log_file.name.split(".")[-1])
        except ValueError:
            continue

        process = processes[pid]
        timestamps = []

        if verbose:
            print(f"\nProcessing log file for PID {pid}: {log_file}")

        # Read all timestamps first
        with open(log_file, "r") as f:
            lines = f.readlines()

            if verbose:
                print(f"  File contains {len(lines)} lines")
                if lines:
                    print(f"  First line: {lines[0].strip()}")
                    if len(lines) > 1:
                        print(f"  Last line: {lines[-1].strip()}")

            # Extract timestamps from all lines
            for line in lines:
                timestamp_match = re.match(r'^(\d+\.\d+|\d{2}:\d{2}:\d{2}\.\d+) ', line)
                if timestamp_match:
                    timestamp_str = timestamp_match.group(1)
                    timestamp = parse_timestamp(timestamp_str)
                    if timestamp is not None:
                        timestamps.append(timestamp)

            # Process lines for other information
            for line in lines:
                # Check for clone/fork/vfork calls to find child processes
                if any(call in line for call in ["clone(", "fork(", "vfork("]):
                    match = re.search(r'= (\d+)$', line.strip())
                    if match and match.group(1) != "0":  # Not returning 0 means this is the parent
                        try:
                            child_pid = int(match.group(1))
                            if child_pid not in processes:
                                # Create a process for this child if it doesn't exist
                                processes[child_pid] = Process(child_pid, parent_pid=pid)
                                if verbose:
                                    print(f"  Created child process {child_pid} for parent {pid}")
                            else:
                                # Update the parent if it wasn't set
                                if processes[child_pid].parent_pid is None:
                                    processes[child_pid].parent_pid = pid
                                    if verbose:
                                        print(f"  Updated parent of {child_pid} to {pid}")

                            processes[pid].children.add(child_pid)
                        except ValueError:
                            continue

                # Check for successful execve calls
                elif "execve(" in line and ") = 0" in line:
                    cmd = extract_command_args(line)
                    processes[pid].add_exec(cmd)
                    if verbose:
                        print(f"  Exec in PID {pid}: {cmd}")

        # Set the process's start and end times if we found any timestamps
        if timestamps:
            if verbose:
                print(f"  Found {len(timestamps)} timestamps")
                print(f"  First timestamp: {timestamps[0]:.6f}")
                print(f"  Last timestamp: {timestamps[-1]:.6f}")
                print(f"  Duration: {timestamps[-1] - timestamps[0]:.6f} seconds")

            process.start_time = timestamps[0]
            process.end_time = timestamps[-1]
        elif verbose:
            print("  No timestamps found in this log file")

    # Ensure all processes have parent_pid attribute
    for pid, process in processes.items():
        if not hasattr(process, 'parent_pid'):
            process.parent_pid = None

    # For processes without commands, try to get them from /proc
    for pid, process in processes.items():
        if not process.commands:
            cmd = get_proc_cmdline(pid)
            if cmd != "unknown":
                process.add_exec(cmd)

    if verbose:
        print("\nProcess tree summary:")
        for pid, process in processes.items():
            print(f"PID {pid}:")
            print(f"  Parent: {process.parent_pid}")
            print(f"  Children: {process.children}")
            print(f"  Start time: {process.start_time}")
            print(f"  End time: {process.end_time}")
            if process.start_time is not None and process.end_time is not None:
                duration = process.end_time - process.start_time
                print(f"  Duration: {duration:.6f} seconds")
            else:
                print(f"  Duration: N/A")
                print(f"  Commands: {process.commands}")

    return processes


def calculate_self_times(processes: Dict[int, Process], verbose: bool = False) -> Dict[int, float]:
    """
    Calculate self time for each process (excluding time spent in children).
    Returns a dictionary mapping PIDs to self times.
    """
    self_times = {}

    if verbose:
        print("\nCalculating self times:")

    for pid, process in processes.items():
        # If process doesn't have both start and end times, set self time to 0
        if process.start_time is None or process.end_time is None:
            self_times[pid] = 0.0
            if verbose:
                print(f"PID {pid}: No valid start/end times, self_time = 0.0")
            continue

        total_time = process.end_time - process.start_time

        # If no children, self time is total time
        if not process.children:
            self_times[pid] = total_time
            if verbose:
                print(f"PID {pid}: No children, self_time = total_time = {total_time:.6f}")
            continue

        # Calculate earliest start and latest end times of children
        earliest_child_start = float('inf')
        latest_child_end = 0.0
        valid_children = []

        for child_pid in process.children:
            if child_pid in processes:
                child_process = processes[child_pid]

                if child_process.start_time is not None and child_process.end_time is not None:
                    valid_children.append(child_pid)

                    earliest_child_start = min(earliest_child_start, child_process.start_time)
                    latest_child_end = max(latest_child_end, child_process.end_time)

        if verbose:
            print(f"PID {pid}:")
            print(f"  Total time: {total_time:.6f} ({process.start_time} to {process.end_time})")
            print(f"  Children: {process.children}")
            print(f"  Valid children: {valid_children}")
            print(f"  Earliest child start: {earliest_child_start if earliest_child_start != float('inf') else 'None'}")
            print(f"  Latest child end: {latest_child_end if latest_child_end != 0.0 else 'None'}")

        # If we couldn't find valid child times, self time is total time
        if earliest_child_start == float('inf') or latest_child_end == 0.0:
            self_times[pid] = total_time
            if verbose:
                print(f"  No valid child times, self_time = total_time = {total_time:.6f}")
            continue

        # Calculate time spent in children (considering parallel execution)
        children_time = max(0.0, latest_child_end - earliest_child_start)

        # Ensure children_time doesn't exceed total_time
        children_time = min(children_time, total_time)

        # Calculate self time
        self_time = total_time - children_time
        self_times[pid] = max(0.0, self_time)  # Ensure self time is not negative

        if verbose:
            print(f"  Children time: {children_time:.6f}")
            print(f"  Self time: {self_times[pid]:.6f}")

    # Print summary of self times
    if verbose:
        print("\nSelf time summary:")
        for pid, self_time in sorted(self_times.items(), key=lambda x: x[1], reverse=True):
            print(f"PID {pid}: {self_time:.6f} seconds")

        max_self_time = max(self_times.values(), default=0.0)
        print(f"\nMaximum self time: {max_self_time:.6f} seconds")

    return self_times


def generate_dot(processes: Dict[int, Process], self_times: Dict[int, float], verbose: bool = False) -> str:
    dot_lines = ["digraph process_tree {",
                 "  rankdir=LR;",  # Left to right layout
                 "  node [shape=box, fontname=\"monospace\"];"]

    # Find max self time to create a color gradient
    max_self_time = max(self_times.values(), default=0.0)

    if verbose:
        print(f"\nGenerating DOT file with max_self_time = {max_self_time:.6f}")

    # Add nodes
    for pid, process in processes.items():
        # If the process has no commands and has a parent, use the parent's latest command
        if not process.commands and process.parent_pid is not None and process.parent_pid in processes:
            parent = processes[process.parent_pid]
            if parent.commands:
                # Use the most recent command from the parent
                process.commands = [f"{parent.commands[-1]} (forked)"]
            else:
                process.commands = ["unknown"]
        elif not process.commands:
            process.commands = ["unknown"]

        # Format the commands for better readability
        command_list = []
        for i, cmd in enumerate(process.commands):
            # Escape special characters for the DOT format
            escaped_cmd = cmd.replace('\\', '\\\\').replace('"', '\\"')
            if i == 0:
                command_list.append(f"Exec: {escaped_cmd}")
            else:
                command_list.append(f"Re-exec: {escaped_cmd}")

        label = f"PID: {pid}\\n" + "\\n".join(command_list)

        # Add self time to the label if in verbose mode
        if verbose:
            self_time = self_times.get(pid, 0.0)
            label += f"\\nSelf time: {self_time:.6f}s"

        # Calculate color based on self time
        self_time = self_times.get(pid, 0.0)
        if max_self_time > 0:
            # Normalize self time to a value between 0 and 1
            normalized_time = self_time / max_self_time
            # Create a color gradient from green (0.0) to red (1.0)
            # HTML color format: #RRGGBB
            green = int(255 * (1.0 - normalized_time))
            red = int(255 * normalized_time)
            color = f"\"#{red:02x}{green:02x}00\""

            if verbose:
                print(f"PID {pid}:")
                print(f"  Self time: {self_time:.6f}")
                print(f"  Normalized time: {normalized_time:.6f}")
                print(f"  Color: {color} (red={red}, green={green})")
        else:
            # Default to green if all self times are 0
            color = "\"#00ff00\""
            if verbose:
                print(f"PID {pid}: Using default green color")

        dot_lines.append(f'  "{pid}" [label="{label}", style=filled, fillcolor={color}];')

    # Add edges - add extra safety check for parent_pid
    for pid, process in processes.items():
        if hasattr(process, 'parent_pid') and process.parent_pid is not None and process.parent_pid in processes:
            dot_lines.append(f'  "{process.parent_pid}" -> "{pid}";')

    dot_lines.append("}")
    return "\n".join(dot_lines)


if __name__ == "__main__":
    main()