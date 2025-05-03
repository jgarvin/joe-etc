#!/usr/bin/env python3
import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, List, Optional, Set


def main() -> None:
    parser = argparse.ArgumentParser(description="Visualize process tree from a command using strace")
    parser.add_argument("command", nargs="+", help="The command to run and trace")
    parser.add_argument("-o", "--output", default="process_tree.svg",
                        help="Output SVG file (default: process_tree.svg)")
    args = parser.parse_args()

    # Create a temporary directory for strace output
    with tempfile.TemporaryDirectory(delete=False) as tmpdir:
        print(f"Using {tmpdir=}", file=sys.stderr)

        # Run the command with strace, focusing on process-related system calls
        strace_cmd = ["strace",
                      "-ff",                    # Follow forks with output into separate files
                      "-e", "trace=process",    # Only trace process-related system calls
                      "-s", "100",             # Capture up to 2048 chars of strings for args
                      "-o", f"{tmpdir}/strace.log",
                      *args.command]

        print(f"Running: {' '.join(strace_cmd)}")
        try:
            subprocess.run(strace_cmd)
            print("Command completed, processing logs...")
        except KeyboardInterrupt:
            print("\nCommand interrupted, processing logs...")

        # Process the strace logs and generate the process tree
        process_tree = parse_strace_logs(tmpdir)

        # Generate the DOT file
        dot_file = generate_dot(process_tree)

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


def parse_strace_logs(log_dir: str) -> Dict[int, Process]:
    processes: Dict[int, Process] = {}

    # First, find all the log files
    log_files = list(Path(log_dir).glob("strace.log.*"))

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

        with open(log_file, "r") as f:
            for line in f:
                # Check for clone/fork/vfork calls to find child processes
                if any(call in line for call in ["clone(", "fork(", "vfork("]):
                    match = re.search(r'= (\d+)$', line.strip())
                    if match and match.group(1) != "0":  # Not returning 0 means this is the parent
                        try:
                            child_pid = int(match.group(1))
                            if child_pid not in processes:
                                # Create a process for this child if it doesn't exist
                                processes[child_pid] = Process(child_pid, parent_pid=pid)
                            else:
                                # Update the parent if it wasn't set
                                if processes[child_pid].parent_pid is None:
                                    processes[child_pid].parent_pid = pid

                            processes[pid].children.add(child_pid)
                        except ValueError:
                            continue

                # Check for successful execve calls
                elif "execve(" in line and ") = 0" in line:
                    cmd = extract_command_args(line)
                    processes[pid].add_exec(cmd)

    # Ensure all processes have parent_pid attribute
    for pid, process in processes.items():
        if not hasattr(process, 'parent_pid'):
            process.parent_pid = None

    return processes


def generate_dot(processes: Dict[int, Process]) -> str:
    dot_lines = ["digraph process_tree {",
                 "  rankdir=LR;",  # Left to right layout
                 "  node [shape=box, fontname=\"monospace\"];"]

    # Add nodes
    for pid, process in processes.items():
        if not process.commands:
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
        dot_lines.append(f'  "{pid}" [label="{label}"];')

    # Add edges - add extra safety check for parent_pid
    for pid, process in processes.items():
        if hasattr(process, 'parent_pid') and process.parent_pid is not None and process.parent_pid in processes:
            dot_lines.append(f'  "{process.parent_pid}" -> "{pid}";')

    dot_lines.append("}")
    return "\n".join(dot_lines)


if __name__ == "__main__":
    main()