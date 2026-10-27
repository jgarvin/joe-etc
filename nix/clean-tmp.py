"""Deletes stale entries under a temporary directory, a whole unit at a time.

A unit is an entry directly in ROOT, or directly in a container. A unit is
deleted as a whole once nothing in it, the unit itself included, was modified
in the last MINUTES, and is otherwise kept as a whole: the old parts of a tree
still in use are often still needed (a git repository's packfiles, build
caches reusing old outputs). A container holds independent units, such as one
directory per session, so it is walked into rather than judged as one, and is
never deleted itself.

Sockets, FIFOs, mount points and anything on another file system always count
as fresh: their mtimes do not show whether something still uses them, and
mount points are never descended into. systemd's PrivateTmp directories
(systemd-private-* directly in ROOT) are skipped: a running service's private
/tmp is bind-mounted from one, so deleting it breaks that service.
"""

import argparse
import os
import re
import shutil
import stat
import time
from fnmatch import fnmatchcase


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--root", required=True)
    parser.add_argument("--max-age-minutes", type=int, required=True, metavar="MINUTES")
    parser.add_argument(
        "--container",
        action="append",
        default=[],
        metavar="GLOB",
        help="a directory whose entries are units, as a path relative to ROOT "
        "with a glob per component (e.g. 'a-*/*'); the directories leading to it "
        "are containers too. Repeatable.",
    )
    args = parser.parse_args()
    root = os.path.abspath(args.root)
    cutoff = time.time() - args.max_age_minutes * 60
    containers = {
        tuple(components[:depth])
        for components in (glob.split("/") for glob in args.container)
        for depth in range(1, len(components) + 1)
    }

    # Mount points come from mountinfo (field 5, with octal escapes for
    # space, tab, newline and backslash) instead of from stat: stat on a
    # mount point asks the mounted file system, which can hang (FUSE).
    mount_points: set[str] = set()
    with open("/proc/self/mountinfo", "rb") as mountinfo:
        for line in mountinfo:
            point = re.sub(
                rb"\\([0-7]{3})",
                lambda m: bytes([int(m.group(1), 8)]),
                line.split(b" ")[4],
            )
            mount_points.add(os.fsdecode(point))

    root_dev = os.lstat(root).st_dev

    def fresh(st: os.stat_result) -> bool:
        return (
            st.st_mtime > cutoff
            or stat.S_ISSOCK(st.st_mode)
            or stat.S_ISFIFO(st.st_mode)
            or st.st_dev != root_dev
        )

    def in_use(path: str, st: os.stat_result) -> bool:
        # Stops at the first fresh entry, so only stale trees are walked whole.
        if fresh(st):
            return True
        directories = [path] if stat.S_ISDIR(st.st_mode) else []
        while directories:
            try:
                with os.scandir(directories.pop()) as entries:
                    children = list(entries)
            except FileNotFoundError:
                continue
            for entry in children:
                if entry.path in mount_points:
                    return True
                try:
                    child = entry.stat(follow_symlinks=False)
                except FileNotFoundError:
                    continue
                if fresh(child):
                    return True
                if stat.S_ISDIR(child.st_mode):
                    directories.append(entry.path)
        return False

    def ignore_vanished(function: object, path: str, error: BaseException) -> None:
        # Another process may delete entries concurrently; anything else is
        # a real failure.
        if not isinstance(error, FileNotFoundError):
            raise error

    # Directories whose entries are units, with their paths relative to root.
    pending: list[tuple[str, tuple[str, ...]]] = [(root, ())]
    while pending:
        directory, relative = pending.pop()
        try:
            with os.scandir(directory) as entries:
                children = list(entries)
        except FileNotFoundError:
            continue
        for entry in children:
            if entry.path in mount_points:
                continue
            if not relative and fnmatchcase(entry.name, "systemd-private-*"):
                continue
            try:
                st = entry.stat(follow_symlinks=False)
            except FileNotFoundError:
                continue
            entry_relative = (*relative, entry.name)
            if (
                stat.S_ISDIR(st.st_mode)
                and st.st_dev == root_dev
                and any(
                    len(glob) == len(entry_relative)
                    and all(map(fnmatchcase, entry_relative, glob))
                    for glob in containers
                )
            ):
                pending.append((entry.path, entry_relative))
            elif not in_use(entry.path, st):
                if stat.S_ISDIR(st.st_mode):
                    shutil.rmtree(entry.path, onexc=ignore_vanished)
                else:
                    try:
                        os.unlink(entry.path)
                    except FileNotFoundError:
                        pass


if __name__ == "__main__":
    main()
