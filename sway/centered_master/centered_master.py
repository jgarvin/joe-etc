#!/usr/bin/env python3
from __future__ import annotations

import os
import sys
import json
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Sequence, Tuple

try:
    import i3ipc  # type: ignore
except Exception as e:
    print(f"[cm] import i3ipc failed: {e}", file=sys.stderr)
    sys.exit(1)

LEFT_MARK = "__cm_left__"
CENTER_MARK = "__cm_center__"
RIGHT_MARK = "__cm_right__"
TARGET_MARK = "__cm_target__"

@dataclass(frozen=True)
class Win:
    id: int
    node: "i3ipc.Con"

class Logger:
    def __init__(self) -> None:
        self.enabled: bool = True
        self.prefix: str = "[cm]"
    def log(self, msg: str) -> None:
        if self.enabled:
            ts = time.strftime("%H:%M:%S")
            print(f"{self.prefix} {ts} {msg}", file=sys.stderr, flush=True)
    def dump(self, label: str, obj: Any) -> None:
        try:
            s = json.dumps(obj, indent=2, default=str)
        except Exception:
            s = repr(obj)
            self.log(f"{label} = {s}")

LOG = Logger()

def _cmd(con: "i3ipc.Con | i3ipc.Connection", cmd: str) -> None:
    LOG.log(f"cmd: {cmd}")
    try:
        res = con.command(cmd)  # type: ignore[attr-defined]
    except Exception as e:
        LOG.log(f"cmd error: {e}")
        raise
    # i3ipc returns a list of dict-like replies
    ok = True
    try:
        for r in res:  # type: ignore[assignment]
            success = getattr(r, "success", None)
            if isinstance(r, dict):
                success = r.get("success", None)
            if success is False:
                ok = False
    except Exception:
        pass
    if not ok:
        LOG.dump("cmd reply", res)

def _is_tiled_leaf(n: "i3ipc.Con") -> bool:
    return bool(n.window) and n.floating in ("auto_off", "user_off", None) and n.type == "con"

def _focused_workspace(conn: "i3ipc.Connection") -> "i3ipc.Con":
    focused = conn.get_tree().find_focused()
    if focused is None:
        raise RuntimeError("No focused node")
    ws = focused.workspace()
    if ws is None:
        raise RuntimeError("Cannot resolve focused workspace")
    LOG.log(f"focused workspace: {ws.name}")
    return ws

def _layout_tag(n: "i3ipc.Con") -> str:
    t = "W" if n.type == "workspace" else ("C" if n.type == "con" else n.type or "?")
    lay = n.layout or "none"
    return f"{t}:{lay}:{','.join(n.marks or [])}"

def _tree_preview(root: "i3ipc.Con", depth: int = 0, max_depth: int = 4) -> List[str]:
    lines: List[str] = []
    pad = "  " * depth
    lines.append(f"{pad}{_layout_tag(root)} children={len(root.nodes)} leaves={len(root.leaves())}")
    if depth >= max_depth:
        return lines
    for c in root.nodes:
        lines.extend(_tree_preview(c, depth + 1, max_depth))
    return lines

def _normalize_workspace_singletons(ws: "i3ipc.Con") -> None:
    LOG.log("normalize: start")
    while True:
        top_children: List["i3ipc.Con"] = [n for n in ws.nodes if n.type == "con"]
        if len(top_children) != 1:
            break
        only = top_children[0]
        if _is_tiled_leaf(only):
            break
        inner_children: List["i3ipc.Con"] = [n for n in only.nodes]
        if len(inner_children) != 1:
            break
        inner = inner_children[0]
        LOG.log(f"normalize: unwrap {_layout_tag(only)} -> move up {_layout_tag(inner)}")
        inner.command("focus")
        _cmd(inner, "move container to workspace current")
        LOG.log("normalize: done")

def _lowest_horizontal_ancestor(n: "i3ipc.Con") -> "i3ipc.Con":
    cur: Optional["i3ipc.Con"] = n
    last_ws: Optional["i3ipc.Con"] = None
    while cur is not None:
        if cur.type == "workspace":
            last_ws = cur
        if cur.layout == "splith":
            LOG.log(f"target ancestor: {_layout_tag(cur)}")
            return cur
        cur = cur.parent
    if last_ws is None:
        raise RuntimeError("Could not find workspace")
    LOG.log("no horizontal ancestor; using workspace")
    return last_ws

def _leaves_under(root: "i3ipc.Con") -> List[Win]:
    leaves = [Win(n.id, n) for n in root.leaves() if _is_tiled_leaf(n)]
    LOG.log(f"leaves under target: {len(leaves)}")
    return leaves

def _clear_marks(conn: "i3ipc.Connection") -> None:
    for m in (LEFT_MARK, CENTER_MARK, RIGHT_MARK, TARGET_MARK):
        _cmd(conn, f"unmark {m}")

def _mark_target(target: "i3ipc.Con") -> None:
    target.command("focus")
    _cmd(target, f"mark --replace {TARGET_MARK}")

def _move_to_target(n: "i3ipc.Con") -> None:
    _cmd(n, f"move container to mark {TARGET_MARK}")

def _unwrap_previous_columns(target: "i3ipc.Con") -> None:
    LOG.log("unwrap prev columns: start")
    for child in list(target.nodes):
        marks = set(child.marks or [])
        if marks & {LEFT_MARK, CENTER_MARK, RIGHT_MARK}:
            LOG.log(f"unwrap: {_layout_tag(child)} with marks {marks}")
            for gc in list(child.nodes):
                gc.command("focus")
                _move_to_target(gc)
                LOG.log("unwrap prev columns: done")

def _wrap_as_vertical_column(seed: Optional["i3ipc.Con"], mark: str) -> Optional["i3ipc.Con"]:
    if seed is None:
        return None
    seed.command("focus")
    _move_to_target(seed)
    _cmd(seed, "split v")
    _cmd(seed, "focus parent")
    _cmd(seed.parent, "layout splitv")  # type: ignore[arg-type]
    _cmd(seed.parent, f"mark --replace {mark}")  # type: ignore[arg-type]
    parent = seed.parent
    if parent is None:
        raise RuntimeError("No parent after splitv")
    LOG.log(f"new column: {_layout_tag(parent)}")
    return parent

def _resize_width_pct(column_con: Optional["i3ipc.Con"], pct: int) -> None:
    if column_con is None:
        return
    column_con.command("focus")
    _cmd(column_con, f"resize set width {pct} ppt")

def _balance_vertical_children(column_con: Optional["i3ipc.Con"]) -> None:
    if column_con is None:
        return
    kids = [c for c in column_con.nodes if _is_tiled_leaf(c) or c.type == "con"]
    k = len(kids)
    LOG.log(f"balance stack: {k} children")
    if k == 0:
        return
    base = 100 // k
    rem = 100 - base * k
    for i, kid in enumerate(kids):
        kid.command("focus")
        _cmd(kid, f"resize set height {base + (1 if i < rem else 0)} ppt")

def _centered_master(conn: "i3ipc.Connection", master_pct: int) -> None:
    LOG.dump("env", {"CENTERED_MASTER_PCT": os.getenv("CENTERED_MASTER_PCT", "")})
    try:
        ver = conn.get_version()  # type: ignore[attr-defined]
        LOG.dump("ipc version", ver)
    except Exception as e:
        LOG.log(f"get_version failed: {e}")

    ws = _focused_workspace(conn)
    LOG.log("workspace tree (pre-normalize):")
    for line in _tree_preview(ws):
        LOG.log(line)

    _normalize_workspace_singletons(ws)

    LOG.log("workspace tree (post-normalize):")
    for line in _tree_preview(ws):
        LOG.log(line)

    focused = ws.find_focused()
    if focused is None:
        LOG.log("no focused node after normalize; abort")
        return

    target = _lowest_horizontal_ancestor(focused)
    _cmd(target, "layout splith")
    _clear_marks(conn)
    _mark_target(target)
    _unwrap_previous_columns(target)

    LOG.log("target subtree (pre):")
    for line in _tree_preview(target):
        LOG.log(line)

    leaves = _leaves_under(target)
    if len(leaves) <= 1:
        LOG.log("<=1 window under target; nothing to do")
        return

    master = next((w for w in leaves if w.node.focused), None)
    if master is None:
        LOG.log("could not locate focused leaf; abort")
        return

    others: List[Win] = [w for w in leaves if w.id != master.id]
    left_count = (len(others) + 1) // 2
    left_list = others[:left_count]
    right_list = others[left_count:]
    LOG.dump("partition", {"left": [hex(w.id) for w in left_list], "right": [hex(w.id) for w in right_list]})

    center_seed = master.node
    left_seed: Optional["i3ipc.Con"] = left_list[0].node if left_list else None
    right_seed: Optional["i3ipc.Con"] = right_list[0].node if right_list else None

    center_col = _wrap_as_vertical_column(center_seed, CENTER_MARK)
    left_col = _wrap_as_vertical_column(left_seed, LEFT_MARK)
    right_col = _wrap_as_vertical_column(right_seed, RIGHT_MARK)

    side_pct = max(0, (100 - master_pct) // 2)
    _resize_width_pct(left_col, side_pct)
    _resize_width_pct(right_col, side_pct)
    _resize_width_pct(center_col, 100 - 2 * side_pct)

    for w in left_list[1:]:
        _cmd(w.node, f"move container to mark {LEFT_MARK}")
    for w in right_list[1:]:
        _cmd(w.node, f"move container to mark {RIGHT_MARK}")

    _balance_vertical_children(left_col)
    _balance_vertical_children(right_col)

    center_seed.command("focus")

    LOG.log("target subtree (post):")
    for line in _tree_preview(target):
        LOG.log(line)

def main() -> None:
    master_env = os.getenv("CENTERED_MASTER_PCT", "").strip()
    master_pct = 60 if not master_env.isdigit() else max(30, min(80, int(master_env)))
    LOG.log(f"master_pct={master_pct}")
    try:
        conn = i3ipc.Connection()
    except Exception as e:
        LOG.log(f"i3ipc.Connection() failed: {e}")
        sock_env = os.getenv("SWAYSOCK") or os.getenv("I3SOCK") or ""
        LOG.log(f"SWAYSOCK/I3SOCK={sock_env!r}")
        raise
    try:
        _centered_master(conn, master_pct)
    finally:
        _clear_marks(conn)
        LOG.log("done")

if __name__ == "__main__":
    main()
