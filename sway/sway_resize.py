#!/usr/bin/env python3
import json
import math
import subprocess
import sys
from typing import Any, Dict, List, Optional, Tuple

Node = Dict[str, Any]

def run(cmd: list[str]) -> Tuple[int, str, str]:
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return p.returncode, p.stdout, p.stderr

def get_tree() -> Node:
    code, out, err = run(["swaymsg", "-rt", "get_tree"])
    if code != 0:
        raise RuntimeError(f"swaymsg get_tree failed: {err.strip()}")
    return json.loads(out)

def find_focused(node: Node, parents: Optional[List[Node]] = None) -> Tuple[Optional[Node], List[Node]]:
    if parents is None:
        parents = []
    if node.get("focused", False):
        return node, parents
    for child in node.get("nodes", []) + node.get("floating_nodes", []):
        found, chain = find_focused(child, parents + [node])
        if found is not None:
            return found, chain
    return None, []

def find_ancestor_of_type(parents: List[Node], type_name: str) -> Optional[Node]:
    for n in reversed(parents):
        if n.get("type") == type_name:
            return n
    return None

def is_floating(n: Node) -> bool:
    # sway sets "floating": "user_on"/"auto_on" when floating
    f = n.get("floating")
    return f in ("user_on", "auto_on")

def scale_dimension(cur_px: int, max_px: int, factor: float) -> Optional[int]:
    # If already full in this dimension, skip scaling for this axis
    if abs(cur_px - max_px) <= 1:
        return None
    target = int(round(min(max_px, max(1, cur_px * factor))))
    if target == cur_px:
        return None
    return target

def resize_con(con_id: int, width_px: Optional[int], height_px: Optional[int], ws_rect: Dict[str, int], floating: bool) -> None:
    cmds: list[list[str]] = []
    if floating:
        if width_px is not None:
            cmds.append(["swaymsg", "-q", f"[con_id={con_id}]", "resize", "set", "width", str(width_px), "px"])
        if height_px is not None:
            cmds.append(["swaymsg", "-q", f"[con_id={con_id}]", "resize", "set", "height", str(height_px), "px"])
    else:
        # use percentage points relative to workspace size to get multiplicative effect
        if width_px is not None:
            ppt_w = max(1, min(100, int(round(width_px / ws_rect["width"] * 100.0))))
            cmds.append(["swaymsg", "-q", f"[con_id={con_id}]", "resize", "set", "width", str(ppt_w), "ppt"])
        if height_px is not None:
            ppt_h = max(1, min(100, int(round(height_px / ws_rect["height"] * 100.0))))
            cmds.append(["swaymsg", "-q", f"[con_id={con_id}]", "resize", "set", "height", str(ppt_h), "ppt"])
    for c in cmds:
        run(c)

def main() -> int:
    if len(sys.argv) != 2 or sys.argv[1] not in ("double", "halve"):
        print("usage: resize_scale.py [double|halve]", file=sys.stderr)
        return 2
    factor = 2.0 if sys.argv[1] == "double" else 0.5

    tree = get_tree()
    focused, parents = find_focused(tree)
    if focused is None:
        return 0

    ws = find_ancestor_of_type(parents, "workspace")
    if ws is None:
        return 0

    con_rect = focused.get("rect") or focused.get("window_rect") or {}
    ws_rect = ws.get("rect") or {}
    if not con_rect or not ws_rect:
        return 0

    cur_w = int(con_rect["width"])
    cur_h = int(con_rect["height"])
    max_w = int(ws_rect["width"])
    max_h = int(ws_rect["height"])

    floating = is_floating(focused)

    tgt_w = scale_dimension(cur_w, max_w, factor)
    tgt_h = scale_dimension(cur_h, max_h, factor)

    # If one axis is already full, still resize along the other
    if tgt_w is None and tgt_h is None:
        # both axes already full; nothing to do
        return 0

    resize_con(int(focused["id"]), tgt_w, tgt_h, ws_rect, floating)
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
