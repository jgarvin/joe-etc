#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import sys
from typing import Any, Dict, List, Optional, Tuple, TypedDict, cast

Node = Dict[str, Any]


class Rect(TypedDict):
    x: int
    y: int
    width: int
    height: int


def _dbg(msg: str) -> None:
    sys.stderr.write(f"[DBG] {msg}\n")


def run(cmd: List[str]) -> Tuple[int, str, str]:
    _dbg(f"exec: {' '.join(cmd)}")
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if p.returncode != 0 or p.stderr.strip():
        _dbg(f"rc={p.returncode} stderr={p.stderr.strip()}")
    else:
        _dbg(f"rc={p.returncode}")
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
    f = n.get("floating")
    return f in ("user_on", "auto_on")


def to_rect(raw: Dict[str, Any]) -> Rect:
    return Rect(
        x=int(raw["x"]),
        y=int(raw["y"]),
        width=int(raw["width"]),
        height=int(raw["height"]),
    )


def pct_from_px(px: int, base: int) -> int:
    return max(1, min(100, int(round(px / float(base) * 100.0))))


def scale_dimension(cur_px: int, max_px: int, factor: float) -> Optional[int]:
    if abs(cur_px - max_px) <= 1:
        _dbg(f"axis full: cur={cur_px} max={max_px}")
        return None
    target = int(round(min(max_px, max(1, cur_px * factor))))
    if target == cur_px:
        _dbg(f"axis unchanged after scale: cur={cur_px} factor={factor} -> {target}")
        return None
    return target


def _edges(parent: Rect, cur: Rect, eps: int = 2) -> Tuple[bool, bool, bool, bool]:
    at_left = abs(cur["x"] - parent["x"]) <= eps
    at_right = abs((cur["x"] + cur["width"]) - (parent["x"] + parent["width"])) <= eps
    at_top = abs(cur["y"] - parent["y"]) <= eps
    at_bottom = abs((cur["y"] + cur["height"]) - (parent["y"] + parent["height"])) <= eps
    return at_left, at_right, at_top, at_bottom


def read_rects_again(con_id: int) -> Tuple[Rect, Rect]:
    tree = get_tree()

    # Find target by id (not relying on focus)
    q: List[Node] = [tree]
    target: Optional[Node] = None
    while q:
        n = q.pop(0)
        if int(n.get("id", -1)) == con_id:
            target = n
            break
        q.extend(n.get("nodes", []))
        q.extend(n.get("floating_nodes", []))
    if target is None:
        raise RuntimeError("lost target container")

    # Find its parent
    parent: Optional[Node] = target.get("parent") if isinstance(target.get("parent"), dict) else None
    while isinstance(parent, dict) and parent.get("type") in ("floating_con", "dockarea"):
        parent = parent.get("parent")  # skip irrelevant wrappers

    # Fall back to workspace
    parent_rect_raw: Optional[Dict[str, Any]] = None
    p = parent
    while isinstance(p, dict) and parent_rect_raw is None:
        maybe = p.get("rect")
        if isinstance(maybe, dict):
            parent_rect_raw = maybe
            break
        p = p.get("parent")
    if parent_rect_raw is None:
        # try workspace
        up = target.get("parent")
        while isinstance(up, dict):
            if up.get("type") == "workspace":
                parent_rect_raw = cast(Dict[str, Any], up["rect"])
                break
            up = up.get("parent")
    if parent_rect_raw is None:
        raise RuntimeError("no parent/workspace rect")

    con_rect_raw = cast(Dict[str, Any], target.get("rect") or target.get("window_rect"))
    return to_rect(con_rect_raw), to_rect(parent_rect_raw)


def apply_resize_set(con_id: int, width_ppt: Optional[int], height_ppt: Optional[int]) -> None:
    if width_ppt is not None and height_ppt is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "width", str(width_ppt), "ppt", "height", str(height_ppt), "ppt"])
    elif width_ppt is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "width", str(width_ppt), "ppt"])
    elif height_ppt is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "height", str(height_ppt), "ppt"])


def apply_resize_delta_dir(grow: bool, axis: str, direction: str, amount_px: int) -> None:
    verb = "grow" if grow else "shrink"
    # Sway requires a cardinal direction variant for edge-aware behavior
    # width -> left/right; height -> up/down
    run(["swaymsg", "resize", verb, direction, str(max(1, amount_px)), "px"])


def resize_con_tiled(con_id: int, tgt_w_px: Optional[int], tgt_h_px: Optional[int], parent_rect: Rect, cur_rect: Rect) -> None:
    cur_w_ppt = pct_from_px(cur_rect["width"], parent_rect["width"])
    cur_h_ppt = pct_from_px(cur_rect["height"], parent_rect["height"])
    tgt_w_ppt: Optional[int] = pct_from_px(tgt_w_px, parent_rect["width"]) if tgt_w_px is not None else None
    tgt_h_ppt: Optional[int] = pct_from_px(tgt_h_px, parent_rect["height"]) if tgt_h_px is not None else None

    _dbg(f"tiled: cur_ppt=({cur_w_ppt},{cur_h_ppt}) tgt_ppt=({tgt_w_ppt},{tgt_h_ppt}) parent=({parent_rect['width']}x{parent_rect['height']})")

    apply_resize_set(con_id, tgt_w_ppt, tgt_h_ppt)

    new_con, new_parent = read_rects_again(con_id)
    new_w_ppt = pct_from_px(new_con["width"], new_parent["width"])
    new_h_ppt = pct_from_px(new_con["height"], new_parent["height"])
    _dbg(f"after set: new_ppt=({new_w_ppt},{new_h_ppt})")

    changed = False
    if tgt_w_ppt is not None and new_w_ppt != cur_w_ppt:
        changed = True
    if tgt_h_ppt is not None and new_h_ppt != cur_h_ppt:
        changed = True
    if changed:
        return

    # Edge-aware, directional fallback using pixel deltas
    at_left, at_right, at_top, at_bottom = _edges(new_parent, new_con)
    _dbg(f"edges: left={at_left} right={at_right} top={at_top} bottom={at_bottom}")

    # Width
    if tgt_w_px is not None and new_con["width"] != tgt_w_px:
        grow = tgt_w_px > new_con["width"]
        if at_right:
            dir_w = "left"
        elif at_left:
            dir_w = "right"
        else:
            dir_w = "right"
            delta_px = abs(tgt_w_px - new_con["width"])
            _dbg(f"fallback width: {('grow' if grow else 'shrink')} {dir_w} by {delta_px}px (cur={new_con['width']} tgt={tgt_w_px})")
            apply_resize_delta_dir(grow, "width", dir_w, delta_px)

        # verify
        new_con2, new_parent2 = read_rects_again(con_id)
        _dbg(f"after dir width: cur_px={new_con2['width']} parent_w={new_parent2['width']}")

    # Height (keep for completeness; typically full in splith)
    if tgt_h_px is not None and new_con["height"] != tgt_h_px:
        grow = tgt_h_px > new_con["height"]
        if at_bottom:
            dir_h = "up"
        elif at_top:
            dir_h = "down"
        else:
            dir_h = "down"
            delta_px = abs(tgt_h_px - new_con["height"])
            _dbg(f"fallback height: {('grow' if grow else 'shrink')} {dir_h} by {delta_px}px (cur={new_con['height']} tgt={tgt_h_px})")
            apply_resize_delta_dir(grow, "height", dir_h, delta_px)


def resize_con_floating(con_id: int, width_px: Optional[int], height_px: Optional[int]) -> None:
    if width_px is not None and height_px is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "width", str(width_px), "px", "height", str(height_px), "px"])
    elif width_px is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "width", str(width_px), "px"])
    elif height_px is not None:
        run(["swaymsg", f"[con_id={con_id}]", "resize", "set", "height", str(height_px), "px"])


def _parent_layout_info(parent: Optional[Node]) -> Tuple[str, int]:
    if not isinstance(parent, dict):
        return ("<none>", 0)
    layout = str(parent.get("layout", "<unknown>"))
    v = parent.get("nodes")
    siblings = len(v) if isinstance(v, list) else 0
    return (layout, siblings)


def main() -> int:
    if len(sys.argv) != 2 or sys.argv[1] not in ("double", "halve"):
        print("usage: sway_resize.py [double|halve]", file=sys.stderr)
        return 2
    factor = 2.0 if sys.argv[1] == "double" else 0.5
    _dbg(f"factor={factor}")

    try:
        tree = get_tree()
    except Exception as e:
        _dbg(f"get_tree failed: {e!r}")
        return 1

    focused, parents = find_focused(tree)
    if focused is None:
        _dbg("no focused node")
        return 0

    ws = find_ancestor_of_type(parents, "workspace")
    if ws is None:
        _dbg("no workspace ancestor")
        return 0

    con_rect_raw = cast(Optional[Dict[str, Any]], focused.get("rect") or focused.get("window_rect"))
    ws_rect_raw = cast(Optional[Dict[str, Any]], ws.get("rect"))
    if not isinstance(con_rect_raw, dict) or not isinstance(ws_rect_raw, dict):
        _dbg("missing rects")
        return 0

    con_rect = to_rect(con_rect_raw)
    ws_rect = to_rect(ws_rect_raw)

    con_id = int(focused["id"])
    floating = is_floating(focused)
    name = str(focused.get("name", "<unnamed>"))
    _dbg(f"focused id={con_id} name={name} floating={floating} con_rect={con_rect} ws_rect={ws_rect}")

    if floating:
        max_w = ws_rect["width"]
        max_h = ws_rect["height"]
        tgt_w = scale_dimension(con_rect["width"], max_w, factor)
        tgt_h = scale_dimension(con_rect["height"], max_h, factor)
        _dbg(f"float targets: tgt_w={tgt_w} tgt_h={tgt_h} (max_w={max_w} max_h={max_h})")
        if tgt_w is None and tgt_h is None:
            _dbg("nothing to do (float)")
            return 0
        resize_con_floating(con_id, tgt_w, tgt_h)
        return 0

    parent = parents[-1] if parents else None
    layout, siblings = _parent_layout_info(parent)
    _dbg(f"parent layout={layout} siblings={siblings}")

    parent_rect_raw: Optional[Dict[str, Any]] = None
    if isinstance(parent, dict):
        r = parent.get("rect")
        if isinstance(r, dict):
            parent_rect_raw = r
    if parent_rect_raw is None:
        parent_rect_raw = ws_rect_raw
        _dbg("parent rect missing; using workspace rect")

    parent_rect = to_rect(parent_rect_raw)
    _dbg(f"parent_rect={parent_rect}")

    max_w = parent_rect["width"]
    max_h = parent_rect["height"]
    tgt_w = scale_dimension(con_rect["width"], max_w, factor)
    tgt_h = scale_dimension(con_rect["height"], max_h, factor)
    _dbg(f"tiled targets: tgt_w={tgt_w} tgt_h={tgt_h} (max_w={max_w} max_h={max_h})")

    if tgt_w is None and tgt_h is None:
        _dbg("nothing to do (tiled)")
        return 0

    resize_con_tiled(con_id, tgt_w, tgt_h, parent_rect, con_rect)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
