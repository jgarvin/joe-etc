#!/usr/bin/env bash
set -euo pipefail

tree="$(swaymsg -t get_tree)"
[ -n "$tree" ] || exit 2

focused_id="$(jq -r '..|objects|select(.focused==true)|.id' <<<"$tree" | head -n1 || true)"
[ -n "$focused_id" ] || exit 1

ws_id="$(jq -r --argjson fid "$focused_id" '
  ..|objects
  | select(.type=="workspace" and (..|objects|.id?==$fid))
  | .id
' <<<"$tree" | head -n1 || true)"
[ -n "$ws_id" ] || exit 0

cmds=()
while IFS= read -r grp; do
    mapfile -t lines < <(jq -r '.children[] | "\(.id) \(.w)"' <<<"$grp")
    n=${#lines[@]}
    (( n >= 2 )) || continue

    ids=()
    weights=()
    total=0
    for line in "${lines[@]}"; do
        id=${line%% *}
        w=${line#* }
        ids+=("$id")
        weights+=("$w")
        (( total += w ))
    done
    (( total > 0 )) || continue

    sum=0
    for ((i=0;i<n;i++)); do
        if (( i == n-1 )); then
            add=$((100 - sum))
        else
            add=$(( 100 * weights[i] / total ))
        fi
        (( sum += add ))
        cmds+=("[con_id=${ids[i]}] resize set width ${add}ppt")
    done
done < <(jq -c --argjson ws "$ws_id" '
  def leaf_count:
    if (.nodes|length)==0 then 1
    else (.nodes | map(leaf_count) | add)
    end;

  ..|objects
  | select(.id==$ws)
  | ..|objects
  | select(.layout=="splith" and (.nodes|length)>=2)
  | {children: [ .nodes | sort_by(.rect.x) | .[] | {id: .id, w: (leaf_count)} ]}
' <<<"$tree")

if ((${#cmds[@]})); then
    IFS=';'; swaymsg -- "${cmds[*]}" >/dev/null; IFS=$' \t\n'
fi
