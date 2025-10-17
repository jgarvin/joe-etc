#!/usr/bin/env bash
set -euo pipefail

# the body of the loop was vibe coded by gpt, but it only makes the
# windows equal size on repeated runs, so lets just run it a bunch of
# times, blech
for repeat in $(seq 1 6); do
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
        mapfile -t kids < <(jq -r '.children[]' <<<"$grp")
        n="${#kids[@]}"
        (( n >= 2 )) || continue

        base=$((100 / n))
        rem=$((100 - base*n))

        sum=0
        for ((i=0;i<n;i++)); do
            add=$base
            if (( i < rem )); then ((add++)); fi
            if (( i == n-1 )); then
                add=$((100 - sum))   # exact remainder for the last child
            fi
            sum=$((sum + add))
            cmds+=("[con_id=${kids[i]}] resize set width ${add}ppt")
        done
    done < <(jq -c --argjson ws "$ws_id" '
      ..|objects
      | select(.id==$ws)
      | ..|objects
      | select(.layout=="splith" and (.nodes|length)>=2)
      | {children: [.nodes|sort_by(.rect.x)|.[]|.id]}
    ' <<<"$tree")

    if ((${#cmds[@]})); then
        IFS=';'; swaymsg -- "${cmds[*]}" >/dev/null; IFS=$' \t\n'
    fi
done