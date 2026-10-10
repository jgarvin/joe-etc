#!/usr/bin/env bash
set -euo pipefail

if [[ $# -eq 0 ]]; then
    echo "usage: $0 COMMAND [ARG ...]" >&2
    exit 2
fi

# An agent launched by this script passes the marker to its children. Keep
# nested invocations in the existing constrained process tree rather than
# creating sibling scopes with independent per-scope allowances.
if [[ ${AGENT_RUN_RESOURCE_LIMITED:-0} == 1 ]]; then
    exec "$@"
fi

readonly slice=agents.slice

if ! systemctl --user cat "$slice" >/dev/null; then
    echo "error: $slice has no unit file; apply the Home Manager configuration first" >&2
    exit 1
fi
systemctl --user start "$slice"

export AGENT_RUN_RESOURCE_LIMITED=1
export NIX_CONFIG="${NIX_CONFIG:+${NIX_CONFIG}
}max-jobs = 4
cores = 1"

printf 'agent limits: slice=%s nix_jobs=4 nix_cores_per_job=1; command:' "$slice" >&2
printf ' %q' "$@" >&2
printf '\n' >&2

exec systemd-run \
    --user \
    --scope \
    --quiet \
    --collect \
    --slice="$slice" \
    --property=OOMPolicy=kill \
    -- \
    nice --adjustment=10 ionice --class idle "$@"
