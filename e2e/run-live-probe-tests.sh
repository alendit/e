#!/usr/bin/env bash
set -euo pipefail

probe_e2e_dir=$(cd "$(dirname "$0")" && pwd)
probe_project_dir=$(cd "$probe_e2e_dir/.." && pwd)
probe_server_name="e-live-probe-e2e-$$"

probe_cleanup() {
  emacsclient --socket-name "$probe_server_name" --timeout=2 \
    --eval "(kill-emacs 0)" >/dev/null 2>&1 || true
}
trap probe_cleanup EXIT HUP INT TERM

emacs --quick --daemon="$probe_server_name" \
  --load "$probe_project_dir/e.el" >/dev/null

probe_run() {
  E_LIVE_PROBE_SOCKET_NAME="$probe_server_name" \
    "$probe_project_dir/scripts/e-live-probe" "$@"
}

probe_ping=$(probe_run ping)
[[ $probe_ping == *":ok t"* ]]
[[ $probe_ping == *":operation ping"* ]]

probe_selected=$(probe_run selected)
[[ $probe_selected == *":operation selected"* ]]
[[ $probe_selected == *":buffer \"*scratch*\""* ]]

probe_symbol=$(probe_run symbol e-dev-live-probe)
[[ $probe_symbol == *":function-bound t"* ]]

emacsclient --socket-name "$probe_server_name" --timeout=2 --eval \
  "(progn (fset 'e-dev-live-probe--dispatch (lambda (&rest _) (let ((cycle (vector nil))) (aset cycle 0 cycle) (signal 'wrong-type-argument (list 'e-harness cycle))))) :fault-installed)" \
  >/dev/null

probe_failure=$(probe_run ping)
[[ $probe_failure == *":condition wrong-type-argument"* ]]
[[ $probe_failure == *"#<cycle>"* ]]
[[ ${#probe_failure} -lt 512 ]]

echo "Live probe E2E passed"
