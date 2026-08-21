#!/usr/bin/env bash
set -euo pipefail

e2e_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$e2e_dir/.." && pwd)
bootstrap_file=$e2e_dir/e-e2e-bootstrap.el
test_file=$e2e_dir/e-current-config-e2e-test.el
server_name=e-current-config-e2e-$$
report_file=$(mktemp -t e-current-config-e2e-report.XXXXXX)

cd "$project_dir"

if [[ ${E_E2E_EMACS_CONFIG:-current} != current ]]; then
  echo "Current-config E2E requires E_E2E_EMACS_CONFIG=current." >&2
  exit 2
fi
export E_E2E_EMACS_CONFIG=current

emacs_command=(emacs)
if [[ -n ${E_E2E_EMACS_INIT_DIRECTORY:-} ]]; then
  emacs_command+=(--init-directory "$E_E2E_EMACS_INIT_DIRECTORY")
fi

cleanup() {
  emacsclient --socket-name "$server_name" \
    --eval "(kill-emacs 0)" >/dev/null 2>&1 || true
  rm -f "$report_file"
}
trap cleanup EXIT HUP INT TERM

eldev prepare emacs
"${emacs_command[@]}" --daemon="$server_name" --load "$bootstrap_file"
result=$(emacsclient --socket-name "$server_name" --eval "
  (progn
    (load \"$test_file\" nil nil t)
    (e-current-config-e2e-test-run-to-file \"$report_file\"))")
cat "$report_file"
[[ $result == 0 ]]
