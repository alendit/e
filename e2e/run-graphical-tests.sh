#!/usr/bin/env bash
set -euo pipefail

e2e_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$e2e_dir/.." && pwd)
test_file=${E_GRAPHICAL_E2E_TEST_FILE:-$e2e_dir/graphical/e-chat-behavior-test.el}
runner_file=$e2e_dir/graphical/e-graphical-test-runner.el

cd "$project_dir"

convert_graphical_screenshots() {
  local directory=${E_GRAPHICAL_E2E_SCREENSHOT_DIR:-}
  [[ -n $directory ]] || return 0
  command -v rsvg-convert >/dev/null 2>&1 || return 0

  local svg png
  shopt -s nullglob
  for svg in "$directory"/*.svg; do
    png=${svg%.svg}.png
    rsvg-convert --output "$png" "$svg"
  done
  shopt -u nullglob
}

emacs_command=(
  eldev emacs
  --load "$test_file"
  --load "$runner_file"
)

system_name=$(uname -s)

if [[ $system_name == Darwin && ${E_GRAPHICAL_E2E_NATIVE_VISIBLE:-} != 1 ]]; then
  server_name=e-graphical-e2e-$$
  report_file=$(mktemp -t e-graphical-e2e-report.XXXXXX)
  emacs_dir=$(mktemp -d -t e-graphical-e2e-emacs.XXXXXX)
  bootstrap_file=$e2e_dir/graphical/e-graphical-daemon-bootstrap.el
  cleanup() {
    emacsclient --socket-name "$server_name" \
      --eval "(kill-emacs 0)" >/dev/null 2>&1 || true
    rm -f "$report_file"
    rm -rf "$emacs_dir"
  }
  trap cleanup EXIT HUP INT TERM

  eldev prepare emacs
  E_GRAPHICAL_E2E_EMACS_DIR="$emacs_dir" \
    emacs --quick --daemon="$server_name" --load "$bootstrap_file"
  result=$(emacsclient --socket-name "$server_name" --eval "
    (let ((frame
           (make-frame
            '((name . \"e graphical E2E\")
              (window-system . ns)
              (width . 140)
              (height . 48)
              (left . -10000)
              (top . -10000)
              (alpha . (0 . 0))
              (no-accept-focus . t)
              (no-focus-on-map . t)
              (skip-taskbar . t)
              (undecorated . t)))))
      (with-selected-frame frame
        (load \"$test_file\" nil nil t)
        (load \"$runner_file\" nil nil t)
        (e-graphical-test-runner-run-to-file \"$report_file\")))")
  cat "$report_file"
  convert_graphical_screenshots
  [[ $result == 0 ]]
  exit
fi

if [[ -z ${DISPLAY:-} && $system_name == Linux ]]; then
  if ! command -v xvfb-run >/dev/null 2>&1; then
    echo "Graphical E2E requires xvfb-run when DISPLAY is unset." >&2
    exit 2
  fi
  if xvfb-run -a -s "-screen 0 1440x1000x24" "${emacs_command[@]}"; then
    test_status=0
  else
    test_status=$?
  fi
  convert_graphical_screenshots
  exit "$test_status"
fi

export E_GRAPHICAL_E2E_AUTORUN=1
if "${emacs_command[@]}"; then
  test_status=0
else
  test_status=$?
fi
convert_graphical_screenshots
exit "$test_status"
