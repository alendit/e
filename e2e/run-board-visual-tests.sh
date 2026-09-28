#!/usr/bin/env bash
set -euo pipefail

e2e_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$e2e_dir/.." && pwd)
test_file=$e2e_dir/graphical/e-board-visual-e2e.el
server_name=e-board-visual-e2e-$$
state_dir=$(mktemp -d -t e-board-visual-e2e-state.XXXXXX)

export E_E2E_EMACS_CONFIG=current
export E_RUNTIME_STATE_DIRECTORY=$state_dir
export E_CURRENT_CONFIG_E2E_STATE_DIR=$state_dir

cleanup() {
  emacsclient --no-wait --socket-name "$server_name" \
    --eval '(kill-emacs 0)' >/dev/null 2>&1 || true
  rm -rf "$state_dir"
}
trap cleanup EXIT HUP INT TERM

eval_in_test() {
  emacsclient --socket-name "$server_name" --eval "$1"
}

expect_true() {
  local result
  result=$(eval_in_test "$1")
  if [[ $result != t ]]; then
    echo "Board visual E2E phase failed: $1 => $result" >&2
    exit 1
  fi
}

wait_for() {
  local probe=$1
  local predicate=$2
  local description=$3
  local attempt
  for attempt in {1..40}; do
    expect_true "$probe"
    sleep 0.25
    if [[ $(eval_in_test "$predicate") == t ]]; then
      echo "Board visual E2E: $description"
      return 0
    fi
  done
  echo "Board visual E2E timed out: $description" >&2
  eval_in_test 'e-board-visual-e2e--web-state' >&2 || true
  eval_in_test 'e-board-visual-e2e--delivery' >&2 || true
  exit 1
}

cd "$project_dir"
for asset in ui/board/pkg/e_board.js ui/board/pkg/e_board_bg.wasm; do
  if [[ ! -r $asset ]]; then
    echo "Missing $asset; build Board assets with wasm-pack build --target web --release --out-dir pkg ui/board" >&2
    exit 2
  fi
done
eldev prepare emacs
emacs --daemon="$server_name" --load "$e2e_dir/e-e2e-bootstrap.el"

expect_true "(let ((frame (make-frame '((name . \"e Board visual E2E\") (window-system . ns) (width . 140) (height . 48) (left . -10000) (top . -10000) (alpha . (0 . 0)) (no-accept-focus . t) (no-focus-on-map . t) (skip-taskbar . t) (undecorated . t))))) (with-selected-frame frame (load \"$test_file\" nil nil t) (e-board-visual-e2e-start)))"
echo 'Board visual E2E: public chat opened WebKit Board with two durable tasks'

wait_for '(e-board-visual-e2e-probe-web)' \
  '(e-board-visual-e2e-web-ready-p)' 'WebKit loaded the Board WASM canvas'

expect_true '(e-board-visual-e2e-install-state-observer)'
sleep 0.25
expect_true '(e-board-visual-e2e-send-state)'
wait_for '(e-board-visual-e2e-probe-delivery)' \
  '(e-board-visual-e2e-delivered-p)' \
  'WebKit received the pending required and running optional task groups'

expect_true '(e-board-visual-e2e-send-presentation-action "show-details")'
wait_for 't' '(e-board-visual-e2e-details-open-p)' \
  'Details action opened the full focused Board view'

expect_true '(e-board-visual-e2e-select-pending-task)'
wait_for 't' '(e-board-visual-e2e-pending-task-selected-p)' \
  'WebKit selected the pending task through the inbound event route'
expect_true '(e-board-visual-e2e-publish-update)'
echo 'Board visual E2E: task selection survived a Board update'

expect_true '(e-board-visual-e2e-send-presentation-action "show-hud")'
wait_for 't' '(e-board-visual-e2e-hud-open-p)' \
  'HUD action restored the compact focusless Board view'

expect_true '(e-board-visual-e2e-finish)'
echo 'Board visual E2E complete: all phases passed.'
