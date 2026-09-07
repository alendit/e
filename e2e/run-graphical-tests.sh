#!/usr/bin/env bash
set -euo pipefail

e2e_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$e2e_dir/.." && pwd)
test_file=${E_GRAPHICAL_E2E_TEST_FILE:-$e2e_dir/graphical/e-graphical-test-suite.el}
runner_file=$e2e_dir/graphical/e-graphical-test-runner.el
source_bootstrap_file=$e2e_dir/graphical/e-graphical-source-bootstrap.el
daemon_bootstrap_file=$e2e_dir/graphical/e-graphical-daemon-bootstrap.el
startup_bootstrap_file=$e2e_dir/graphical/e-graphical-startup-bootstrap.el

cd "$project_dir"

emacs_config_mode=${E_E2E_EMACS_CONFIG:-isolated}
if [[ $emacs_config_mode != isolated && $emacs_config_mode != current ]]; then
  echo "E_E2E_EMACS_CONFIG must be isolated or current." >&2
  exit 2
fi
if [[ ${E_GRAPHICAL_E2E_SELECTOR:-} == startup-prewarm && $emacs_config_mode != isolated ]]; then
  echo "startup-prewarm requires E_E2E_EMACS_CONFIG=isolated; refusing to launch Emacs." >&2
  exit 2
fi
export E_E2E_EMACS_CONFIG=$emacs_config_mode

startup_fixture_directory=
startup_stall_directory=
startup_report_file=
startup_fixture_enabled=0

cleanup_startup_fixture() {
  if [[ $startup_fixture_enabled == 1 ]]; then
    startup_fixture_enabled=0
    if [[ -n $startup_report_file ]]; then
      rm -f "$startup_report_file"
    fi
    if [[ -n $startup_fixture_directory ]]; then
      rm -rf "$startup_fixture_directory"
    fi
    if [[ -n $startup_stall_directory ]]; then
      rm -rf "$startup_stall_directory"
    fi
  fi
}

if [[ ${E_GRAPHICAL_E2E_SELECTOR:-} == startup-prewarm && $emacs_config_mode == isolated ]]; then
  startup_fixture_enabled=1
  # Install the EXIT cleanup before allocating either directory so an early
  # validation failure or a headless runner exit cannot leak a partial fixture.
  trap cleanup_startup_fixture EXIT
  startup_fixture_directory=$(mktemp -d -t e-graphical-startup-v5.XXXXXX)
  startup_stall_directory=$(mktemp -d -t e-graphical-startup-stall.XXXXXX)
  startup_report_file=$startup_fixture_directory/startup-report
  export E_RUNTIME_STATE_DIRECTORY=$startup_fixture_directory
  export E_RUNTIME_STORE_TEST_STALL_DIRECTORY=$startup_stall_directory
  export E_GRAPHICAL_E2E_STARTUP_REPORT=$startup_report_file
fi

current_emacs_command=(emacs)
if [[ -n ${E_E2E_EMACS_INIT_DIRECTORY:-} ]]; then
  if [[ $emacs_config_mode != current ]]; then
    echo "E_E2E_EMACS_INIT_DIRECTORY requires E_E2E_EMACS_CONFIG=current." >&2
    exit 2
  fi
  current_emacs_command+=(--init-directory "$E_E2E_EMACS_INIT_DIRECTORY")
fi

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

if [[ $emacs_config_mode == current ]]; then
  eldev prepare emacs
  emacs_command=(
    "${current_emacs_command[@]}"
    --load "$daemon_bootstrap_file"
    --load "$test_file"
    --load "$runner_file"
  )
else
  if [[ $startup_fixture_enabled == 1 ]]; then
    emacs_command=(
      eldev emacs
      --load "$startup_bootstrap_file"
      --load "$source_bootstrap_file"
      --load "$test_file"
      --load "$runner_file"
    )
  else
    emacs_command=(
      eldev emacs
      --load "$source_bootstrap_file"
      --load "$test_file"
      --load "$runner_file"
    )
  fi
fi

system_name=$(uname -s)

if [[ $system_name == Darwin && ${E_GRAPHICAL_E2E_NATIVE_VISIBLE:-} != 1 ]]; then
  server_name=e-graphical-e2e-$$
  report_file=$(mktemp -t e-graphical-e2e-report.XXXXXX)
  emacs_dir=
  if [[ $emacs_config_mode == isolated ]]; then
    emacs_dir=$(mktemp -d -t e-graphical-e2e-emacs.XXXXXX)
  fi
  cleanup() {
    emacsclient --no-wait --socket-name "$server_name" \
      --eval "(kill-emacs 0)" >/dev/null 2>&1 || true
    rm -f "$report_file"
    cleanup_startup_fixture
    if [[ -n $emacs_dir ]]; then
      rm -rf "$emacs_dir"
    fi
  }
  trap cleanup EXIT HUP INT TERM

  eldev prepare emacs
  if [[ $emacs_config_mode == current ]]; then
    "${current_emacs_command[@]}" \
      --daemon="$server_name" --load "$daemon_bootstrap_file"
  else
    if [[ $startup_fixture_enabled == 1 ]]; then
      CFFIXED_USER_HOME="$emacs_dir" \
        E_GRAPHICAL_E2E_EMACS_DIR="$emacs_dir" \
        emacs --quick --daemon="$server_name" \
        --load "$startup_bootstrap_file" --load "$daemon_bootstrap_file"
    else
      CFFIXED_USER_HOME="$emacs_dir" \
        E_GRAPHICAL_E2E_EMACS_DIR="$emacs_dir" \
        emacs --quick --daemon="$server_name" --load "$daemon_bootstrap_file"
    fi
  fi
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
  if [[ $result == 0 ]]; then
    test_status=0
  else
    test_status=$?
  fi
  cleanup_startup_fixture
  exit "$test_status"
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
  cleanup_startup_fixture
  exit "$test_status"
fi

export E_GRAPHICAL_E2E_AUTORUN=1
if "${emacs_command[@]}"; then
  test_status=0
else
  test_status=$?
fi
convert_graphical_screenshots
cleanup_startup_fixture
exit "$test_status"
