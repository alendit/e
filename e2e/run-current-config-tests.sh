#!/usr/bin/env bash
set -euo pipefail

e2e_dir=$(cd "$(dirname "$0")" && pwd)
project_dir=$(cd "$e2e_dir/.." && pwd)
bootstrap_file=$e2e_dir/e-e2e-bootstrap.el
test_file=$e2e_dir/e-current-config-e2e-test.el
s7_test_file=$e2e_dir/e-current-config-anthropic-s7-test.el
server_name=e-current-config-e2e-$$
report_file=$(mktemp -t e-current-config-e2e-report.XXXXXX)
state_dir=$(mktemp -d -t e-current-config-e2e-state.XXXXXX)

cd "$project_dir"

if [[ ${E_E2E_EMACS_CONFIG:-current} != current ]]; then
  echo "Current-config E2E requires E_E2E_EMACS_CONFIG=current." >&2
  exit 2
fi
export E_E2E_EMACS_CONFIG=current
unset E_E2E_CONFIG
export E_CURRENT_CONFIG_E2E_STATE_DIR=$state_dir
export E_RUNTIME_STATE_DIRECTORY=$state_dir

selector=${E_CURRENT_CONFIG_E2E_SELECTOR:-'^e-current-config-e2e-test-'}
export E_CURRENT_CONFIG_E2E_SELECTOR=$selector
s7_selector=e-current-config-e2e-test-f97-s7-anthropic-default
if [[ ${E_CURRENT_CONFIG_F97_S7:-0} == 1 ]]; then
  if [[ $selector != "$s7_selector" ]]; then
    echo "F97-S7 requires E_CURRENT_CONFIG_E2E_SELECTOR=$s7_selector." >&2
    exit 2
  fi
  s7_doom_dir=${DOOMDIR:-$HOME/.doom.d}
  s7_config_org=$s7_doom_dir/config.org
  s7_config_el=$s7_doom_dir/config.el
  [[ -f $s7_config_org && -f $s7_config_el ]] || {
    echo "F97-S7 requires the current Doom config.org and config.el." >&2
    exit 2
  }
  export E_CURRENT_CONFIG_F97_REPO_HEAD
  E_CURRENT_CONFIG_F97_REPO_HEAD=$(git -C "$project_dir" rev-parse HEAD)
  export E_CURRENT_CONFIG_F97_DOOM_ORG_SHA256
  E_CURRENT_CONFIG_F97_DOOM_ORG_SHA256=$(shasum -a 256 "$s7_config_org" | cut -d' ' -f1)
  export E_CURRENT_CONFIG_F97_DOOM_EL_SHA256
  E_CURRENT_CONFIG_F97_DOOM_EL_SHA256=$(shasum -a 256 "$s7_config_el" | cut -d' ' -f1)
elif [[ $selector == "$s7_selector" ]]; then
  echo "F97-S7 requires E_CURRENT_CONFIG_F97_S7=1." >&2
  exit 2
fi

emacs_command=(emacs)
if [[ -n ${E_E2E_EMACS_INIT_DIRECTORY:-} ]]; then
  emacs_command+=(--init-directory "$E_E2E_EMACS_INIT_DIRECTORY")
fi

cleanup() {
  emacsclient --socket-name "$server_name" \
    --eval "(kill-emacs 0)" >/dev/null 2>&1 || true
  rm -f "$report_file"
  rm -rf "$state_dir"
}
trap cleanup EXIT HUP INT TERM

eldev prepare emacs
"${emacs_command[@]}" --daemon="$server_name" --load "$bootstrap_file"
result=$(emacsclient --socket-name "$server_name" --eval "
  (progn
    (load \"$test_file\" nil nil t)
    (when (equal (getenv \"E_CURRENT_CONFIG_F97_S7\") \"1\")
      (load \"$s7_test_file\" nil nil t))
    (e-current-config-e2e-test-run-to-file \"$report_file\" \"$selector\"))")
cat "$report_file"
if [[ ${E_CURRENT_CONFIG_F97_S7:-0} == 1 ]]; then
  [[ $E_CURRENT_CONFIG_F97_REPO_HEAD == "$(git -C "$project_dir" rev-parse HEAD)" ]] || {
    echo "F97-S7 repository revision changed during the probe." >&2
    exit 2
  }
  [[ $E_CURRENT_CONFIG_F97_DOOM_ORG_SHA256 == "$(shasum -a 256 "$s7_config_org" | cut -d' ' -f1)" ]] || {
    echo "F97-S7 Doom config.org changed during the probe." >&2
    exit 2
  }
  [[ $E_CURRENT_CONFIG_F97_DOOM_EL_SHA256 == "$(shasum -a 256 "$s7_config_el" | cut -d' ' -f1)" ]] || {
    echo "F97-S7 Doom config.el changed during the probe." >&2
    exit 2
  }
fi
[[ $result == 0 ]]
