#!/usr/bin/env bash
set -eu

: "${E_GRAPHICAL_E2E_RUNNER:?Set E_GRAPHICAL_E2E_RUNNER to candidate e2e/run-graphical-tests.sh}"
: "${E_GRAPHICAL_E2E_TEST_FILE:?Set E_GRAPHICAL_E2E_TEST_FILE to candidate e2e/e-f99-daily-graphical-test.el}"

state_directory="$(mktemp -d "${TMPDIR:-/tmp}/e-f99-daily-graphical.XXXXXX")"
handoff="${state_directory}/handoff.el"
screenshots="${E_GRAPHICAL_E2E_SCREENSHOT_DIR:-}"

cleanup() {
  rm -f -- "${handoff}" "${handoff}.tmp"
  rmdir -- "${state_directory}" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

run_phase() {
  local selector="$1"
  local phase="$2"
  if [[ -n "${screenshots}" ]]; then
    GRIMOIRE_GRAPHICAL_DAILY_HANDOFF="${handoff}" \
      E_GRAPHICAL_E2E_TEST_FILE="${E_GRAPHICAL_E2E_TEST_FILE}" \
      E_GRAPHICAL_E2E_SELECTOR="${selector}" \
      E_GRAPHICAL_E2E_SCREENSHOT_DIR="${screenshots}/${phase}" \
      bash "${E_GRAPHICAL_E2E_RUNNER}"
  else
    GRIMOIRE_GRAPHICAL_DAILY_HANDOFF="${handoff}" \
      E_GRAPHICAL_E2E_TEST_FILE="${E_GRAPHICAL_E2E_TEST_FILE}" \
      E_GRAPHICAL_E2E_SELECTOR="${selector}" \
      bash "${E_GRAPHICAL_E2E_RUNNER}"
  fi
}

run_phase "e-f99-daily-graphical-test-direct-write-phase-one" "phase-one"
test -s "${handoff}"
sleep 1
run_phase "e-f99-daily-graphical-test-direct-write-phase-two" "phase-two"
test ! -e "${handoff}"
