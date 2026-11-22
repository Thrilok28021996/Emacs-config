#!/usr/bin/env bash
# Check and benchmark the built-in-only Emacs 31.1 configuration.
set -euo pipefail

D="${SYNC_DIR:-${HOME}/.emacs.d}"
EMACS_BIN="${EMACS:-$(command -v emacs)}"
[[ -x "$EMACS_BIN" ]] || { printf 'emacs not found\n' >&2; exit 1; }

case "${1:-check}" in
  check)
    printf 'Emacs: '; "$EMACS_BIN" --version | sed -n '1p'
    printf 'Configuration: %s\n' "$D/init.el"
    "$EMACS_BIN" --batch -Q \
      --eval "(setq user-emacs-directory \"$D/\")" \
      -l "$D/early-init.el" \
      --eval "(condition-case err (progn (load \"$D/init.el\" nil nil t) (princ \"CONFIG_OK\\n\")) (error (princ (format \"CONFIG_ERROR: %S\\n\" err)) (kill-emacs 1)))"
    ;;
  build)
    "$EMACS_BIN" --batch -Q \
      --eval "(setq user-emacs-directory \"$D/\")" \
      -l "$D/early-init.el" \
      --eval "(byte-compile-file \"$D/init.el\")"
    ;;
  test)
    # Headless acceptance checks for the Org knowledge system.
    # Exit status is the harness's, so a failing check fails this script.
    "$EMACS_BIN" --batch -Q \
      --eval "(setq user-emacs-directory \"$D/\")" \
      -l "$D/early-init.el" \
      -l "$D/init.el" \
      -l "$D/lisp/tests/stage1-check.el" \
      -l "$D/lisp/tests/stage2-check.el" \
      -l "$D/lisp/tests/stage3-check.el" \
      -l "$D/lisp/tests/stage4-check.el"
    ;;
  verify)
    "$0" check && "$0" test && "$0" benchmark
    ;;
  benchmark)
    "$EMACS_BIN" --batch -Q \
      --eval "(setq user-emacs-directory \"$D/\")" \
      -l "$D/early-init.el" \
      --eval "(let ((start (float-time))) (load \"$D/init.el\") (princ (format \"load-seconds=%.3f\\n\" (- (float-time) start))))"
    ;;
  doctor)
    printf 'External executables used by optional workflows:\n'
    for tool in rg clangd pyright-langserver basedpyright clang-format ruff; do
      if command -v "$tool" >/dev/null 2>&1; then
        printf '  ✓ %-20s %s\n' "$tool" "$(command -v "$tool")"
      else
        printf '  - %-20s missing (optional)\n' "$tool"
      fi
    done
    ;;
  *)
    printf 'Usage: %s [check|test|verify|build|benchmark|doctor]\n' "$0"
    exit 2
    ;;
esac
