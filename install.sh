#!/usr/bin/env bash
# install.sh — Install all straight.el packages headlessly from the terminal.
# Run once after cloning: bash install.sh
# This loads the full config in batch mode, which causes straight.el to clone
# every package declared with use-package (straight-use-package-by-default t).

set -euo pipefail

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

if [[ ! -x "$EMACS" ]]; then
  EMACS=$(command -v emacs 2>/dev/null || true)
  if [[ -z "$EMACS" ]]; then
    echo "Emacs not found. Install from https://emacsformacosx.com or: brew install --cask emacs"
    exit 1
  fi
fi

echo "Using Emacs: $EMACS"
echo "Installing all straight.el packages (this takes a few minutes on first run)..."
echo ""

"$EMACS" --batch -Q \
  -l "$HOME/.emacs.d/early-init.el" \
  -l "$HOME/.emacs.d/init.el" \
  --eval "(message \"All packages installed. Count: %d\" (hash-table-count straight--recipe-cache))" \
  2>&1

echo ""
echo "Done. Open Emacs and run SPC h S to byte-compile all modules."
