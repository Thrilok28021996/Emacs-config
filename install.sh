#!/usr/bin/env bash
# install.sh — Install all straight.el packages + tree-sitter grammars headlessly.
# Run once after cloning: bash install.sh
# Safe to re-run (straight.el skips already-cloned repos; grammars skip if present).

set -euo pipefail

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

if [[ ! -x "$EMACS" ]]; then
  EMACS=$(command -v emacs 2>/dev/null || true)
  if [[ -z "$EMACS" ]]; then
    echo "Emacs not found. Install from https://emacsformacosx.com"
    exit 1
  fi
fi

echo "Using: $EMACS"
echo ""

# ---------------------------------------------------------------------------
# Step 1: Install all straight.el / use-package packages
# ---------------------------------------------------------------------------
echo "==> Installing Emacs packages via straight.el..."
"$EMACS" --batch -Q \
  -l "$HOME/.emacs.d/early-init.el" \
  -l "$HOME/.emacs.d/init.el" \
  --eval "(message \"Packages ready: %d\" (hash-table-count straight--recipe-cache))" \
  2>&1
echo ""

# ---------------------------------------------------------------------------
# Step 2: Compile tree-sitter grammars
# ---------------------------------------------------------------------------
echo "==> Compiling tree-sitter grammars (c, cpp, python, json, css, html)..."
"$EMACS" --batch -Q \
  -l "$HOME/.emacs.d/early-init.el" \
  -l "$HOME/.emacs.d/init.el" \
  --eval "(progn
    (require 'treesit)
    (setq treesit-language-source-alist
          '((c      \"https://github.com/tree-sitter/tree-sitter-c\"      \"v0.20.7\")
            (cpp    \"https://github.com/tree-sitter/tree-sitter-cpp\"    \"v0.20.3\")
            (python \"https://github.com/tree-sitter/tree-sitter-python\" \"v0.20.4\")
            (json   \"https://github.com/tree-sitter/tree-sitter-json\")
            (css    \"https://github.com/tree-sitter/tree-sitter-css\")
            (html   \"https://github.com/tree-sitter/tree-sitter-html\")))
    (dolist (lang '(c cpp python json css html))
      (if (treesit-language-available-p lang)
          (message \"  skip %s (already compiled)\" lang)
        (message \"  building %s...\" lang)
        (treesit-install-language-grammar lang)
        (message \"  done %s\" lang)))
    (message \"Tree-sitter grammars ready.\"))" \
  2>&1
echo ""

echo "==> Done."
echo "    Open Emacs and run SPC h S to byte-compile all modules."
