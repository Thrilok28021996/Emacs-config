#!/bin/bash
#
# install_packages.sh — Fresh-install / CI bootstrap for the Emacs config.
#
# Runs Emacs in batch mode (no GUI) to:
#   1. Load init.el (bootstraps straight.el + use-package declarations)
#   2. Pull all package repos and rebuild them
#   3. Freeze versions to a lockfile (straight/versions/default.el)
#      so that my/rollback has a known-good state to restore
#   4. Byte-compile init.el, early-init.el, and modules/*.el
#
# Usage:
#   cd ~/.emacs.d && bash install_packages.sh
#
# Each step runs a separate Emacs process to avoid state leaking
# between phases (e.g. a failed pull shouldn't skip the freeze).

echo "🚀 Emacs Package Installation Script"
echo "===================================="

# Step 1: Load config, clone/pull all repos, rebuild native packages
echo "🔄 Loading configuration and pulling/rebuilding packages..."
emacs --batch -l init.el --eval "(straight-pull-all)" --eval "(straight-rebuild-all)"

# Step 2: Write straight/versions/default.el with pinned commit SHAs
echo "🔒 Freezing package versions (lockfile)..."
emacs --batch -l init.el --eval "(straight-freeze-versions)"

# Step 3: Byte-compile .el -> .elc for faster startup
echo "⚡ Byte-compiling configuration..."
emacs --batch -l init.el --eval "(my/byte-compile-config)"

echo "🎉 Package installation complete!"
