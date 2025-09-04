#!/bin/bash

echo "🚀 Emacs Package Installation Script"
echo "===================================="

# Check if straight directory exists
if [ ! -d "$HOME/.emacs.d/straight" ]; then
    echo "📥 straight.el not found, bootstrapping..."
    
    # Download and install straight.el
    curl -fLo /tmp/straight-bootstrap.el https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el
    emacs --batch -l /tmp/straight-bootstrap.el
    
    # Install use-package
    echo "📦 Installing use-package..."
    emacs --batch -l "$HOME/.emacs.d/straight/bootstrap.el" --eval "(straight-use-package 'use-package)"
    
    # Clean up
    rm -f /tmp/straight-bootstrap.el
    echo "✅ straight.el bootstrap complete"
else
    echo "✅ straight.el already installed"
fi

echo "🔄 Loading configuration and pulling/rebuilding packages..."
emacs --batch -l init.el --eval "(straight-pull-all)" --eval "(straight-rebuild-all)"

echo "🎉 Package installation complete!"
