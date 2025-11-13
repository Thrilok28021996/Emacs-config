#!/bin/bash

echo "🚀 Emacs Package Installation Script"
echo "===================================="



echo "🔄 Loading configuration and pulling/rebuilding packages..."
emacs --batch -l init.el --eval "(straight-pull-all)" --eval "(straight-rebuild-all)"

echo "🎉 Package installation complete!"
