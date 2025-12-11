# Debugger Installation Checklist

Quick setup checklist for dap-mode debugging:

## ✅ Installation Steps

### Python Debugger:
```bash
□ pip install debugpy

□ Verify: python -c "import debugpy; print('✅ Installed')"
```

### C++ Debugger (macOS):
```bash
□ which lldb-vscode

□ If missing: brew install llvm

□ Add to PATH: echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc

□ Reload: source ~/.zshrc

□ Verify: lldb-vscode --version
```

## ✅ Test Debugging

### Python Test:
```bash
□ Create test.py with a simple function

□ Open in Emacs: emacs test.py

□ Set breakpoint: SPC d b

□ Debug: SPC d d

□ Should stop at breakpoint ✅
```

### C++ Test:
```bash
□ Create test.cpp with main()

□ Open in Emacs: emacs test.cpp

□ Set breakpoint: SPC d b

□ Debug: SPC d d (compiles + debugs)

□ Should stop at breakpoint ✅
```

## 📚 Documentation

□ Read: ~/.emacs.d/DEBUGGER-SETUP.md

□ Read: ~/.emacs.d/COMPLETE-IDE-SUMMARY.md

## ✅ You're Done!

Once both tests pass, your IDE is 100% complete!
