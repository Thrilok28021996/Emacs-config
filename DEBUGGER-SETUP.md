# Debugger Setup Guide (dap-mode)

**Status:** ✅ Installed and configured for Python & C++

---

## 🎯 What You Got

Full debugging support for Python and C++ with:
- **Breakpoints** - Click to pause execution
- **Step through code** - Line by line execution
- **Variable inspection** - See values in real-time
- **Expressions** - Evaluate code during debug
- **Call stack** - See function calls
- **Debug REPL** - Interactive debugging

---

## 📋 Installation Steps

### 1. Install Python Debugger (debugpy)

```bash
# Install in your conda base environment
conda install -c conda-forge debugpy

# OR install with pip
pip install debugpy
```

**Verify:**
```bash
python -c "import debugpy; print(debugpy.__version__)"
# Should print version like: 1.8.0
```

---

### 2. Install C++ Debugger (lldb-vscode)

**macOS already has LLDB!** Just verify:

```bash
# Check if lldb-vscode is available
which lldb-vscode

# If not found, install LLVM
brew install llvm

# Add to PATH (add to ~/.zshrc)
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
```

**Verify:**
```bash
lldb-vscode --version
# Should show LLDB version
```

---

## 🚀 Quick Start

### Python Debugging

**1. Open a Python file:**
```python
# test.py
def calculate(x, y):
    result = x + y
    return result

def main():
    a = 5
    b = 10
    answer = calculate(a, b)
    print(f"Answer: {answer}")

if __name__ == "__main__":
    main()
```

**2. Set breakpoint:**
- Move cursor to line 3 (inside calculate function)
- Press: `SPC d b` (toggle breakpoint)
- You'll see a red dot in the margin

**3. Start debugging:**
- Press: `SPC d d` (debug current file)
- Code will run and stop at your breakpoint

**4. Debug commands:**
```
SPC d n    # Step over (next line)
SPC d s    # Step into (enter function)
SPC d o    # Step out (exit function)
SPC d c    # Continue (run to next breakpoint)
SPC d q    # Quit debugging
```

**5. Inspect variables:**
- Hover over variable to see value
- Or press `SPC d E` on a variable
- Windows show locals automatically

---

### C++ Debugging

**1. Create a C++ file:**
```cpp
// test.cpp
#include <iostream>

int add(int x, int y) {
    int result = x + y;  // Set breakpoint here
    return result;
}

int main() {
    int a = 5;
    int b = 10;
    int answer = add(a, b);
    std::cout << "Answer: " << answer << std::endl;
    return 0;
}
```

**2. Set breakpoint:**
- Move to line 5 (int result = x + y)
- Press: `SPC d b`

**3. Start debugging:**
- Press: `SPC d d`
- Will compile with debug symbols (-g) automatically
- Then starts debugger

**4. Same debug commands:**
```
SPC d n    # Step over
SPC d s    # Step into
SPC d o    # Step out
SPC d c    # Continue
SPC d q    # Quit
```

---

## 🎮 Complete Keybindings

### Breakpoints:
```
SPC d b    # Toggle breakpoint at current line
SPC d B    # Conditional breakpoint (break if x > 10)
SPC d l    # Log point (print without stopping)
SPC d a    # Add breakpoint
SPC d r    # Remove breakpoint
SPC d R    # Remove ALL breakpoints
```

### Execution:
```
SPC d d    # Debug current file (smart - detects Python/C++)
SPC d D    # Debug with template (Flask, Django, etc.)
SPC d c    # Continue execution
SPC d n    # Step over (next line)
SPC d s    # Step into function
SPC d o    # Step out of function
SPC d t    # Restart current frame
SPC d q    # Stop debugging
SPC d Q    # Kill all debug sessions
```

### Inspection:
```
SPC d e    # Evaluate expression (type Python/C++ code)
SPC d E    # Evaluate thing at cursor
SPC d i    # Inspect variable at cursor
SPC d w    # Add watch expression (monitor variable)
SPC d W    # Remove watch expression
```

### UI Windows:
```
SPC d u    # Open debug REPL (interactive console)
SPC d h    # Show ALL debug commands (hydra menu)
SPC d L    # Show local variables window
SPC d S    # Show debug sessions window
```

### Python-Specific:
```
SPC d p    # Debug current pytest test method
```

---

## 📁 Debug Templates Available

### Python:
1. **Python :: Run file** - Debug current Python file
2. **Python :: Run file with arguments** - Pass args to script
3. **Python :: Run pytest** - Debug all tests
4. **Python :: Flask** - Debug Flask app
5. **Python :: Django** - Debug Django app

### C++:
1. **C++ :: Run binary** - Debug compiled program
2. **C++ :: Run with arguments** - Debug with CLI args
3. **C++ :: GDB Run** - Use GDB instead of LLDB

---

## 🎓 Example Workflows

### Example 1: Debug Python Script

```bash
# 1. Open file
emacs script.py

# 2. Set breakpoint (SPC d b) on line you want to inspect

# 3. Start debug (SPC d d)

# 4. When stopped at breakpoint:
#    - Press SPC d E on variables to see values
#    - Press SPC d n to go to next line
#    - Press SPC d s to step into functions
#    - Press SPC d c to continue

# 5. Stop (SPC d q)
```

---

### Example 2: Debug Python Test

```python
# test_math.py
def add(a, b):
    return a + b  # Set breakpoint here

def test_add():
    result = add(2, 3)
    assert result == 5
```

```bash
# 1. Open test file
emacs test_math.py

# 2. Move cursor to test_add function

# 3. Debug test: SPC d p

# 4. Step through with SPC d n
```

---

### Example 3: Debug C++ with Arguments

```cpp
// args.cpp
#include <iostream>
int main(int argc, char* argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << "Arg " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

```bash
# 1. Open file and set breakpoint

# 2. Debug with template: SPC d D

# 3. Select "C++ :: Run with arguments"

# 4. Enter args when prompted: arg1 arg2 arg3

# 5. Debug as normal
```

---

## 🪟 Debug Windows Layout

When debugging, you'll see:

```
┌─────────────────────────┬──────────────────┐
│                         │  *dap-ui-locals* │
│                         │  (variables)     │
│   YOUR CODE             ├──────────────────┤
│   (with breakpoint)     │  *dap-ui-expr*   │
│                         │  (watches)       │
│                         ├──────────────────┤
│                         │  *dap-ui-sess*   │
├─────────────────────────┴──────────────────┤
│  *dap-ui-repl* (interactive debug console)│
└────────────────────────────────────────────┘
```

**Navigate windows:** `C-h/j/k/l` (Vim-style)

---

## 🔧 Troubleshooting

### "debugpy not found"

**Fix:**
```bash
# Make sure debugpy installed in conda env
conda activate myenv
pip install debugpy

# Or install globally
pip install debugpy
```

---

### "lldb-vscode not found"

**Fix:**
```bash
# Install LLVM
brew install llvm

# Add to PATH
echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Verify
which lldb-vscode
```

---

### "No debug template found"

**Fix:**
```bash
# Use SPC d D instead of SPC d d
# This lets you select a template manually
```

---

### "Debug session won't start"

**Fix:**
1. Check if file is saved: `SPC f s`
2. Make sure conda env activated
3. Try `SPC d Q` to kill all sessions
4. Restart Emacs

---

### C++ compile errors during debug

**Fix:**
```bash
# SPC d d compiles with: g++ -std=c++20 -g -O0 file.cpp

# If errors, fix them first
# Then try SPC d d again
```

---

## 🎯 Pro Tips

### 1. Conditional Breakpoints
```
# Break only if x > 100:
SPC d B
# Enter condition: x > 100
```

### 2. Log Points (print without stopping)
```
# Print value without breaking:
SPC d l
# Enter message: "Value is {x}"
```

### 3. Debug REPL
```
# Open REPL: SPC d u
# Type Python code interactively
>>> print(x + y)
>>> dir(object)
```

### 4. Watch Expressions
```
# Monitor variable across execution:
SPC d w
# Enter: x * 2
# Updates automatically as you step
```

### 5. Quick Inspect
```
# Hover over variable with mouse
# OR
# Put cursor on variable, SPC d E
```

---

## 📊 Debug vs Print Debugging

### Print Debugging (old way):
```python
def calculate(x, y):
    print(f"DEBUG: x={x}, y={y}")  # Add prints
    result = x + y
    print(f"DEBUG: result={result}")  # More prints
    return result
```

### DAP Debugging (new way):
```python
def calculate(x, y):
    result = x + y  # Set breakpoint, inspect x, y, result
    return result
```

**Benefits:**
- ✅ No code changes needed
- ✅ See ALL variables automatically
- ✅ Step through code
- ✅ Modify values on the fly
- ✅ No cleanup after debugging

---

## ✅ Summary

**You now have:**
- Full Python debugging (like PyCharm)
- Full C++ debugging (like Visual Studio)
- Breakpoints, step-through, inspection
- Debug REPL, watches, conditional breaks
- Works with conda environments
- Zero configuration needed

**Next time you debug:**
1. `SPC d b` - Set breakpoint
2. `SPC d d` - Start debug
3. `SPC d n` - Step through
4. `SPC d q` - Stop

**Your Emacs is now a complete IDE!** 🎉

---

*Setup complete!*
*Ready to debug Python and C++*
