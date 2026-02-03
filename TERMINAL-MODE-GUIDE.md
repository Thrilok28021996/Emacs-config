# Emacs Terminal Mode Guide (emacs -nw)

**Question:** "Why when I use `emacs --nw` will I get any issues by doing so?"

**Answer:** Your config is already optimized for terminal mode! But there are some limitations to be aware of.

---

## ✅ Your Config is Terminal-Friendly

**Good news:** Your configuration already handles terminal mode properly!

```elisp
# Nerd-icons only loads in GUI mode:
:if (display-graphic-p)  # Skips in terminal
```

**What works in terminal:**
- ✅ All core features
- ✅ All keybindings
- ✅ LSP (Eglot)
- ✅ Debugging (dap-mode)
- ✅ Git (Magit)
- ✅ Completion (Corfu)
- ✅ File navigation
- ✅ Org-mode
- ✅ Code editing
- ✅ Compilation
- ✅ Terminal (eshell)

---

## ⚠️ Limitations in Terminal Mode

### 1. **Icons Won't Show** (Minor)

**What happens:**
- Nerd-icons disabled automatically (already configured!)
- File icons won't appear in dired
- Completion icons won't show
- Modeline icons simplified

**Impact:** ⭐ LOW
- Everything works, just looks simpler
- Text-based indicators used instead

**Example:**
```
# GUI mode:
 main.py    (file icon)

# Terminal mode:
main.py      (no icon, just text)
```

---

### 2. **Limited Colors** (Depends on Terminal)

**What happens:**
- 256 colors max (vs millions in GUI)
- Some themes may look different
- Syntax highlighting simplified

**Impact:** ⭐ LOW to MEDIUM
- Depends on your terminal emulator
- Modern terminals (iTerm2, kitty) handle this well

**Fix:**
```bash
# Use a terminal with good color support:
# - iTerm2 (macOS) ✅ Excellent
# - kitty ✅ Excellent
# - Alacritty ✅ Excellent
# - Terminal.app ⚠️ OK (limited colors)
```

**Enable true color in terminal:**
```bash
# Add to ~/.zshrc or ~/.bashrc:
export TERM=xterm-256color

# For true color (24-bit):
export COLORTERM=truecolor
```

---

### 3. **Mouse Support Limited** (Not an Issue for You)

**What happens:**
- Mouse clicks work but less precise
- Scrolling works
- Drag and drop doesn't work

**Impact:** ⭐ NONE
- You use Evil/Vim bindings (keyboard-only)
- Mouse not needed!

---

### 4. **Certain Keybindings May Conflict** (Rare)

**Potential issues:**
```
C-;     # Terminal may not recognize
C-S-v   # Shift+Ctrl combinations
M-;     # Alt key may not work in some terminals
```

**Impact:** ⭐ LOW
- Your main keybindings use `SPC` prefix
- Evil uses standard Vim keys
- Rarely affects workflow

**Fix if needed:**
```elisp
# Remap problematic keys in init.el:
(global-set-key (kbd "C-c ;") 'comment-line)  # Instead of C-;
```

---

### 5. **Some UI Features Simplified**

**What's different:**
```
# GUI mode:
- Smooth scrolling
- Pixel-precise positioning
- Fancy borders
- Native OS widgets

# Terminal mode:
- Character-based scrolling
- Grid-aligned text
- ASCII borders
- Terminal widgets
```

**Impact:** ⭐ LOW
- Functionality same
- Just looks simpler

---

### 6. **Debugging UI May Look Different**

**What happens:**
- DAP mode debug windows still work
- Layout may be slightly different
- No fancy graphics

**Impact:** ⭐ LOW
- All debug features work
- Just text-based display

**Example:**
```
# GUI mode:
┌────────────────┬──────────────┐
│ Code           │ Variables    │
│                │ x = 5        │
│ >>> breakpoint │ y = 10       │
└────────────────┴──────────────┘

# Terminal mode:
+----------------+--------------+
| Code           | Variables    |
|                | x = 5        |
| >>> breakpoint | y = 10       |
+----------------+--------------+
```

---

## 🚀 How to Use Terminal Mode

### Basic Usage:

```bash
# Start Emacs in terminal:
emacs -nw

# Or just:
emacs -nw myfile.py

# Create alias for convenience:
echo 'alias e="emacs -nw"' >> ~/.zshrc
source ~/.zshrc

# Now use:
e myfile.py
```

---

### Recommended Terminal Emulators:

**Best (True Color + All Features):**
1. **iTerm2** (macOS) ⭐⭐⭐⭐⭐
   ```bash
   brew install --cask iterm2
   ```

2. **kitty** ⭐⭐⭐⭐⭐
   ```bash
   brew install --cask kitty
   ```

3. **Alacritty** ⭐⭐⭐⭐⭐
   ```bash
   brew install --cask alacritty
   ```

**Good Enough:**
4. **Terminal.app** (macOS default) ⭐⭐⭐
   - Works but limited colors
   - Good for quick edits

---

## 🎨 Terminal Mode Optimizations

### 1. Enable 256 Colors:

```bash
# Add to ~/.zshrc:
export TERM=xterm-256color

# Verify:
echo $TERM
# Should show: xterm-256color
```

---

### 2. Fix Meta Key (Alt) on macOS:

**iTerm2:**
```
Preferences → Profiles → Keys
→ Left Option Key: Esc+
→ Right Option Key: Esc+
```

**Terminal.app:**
```
Preferences → Profiles → Keyboard
→ Check "Use Option as Meta key"
```

---

### 3. Enable Mouse Support (Optional):

```elisp
# Add to init.el if you want mouse in terminal:
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
```

---

### 4. Improve Performance:

```elisp
# Already optimized in your config!
# But if you want even faster terminal:

(setq redisplay-dont-pause t)           # Faster redisplay
(setq fast-but-imprecise-scrolling t)   # Faster scrolling
```

---

## 🔧 Troubleshooting

### Issue: Colors Look Wrong

**Fix:**
```bash
# 1. Set TERM correctly:
export TERM=xterm-256color

# 2. Restart Emacs

# 3. Check terminal color support:
tput colors
# Should show: 256 or more
```

---

### Issue: Alt/Meta Key Doesn't Work

**Fix:**
```bash
# Terminal.app:
# Preferences → Keyboard → Use Option as Meta key ✓

# iTerm2:
# Preferences → Profiles → Keys → Left Option: Esc+

# Or use Escape as Meta:
# ESC then key = M-key
# Example: ESC x = M-x
```

---

### Issue: Icons Show as Boxes

**This is normal!**
```
# Icons are disabled in terminal mode.
# Your config already handles this correctly.

# Nerd-icons only load with:
:if (display-graphic-p)  # False in terminal

# No action needed!
```

---

### Issue: Some Keys Don't Work

**Common problematic keys:**
```
C-;     → Use C-c ; instead
C-S-v   → Use normal paste
M-;     → Check terminal meta key settings
```

**Your keybindings are safe:**
```
SPC ...   # All work perfectly ✅
g d       # All work perfectly ✅
d d       # All work perfectly ✅
```

---

### Issue: Scrolling Feels Choppy

**Expected behavior:**
- Terminal mode scrolls by characters, not pixels
- This is normal and unavoidable
- Use Vim-style navigation: `C-d`, `C-u` for smooth jumps

---

## 💡 Best Practices

### When to Use Terminal Mode:

**Use `emacs -nw` when:**
- ✅ SSH into remote servers
- ✅ Quick file edits
- ✅ Working in tmux/screen
- ✅ Limited system resources
- ✅ Prefer terminal workflow

**Use GUI mode when:**
- ✅ Primary development machine
- ✅ Want prettier UI
- ✅ Using mouse occasionally
- ✅ Want smooth scrolling

---

### Hybrid Approach (Recommended):

```bash
# Local development:
emacs &          # GUI mode

# Quick edits:
emacs -nw file   # Terminal mode

# Remote work:
ssh server
emacs -nw        # Terminal mode

# Or use daemon:
emacs --daemon   # Start once
emacsclient -nw  # Connect in terminal
emacsclient -c   # Connect in GUI
```

---

## 🎯 Summary

### Will You Get Issues?

**Short answer:** NO (very minor cosmetic differences only)

**Your config handles terminal mode perfectly:**
```
✅ Nerd-icons auto-disabled in terminal
✅ All features work
✅ All keybindings work
✅ Performance optimized
✅ No errors or warnings
```

### Actual Differences:

**Visual only (not functional):**
1. ⭐ No icons (not needed anyway)
2. ⭐ Simpler colors (still readable)
3. ⭐ Character-based UI (still functional)

**Everything else identical:**
- ✅ LSP works
- ✅ Debugging works
- ✅ Git works
- ✅ Completion works
- ✅ All commands work
- ✅ All keybindings work

---

## 🚀 Quick Start

```bash
# 1. Enable 256 colors:
echo 'export TERM=xterm-256color' >> ~/.zshrc
source ~/.zshrc

# 2. Fix Meta key (Terminal.app):
# Preferences → Keyboard → Use Option as Meta ✓

# 3. Try it:
emacs -nw

# 4. Use normally:
# All your keybindings work exactly the same!
# SPC f f, SPC d d, SPC g s, etc.
```

---

## ✅ Verdict

**You can safely use `emacs -nw` without issues!**

Your configuration is already optimized for both GUI and terminal modes. The only differences are cosmetic (no icons, simpler UI), not functional.

**Recommendation:**
- Use GUI mode for primary development (prettier)
- Use terminal mode for quick edits (faster startup)
- Use terminal mode for SSH (only option)

**Both modes work perfectly with your config!** ✅

---

*Terminal mode fully supported*
*No configuration changes needed*
*All features work identically*
