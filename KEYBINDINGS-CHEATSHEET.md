# Emacs Keybindings Cheatsheet

**Quick reference for your optimized setup**

---

## 🚀 Essential (Memorize These First!)

| Key | Action | Context |
|-----|--------|---------|
| `SPC o c` | **Capture** (open menu) | Global |
| `SPC o o` | **Agenda** (tasks) | Global |
| `SPC f f` | **Find file** | Global |
| `SPC b b` | **Switch buffer** | Global |
| `C-c C-c` | **Execute code block** | Org-mode |
| `C-x s` | **Save all** | Global |
| `C-c q` | **Quit** Emacs | Global |

---

## 📝 Org Capture Templates

| Key | Template | Use For |
|-----|----------|---------|
| `SPC o c` → `l c` | **Course** | Online courses |
| `SPC o c` → `l e` | **Learning Extract** | Book/article notes |
| `SPC o c` → `l t` | **Tutorial** | Step-by-step guides |
| `SPC o c` → `l n` | **Universal Note** | Quick capture |
| `SPC o c` → `p` | **Project** | Project tracking |
| `SPC o c` → `f` | **Weekly Flow** | Weekly planning |

**After capture:** `C-c C-c` to save, `C-c C-k` to cancel

---

## 📂 File & Buffer Navigation

| Key | Action |
|-----|--------|
| `SPC f f` | Find file |
| `SPC f r` | Recent files |
| `SPC f s` | Save file |
| `SPC b b` | Switch buffer |
| `SPC b d` | Delete buffer |
| `SPC b i` | Open ibuffer |
| `SPC p f` | Find file in project |
| `SPC p p` | Switch project |
| `SPC /` | Search in project |

---

## ✏️ Editing (Evil/Vim-style)

### Normal Mode:
| Key | Action |
|-----|--------|
| `i` | Insert mode (before cursor) |
| `a` | Append (after cursor) |
| `o` | New line below |
| `O` | New line above |
| `v` | Visual mode |
| `V` | Visual line mode |
| `d d` | Delete line |
| `y y` | Yank (copy) line |
| `p` | Paste after cursor |
| `P` | Paste before cursor |
| `u` | Undo |
| `C-r` | Redo |
| `/` | Search forward |
| `?` | Search backward |
| `n` | Next search result |
| `N` | Previous search result |

### Insert Mode:
| Key | Action |
|-----|--------|
| `ESC` | Back to normal mode |
| `C-s` | Save buffer |
| `C-h` | Backspace |
| `C-w` | Delete word |
| `C-u` | Delete to line start |

---

## 🪟 Window Management

| Key | Action |
|-----|--------|
| `SPC w s` | Split horizontal |
| `SPC w v` | Split vertical |
| `SPC w d` | Delete window |
| `SPC w o` | Delete other windows |
| `C-h` | Move to left window |
| `C-j` | Move to down window |
| `C-k` | Move to up window |
| `C-l` | Move to right window |

---

## 💻 Coding (LSP)

| Key | Action |
|-----|--------|
| `g d` | Go to definition |
| `g r` | Find references |
| `K` | Show documentation |
| `SPC c a` | Code actions |
| `SPC c r` | Rename symbol |
| `SPC c f` | Format buffer |
| `SPC c d` | Show diagnostics |

### Python-Specific:
| Key | Action |
|-----|--------|
| `SPC m t` | Run pytest |
| `SPC m v` | Activate venv |
| `C-c C-c` | Send region to Python REPL |

### C++-Specific:
| Key | Action |
|-----|--------|
| `C-c C-c` | Compile current file |
| `SPC m c` | Compile project |

---

## 📋 Org-Mode

### Structure:
| Key | Action |
|-----|--------|
| `TAB` | Cycle fold |
| `S-TAB` | Cycle all folds |
| `M-RET` | New heading |
| `M-←/→` | Promote/demote heading |
| `M-↑/↓` | Move heading up/down |

### Code Blocks:
| Key | Action |
|-----|--------|
| `C-c C-c` | **Execute code block** |
| `C-c '` | Edit block in separate buffer |
| `<s TAB` | Insert src block template |

### Tasks:
| Key | Action |
|-----|--------|
| `C-c C-t` | Cycle TODO state |
| `C-c C-s` | Schedule task |
| `C-c C-d` | Add deadline |
| `C-c C-c` | Toggle checkbox [ ] |

### Tables:
| Key | Action |
|-----|--------|
| `TAB` | Next cell / align table |
| `S-TAB` | Previous cell |
| `M-↑/↓` | Move row |
| `M-←/→` | Move column |
| `M-S-↑` | Insert row above |
| `M-S-↓` | Insert row below |
| `C-c C-c` | Recalculate formulas |

### Links:
| Key | Action |
|-----|--------|
| `C-c C-l` | Insert/edit link |
| `C-c C-o` | Open link |
| `C-c C-x C-v` | Toggle inline images |

---

## 📅 Org-Agenda

| Key | Action |
|-----|--------|
| `SPC o o` | Open agenda |
| `t` | Change TODO state |
| `q` | Quit agenda |
| `r` | Refresh |
| `s` | Save all org files |
| `RET` | Go to task |
| `/` | Filter by tag |
| `f` | Forward (next day/week) |
| `b` | Backward (prev day/week) |
| `v d` | Day view |
| `v w` | Week view |
| `v m` | Month view |

---

## 🔗 Org-Roam

| Key | Action |
|-----|--------|
| `SPC n f` | Find/create roam note |
| `SPC n i` | Insert roam link |
| `SPC n c` | Org-roam capture |
| `SPC n g` | Open roam graph (if UI installed) |

---

## 🔍 Search & Replace

| Key | Action |
|-----|--------|
| `C-s` | Search forward |
| `C-r` | Search backward |
| `M-%` | Query replace |
| `SPC s s` | Search symbol |
| `SPC s p` | Search project |

---

## 📦 Package Management

| Key | Action |
|-----|--------|
| `M-x package-refresh-contents` | Refresh packages |
| `M-x package-list-packages` | List all packages |

---

## 🔧 Help System

| Key | Action |
|-----|--------|
| `C-h k` | Describe key |
| `C-h f` | Describe function |
| `C-h v` | Describe variable |
| `C-h m` | Describe mode |
| `C-h t` | Tutorial |
| `SPC h k` | Which-key (shows available keys) |

---

## 🎯 My Custom Commands

| Key | Action | Note |
|-----|--------|------|
| `C-c q` | Smart quit | Prompts to save |
| `C-a` | Select all | Both normal & visual |
| `C-s` | Save | Works in insert & normal |

---

## 🔥 Pro Tips

### Quick File Open:
```
SPC f f        # Start typing filename
~/.emacs.d/    # Your config
~/Documents/org/  # Your notes
```

### Learning Session Workflow:
```
1. SPC o c → l c     # Open course template
2. Write notes + code
3. C-c C-c           # Execute code blocks
4. C-x s             # Save
5. SPC o o           # Check agenda
```

### Code Editing Workflow:
```
1. SPC p p           # Open project
2. SPC p f           # Find file
3. g d               # Navigate to definition
4. SPC c a           # Code actions
5. SPC g g           # Git status (Magit)
```

### Daily Review:
```
1. SPC o o           # Open agenda
2. t                 # Toggle TODO/DONE
3. SPC f f           # Open weekly flow
4. Update progress
5. C-x s             # Save all
```

---

## 🎓 Learning Path

**Week 1: Essentials**
- [ ] Master file navigation (`SPC f f`, `SPC b b`)
- [ ] Learn org-capture templates
- [ ] Practice code execution (`C-c C-c`)

**Week 2: Efficiency**
- [ ] Master window management (`C-h/j/k/l`)
- [ ] Learn LSP features (`g d`, `g r`, `K`)
- [ ] Use agenda daily (`SPC o o`)

**Week 3: Power User**
- [ ] Org-roam linking (`SPC n f`, `SPC n i`)
- [ ] Magit for Git (`SPC g g`)
- [ ] Custom capture templates

---

## 🆘 Emergency Commands

**Something went wrong:**
```
C-g              # Cancel current command
M-x recover-session  # Recover crashed session
M-x revert-buffer    # Reload file from disk
```

**Emacs frozen:**
```
C-z              # Suspend Emacs (fg to resume)
pkill -USR2 emacs    # Force garbage collection (from terminal)
```

**Restart:**
```
M-x restart-emacs    # Clean restart
```

---

## 📱 Quick Reference

**Print this section and keep it handy:**

```
ESSENTIAL 5:
SPC o c  = Capture
SPC o o  = Agenda
SPC f f  = Find file
C-c C-c  = Execute code (Org) / Confirm
C-x s    = Save all

NAVIGATION:
SPC b b  = Buffers
SPC p f  = Project files
g d      = Go to definition
C-h/j/k/l = Move between windows

EDITING:
i/a/o    = Insert modes
d d      = Delete line
y y      = Copy line
u        = Undo
```

---

**Master these, and you'll be flying! ✈️**

*Save this file: `~/.emacs.d/KEYBINDINGS-CHEATSHEET.md`*
*Print and keep near your desk!*
