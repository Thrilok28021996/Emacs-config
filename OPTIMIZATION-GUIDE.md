# Emacs Optimization & Daily Workflow Guide

**Your Setup:** Emacs 30.2 on macOS (optimized for Python/C++ learning)

---

## 📊 Current Status

### ✅ What's Already Optimized:

1. **Clean Configuration** - 135 lines init.el (excellent!)
2. **Modular Structure** - 10 organized modules
3. **Native Compilation** - Enabled for speed
4. **GC Tuning** - Optimized garbage collection
5. **Evil Mode** - Vim keybindings configured
6. **Modern Completion** - Vertico + Corfu + Consult
7. **LSP Support** - Python (pyright) + C++ (clangd)
8. **Tree-sitter** - Modern syntax highlighting
9. **Org-mode** - With org-roam + templates
10. **Templates** - 6 templates ready (enhanced today!)

### 🎯 Your Setup Score: **8.5/10** (Very Good!)

---

## 🚀 Quick Wins (Do These First!)

### 1. **Start Emacs as Daemon** (Instant Startups!)

**Problem:** Emacs takes 2-3 seconds to start

**Solution:** Run as background daemon

```bash
# Start daemon once (runs in background):
emacs --daemon

# Open client (instant!):
emacsclient -c

# Or create alias:
echo 'alias e="emacsclient -c"' >> ~/.zshrc
source ~/.zshrc

# Now just type: e
```

**Add to macOS login items:**
```bash
# Create LaunchAgent:
mkdir -p ~/Library/LaunchAgents

cat > ~/Library/LaunchAgents/gnu.emacs.daemon.plist << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
"http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>gnu.emacs.daemon</string>
  <key>ProgramArguments</key>
  <array>
    <string>/opt/homebrew/bin/emacs</string>
    <string>--fg-daemon</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>StandardErrorPath</key>
  <string>/tmp/emacs-daemon.err</string>
  <key>StandardOutPath</key>
  <string>/tmp/emacs-daemon.out</string>
</dict>
</plist>
EOF

# Load it:
launchctl load ~/Library/LaunchAgents/gnu.emacs.daemon.plist

# Now Emacs daemon starts at login!
```

**Result:** Instant Emacs windows! ⚡

---

### 2. **Master Key Workflows** (Efficiency!)

Your most important keybindings:

#### **Learning Workflow:**
```
SPC o c → l c    # Course template (with executable code!)
SPC o c → l e    # Learning extract
SPC o c → l t    # Tutorial template
SPC o c → l n    # Quick note
```

#### **Navigation:**
```
SPC f f          # Find file
SPC b b          # Switch buffer
SPC p f          # Project find file
SPC /            # Search in project
```

#### **Tasks:**
```
SPC o o          # Open agenda
SPC o c          # Capture
C-c C-s          # Schedule task
C-c C-d          # Deadline
```

#### **Code:**
```
SPC c a          # Code actions (LSP)
g d              # Go to definition
g r              # Find references
K                # Documentation
```

#### **Window Management:**
```
C-h/j/k/l        # Navigate windows (like Vim)
SPC w s          # Split horizontal
SPC w v          # Split vertical
SPC w d          # Delete window
```

---

### 3. **Create Learning Workflow Script**

**Problem:** Too many steps to start learning session

**Solution:** One command to start everything

```bash
# Create helper script:
cat > ~/.local/bin/learn << 'EOF'
#!/bin/bash
# Open Emacs with today's learning setup

emacsclient -c -e "(progn
  (org-agenda nil \"a\")
  (split-window-right)
  (other-window 1)
  (find-file \"~/Documents/org/learning/current-course.org\"))"
EOF

chmod +x ~/.local/bin/learn

# Usage:
learn  # Opens agenda + current course note!
```

---

## ⚙️ Performance Optimizations

### Current Performance:
```
Startup (daemon): ~1 second
Startup (fresh):  ~2-3 seconds
Memory usage:     ~150-200 MB
```

### Optimization 1: **Lazy Load More Packages**

Add to `init.el` (after line 130):

```elisp
;; Lazy load heavy packages
(use-package org
  :defer t  ; Only load when .org file opened
  :mode ("\\.org\\'" . org-mode))

(use-package magit
  :defer t
  :commands (magit-status magit-dispatch))

(use-package python-mode
  :defer t
  :mode ("\\.py\\'" . python-mode))
```

**Result:** Faster startup, load only when needed

---

### Optimization 2: **Benchmark Startup**

Add this to see what's slow:

```elisp
;; Add to beginning of init.el:
(defun my/display-startup-time ()
  "Show Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)
```

**Result:** Know exactly how fast you are!

---

### Optimization 3: **Clean Up Old Packages**

```bash
# Remove unused compiled files:
rm -rf ~/.emacs.d/eln-cache/*.eln

# Remove old straight builds:
cd ~/.emacs.d/straight
rm -rf build-cache.el

# Restart Emacs - it rebuilds faster!
```

---

## 🐍 Python Development Optimizations

### Current Setup:
- ✅ pyright LSP
- ✅ Tree-sitter syntax
- ✅ Black formatting
- ✅ Flymake linting

### Additions for Better Python Experience:

✅ Already configured in `~/.emacs.d/modules/modern-languages.el`:

```elisp
;; Conda environment management (primary)
;; - Auto-activates conda environments from environment.yml
;; - Manual activation: M-x conda-env-activate

;; iPython REPL integration
;; - Automatically uses iPython if available
;; - Falls back to python3 if not installed

;; Pytest integration
;; - Colored output for better readability
;; - Failed tests run first
;; - Stops after 5 failures
```

**How to use:**

1. **Create conda environment in your project:**
```bash
cd ~/my-project
cat > environment.yml << EOF
name: myproject
dependencies:
  - python=3.11
  - numpy
  - pandas
  - pytest
EOF

conda env create -f environment.yml
```

2. **Open Python file - environment activates automatically:**
```bash
emacs main.py  # Conda env 'myproject' activates!
```

3. **Manual activation if needed:**
```
M-x conda-env-activate RET myproject RET
```

**Keybindings for Python:**
```
M-x conda-env-activate    # Activate conda environment
M-x python-pytest         # Run pytest
C-c C-c (in REPL)         # Execute code
```

---

## ⚡ C++ Development Optimizations

### Current Setup:
- ✅ clangd LSP
- ✅ Tree-sitter syntax
- ✅ clang-format

### Additions for Better C++ Experience:

Add to `~/.emacs.d/modules/modern-languages.el`:

```elisp
;; C++ improvements
(use-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.h\\'" . c++-mode))
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)

  ;; Better C++ indentation
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+))

;; CMake support
(use-package cmake-mode
  :straight t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Modern C++ font-lock
(use-package modern-cpp-font-lock
  :straight t
  :hook (c++-mode . modern-c++-font-lock-mode))
```

**Compile commands:**
```elisp
;; Quick compile (add to c++-mode-hook):
(defun my/cpp-compile ()
  "Compile current C++ file."
  (interactive)
  (compile (format "g++ -std=c++20 -Wall -g %s -o %s"
                   (buffer-file-name)
                   (file-name-sans-extension (buffer-file-name)))))

(define-key c++-mode-map (kbd "C-c C-c") 'my/cpp-compile)
```

**New keybindings for C++:**
```
C-c C-c   # Compile current file
SPC m c   # Compile project
```

---

## 📝 Org-Mode Learning Workflow

### Your Templates (Ready to Use!):

1. **Course Template** (`SPC o c → l c`)
   - Track modules with checkboxes
   - Execute code inline!
   - Schedule reviews

2. **Learning Extract** (`SPC o c → l e`)
   - Extract knowledge from books/articles
   - Link concepts
   - Track mastery level

3. **Tutorial Template** (`SPC o c → l t`)
   - Step-by-step guides
   - Code examples
   - Troubleshooting

4. **Universal Note** (`SPC o c → l n`)
   - Quick capture
   - Flexible structure

### Executable Code (YOUR KILLER FEATURE!):

```org
* Python Basics

** Variables
#+begin_src python :results output
x = 42
y = "Hello"
print(f"{y}, answer is {x}")
#+end_src

# Press C-c C-c to execute!

#+RESULTS:
: Hello, answer is 42

** Lists
#+begin_src python :results value
numbers = [1, 2, 3, 4, 5]
return sum(numbers)
#+end_src

#+RESULTS:
: 15
```

**This is what Obsidian/VS Code CAN'T do!**

---

### Org-Agenda Power User Tips:

```elisp
;; Add to org-config.el:
(setq org-agenda-custom-commands
      '(("d" "Daily Agenda and TODOs"
         ((agenda "" ((org-agenda-span 1)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled TODOs:")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)))
          (todo "DONE"
                ((org-agenda-overriding-header "Completed This Week:")))
          (todo "TODO"
                ((org-agenda-overriding-header "Still TODO:")))))

        ("l" "Learning Tasks"
         ((tags-todo "learning"
                     ((org-agenda-overriding-header "Learning TODOs:")))))))

;; Now use:
;; SPC o o → d (daily view)
;; SPC o o → w (weekly review)
;; SPC o o → l (learning tasks)
```

---

## 🎯 Daily Workflow (Optimized!)

### **Morning (5 minutes):**

```bash
# 1. Start Emacs (if not running):
e  # Opens instantly (daemon)

# 2. Check agenda:
SPC o o

# 3. Check weekly flow:
SPC f f ~/Documents/org/planning/weekly.org

# 4. Start learning:
SPC o c → l c  # If starting new course
# or
SPC f r  # Recent files - open current course note
```

---

### **Learning Session (example: Python course):**

```
# 1. Open course note:
SPC f f → course-python-fundamentals.org

# 2. Navigate to today's module:
Ctrl-s → "Module 3"

# 3. Write notes + code:
* Module 3: Functions

** Basic Functions
#+begin_src python :results output
def greet(name):
    return f"Hello, {name}!"

print(greet("World"))
#+end_src

# 4. Execute code:
C-c C-c (on code block)

# 5. See results inline:
#+RESULTS:
: Hello, World!

# 6. Add more examples:
** Lambda Functions
#+begin_src python :results value
square = lambda x: x ** 2
return [square(i) for i in range(5)]
#+end_src

C-c C-c

#+RESULTS:
: [0, 1, 4, 9, 16]
```

**This workflow is IMPOSSIBLE in VS Code/Obsidian!**

---

### **Coding Session:**

```
# 1. Open project:
SPC p p → select project

# 2. Find file:
SPC p f → start typing filename

# 3. LSP features:
g d        # Go to definition
g r        # Find references
K          # Show documentation
SPC c a    # Code actions
SPC c r    # Rename symbol

# 4. Run code:
SPC o e    # Execute in eshell
or
C-c C-c    # Compile/run

# 5. Git:
SPC g g    # Magit status
s          # Stage
c c        # Commit
P p        # Push
```

---

### **Evening (5 minutes):**

```
# 1. Update weekly flow:
SPC f f ~/Documents/org/planning/weekly.org

* Monday [Entry]
- Completed Module 3
- Learned lambda functions
- Energy: 8/10

# 2. Review tasks:
SPC o o

# 3. Mark tasks DONE:
Move to task → t (toggle TODO/DONE)

# 4. Capture quick idea:
SPC o c → l n
Write note
C-c C-c (save)

# 5. Save all:
C-x s (yes to all)
```

---

## 🔧 Troubleshooting

### **Issue: Emacs is slow**

**Solution 1:** Check what's loaded
```elisp
M-x list-packages  # See installed packages
M-x describe-variable RET load-path  # See what's being loaded
```

**Solution 2:** Profile startup
```bash
emacs --debug-init  # See errors
```

---

### **Issue: LSP not working**

**Check:**
```bash
# Python:
which pyright  # Should be in PATH
pip install pyright  # If missing

# C++:
which clangd  # Should be in PATH
brew install llvm  # If missing (includes clangd)
```

**Restart LSP:**
```
SPC c r  # Restart LSP server
```

---

### **Issue: Code execution not working**

**Check Babel languages:**
```elisp
;; In org-config.el, ensure you have:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (cpp . t)
   (shell . t)))

;; Reload:
M-x load-file RET ~/.emacs.d/config/org-config.el
```

---

## 📚 Learning Resources

### **Emacs:**
- `C-h t` - Built-in tutorial
- `C-h k` - Describe key
- `C-h f` - Describe function
- Your config: `~/.emacs.d/`

### **Org-mode:**
- `C-h i m org RET` - Org manual
- Your templates: `~/.emacs.d/templates/`
- Template guide: `~/.emacs.d/templates/TEMPLATES-GUIDE.md`

### **Tags:**
- `~/.emacs.d/templates/TAGS-REFERENCE.org`

---

## 🎓 Next Level Optimizations

### **1. Org-roam Daily Notes:**

```elisp
;; Add to org-config.el:
(use-package org-roam-dailies
  :after org-roam
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n")))))

;; Keybinding:
;; SPC n d → Today's daily note
```

---

### **2. Snippet Expansion:**

```elisp
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)

  ;; Python snippets:
  ;; Type "def" then TAB:
  ;; def function_name():
  ;;     pass
  )
```

---

### **3. Project Templates:**

```elisp
(use-package projectile
  :config
  (projectile-register-project-type 'python '("setup.py")
    :project-file "setup.py"
    :compile "python setup.py build"
    :test "python -m pytest"
    :run "python -m main"))
```

---

## 🎯 Summary: Your Optimized Workflow

### **What You Have:**
1. ✅ Emacs 30.2 (latest!)
2. ✅ Clean config (135 lines)
3. ✅ Evil mode (Vim bindings)
4. ✅ LSP (Python + C++)
5. ✅ Org-mode + templates
6. ✅ Executable code blocks (UNIQUE!)
7. ✅ Org-roam (knowledge graph)
8. ✅ Task management (agenda)

### **Key Advantages:**
1. 🔥 **Run code in notes** (learning superpower!)
2. 🔥 **One tool for everything** (no context switching)
3. 🔥 **Powerful task management** (org-agenda)
4. 🔥 **Templates ready** (just enhanced today!)
5. 🔥 **Native on Linux** (if you switch)

### **Quick Start Checklist:**

- [ ] Set up Emacs daemon (`emacs --daemon`)
- [ ] Create alias `alias e="emacsclient -c"`
- [ ] Take first course note (`SPC o c → l c`)
- [ ] Execute code in note (`C-c C-c`)
- [ ] Check agenda daily (`SPC o o`)
- [ ] Update weekly flow (`SPC f f weekly.org`)

### **Daily Commands (Memorize These!):**

```
e                # Open Emacs (instant!)
SPC o o          # Agenda
SPC o c → l c    # Course note
C-c C-c          # Execute code!
SPC f f          # Find file
SPC b b          # Switch buffer
C-x s            # Save all
C-c q            # Quit
```

---

**You're ready to rock! 🚀**

*Your Emacs setup is optimized and ready for serious Python/C++ learning with executable notes!*

---

*Last updated: 2025-01-13*
