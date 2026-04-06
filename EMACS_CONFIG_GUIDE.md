# Emacs Configuration Guide

A comprehensive reference for this Emacs setup — packages, keymaps, and usage patterns.

> **Leader key**: `SPC` in normal/visual mode · `C-SPC` in insert/emacs mode
> **Tip**: Press `SPC` and wait 0.3s to see all available bindings via `which-key`

---

## Table of Contents

1. [Architecture & Loading](#architecture--loading)
2. [Package Overview](#package-overview)
3. [Keymaps Reference](#keymaps-reference)
   - [Quick Access](#quick-access-no-prefix)
   - [Files (SPC f)](#spc-f---files)
   - [Buffers (SPC b)](#spc-b---buffers)
   - [Windows (SPC w)](#spc-w---windows)
   - [Toggles (SPC t)](#spc-t---toggles)
   - [Search (SPC s)](#spc-s---search)
   - [Replace (SPC r)](#spc-r---replace)
   - [Jump/Navigate (SPC j)](#spc-j---jumpnavigate)
   - [Project (SPC p)](#spc-p---project)
   - [Git (SPC g)](#spc-g---git)
   - [Code (SPC c)](#spc-c---code)
   - [LSP/Eglot (SPC l)](#spc-l---lspeglot)
   - [Errors/Diagnostics (SPC e)](#spc-e---errorsdiagnostics)
   - [Debug (SPC d)](#spc-d---debug-dap)
   - [Comments (SPC /)](#spc----comments)
   - [Embark/Multicursor (SPC m)](#spc-m---embark--multiple-cursors)
   - [Org-mode (SPC o)](#spc-o---org-mode)
   - [Notes/Roam (SPC n)](#spc-n---notesorg-roam)
   - [Denote (SPC N)](#spc-n---denote)
   - [Python (SPC P)](#spc-p---python-repl)
   - [Jupyter (SPC J)](#spc-j---jupyter)
   - [Conda (SPC v)](#spc-v---conda)
   - [Markdown (SPC M)](#spc-m---markdown)
   - [Text Manipulation (SPC x)](#spc-x---text-manipulation)
   - [Snippets (SPC i)](#spc-i---snippets)
   - [Workspaces (SPC TAB)](#spc-tab---workspaces)
   - [Help (SPC h)](#spc-h---help--maintenance)
   - [Bookmarks/Calculator (SPC k)](#spc-k---bookmarkscalculator)
   - [Align (SPC a)](#spc-a---align--editing)
   - [Registers (SPC @)](#spc----registers)
   - [Spelling (SPC z)](#spc-z---spelling)
   - [System Utilities (SPC y)](#spc-y---system-utilities)
   - [Diff (SPC D)](#spc-d---diffcomparison)
   - [Search Analysis (SPC S)](#spc-s---search-analysis)
   - [Quit (SPC q)](#spc-q---quit)
   - [Evil / Global Bindings](#evil--global-bindings)
4. [Package Usage Guide](#package-usage-guide)

---

## Prerequisites

External tools required before certain features work. Install once via Homebrew / pip / npm.

### Required for LSP (SPC l e)

| Language | Tool | Install |
|----------|------|---------|
| Python | pyright | `pip install pyright` |
| Python (alt) | pylsp | `pip install python-lsp-server` |
| C / C++ | clangd | `brew install llvm` |
| CSS | vscode-css-language-server | `npm install -g vscode-css-languageservice` |
| HTML | vscode-html-language-server | `npm install -g vscode-html-languageservice` |

### Required for C++ projects (compile_commands.json)

```bash
# CMake projects
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
ln -s build/compile_commands.json .

# Non-CMake (any build system)
brew install bear
bear -- make          # or: bear -- your-build-command
```

### Required for formatting (SPC c f)

| Language | Tool | Install |
|----------|------|---------|
| Python | black | `pip install black` |
| Python imports | isort | `pip install isort` |
| C / C++ | clang-format | `brew install clang-format` |
| Web / JSON | prettier | `npm install -g prettier` |

### Required for search & navigation

| Feature | Tool | Install |
|---------|------|---------|
| `SPC s r`, `SPC n /` | ripgrep | `brew install ripgrep` |
| `g d` fallback (dumb-jump) | ripgrep | `brew install ripgrep` |
| `SPC n g` (org-roam graph) | graphviz | `brew install graphviz` |

### Required for Jupyter (SPC J)

```bash
pip install jupyter
# Start a kernel before using SPC J commands:
# SPC J r → SPC J a → SPC J e
```

### Required for PDF annotation (SPC o n — org-noter)

```elisp
;; Add to init.el or run M-x:
(straight-use-package 'pdf-tools)
(pdf-tools-install)
```

### Required for EPUB reading (nov-mode)

Open any `.epub` file — nov.el is already in the package list.
Evil keybindings apply automatically (`n/p` chapters, `+/-` font size).

---

## Architecture & Loading

The config uses a **phased idle-timer loading** system for a ~0.3s startup time.

```
~/.emacs.d/
├── init.el                   # Main entry point (phased loader)
├── early-init.el             # Runs before packages (GC, UI suppression)
├── config/
│   ├── org-config.el         # Org-mode + roam + journal
│   └── markdown.el           # Markdown + grip preview
└── modules/
    ├── utilities.el           # Shared constants, upgrade system
    ├── core-ui.el             # Bracket matching, line numbers, frames
    ├── modern-performance.el  # GC tuning, native-comp, scrolling
    ├── startup-dashboard.el   # Startup screen
    ├── evil-config.el         # Evil + general.el leader keys
    ├── modern-completion.el   # Vertico, Consult, Corfu, Embark
    ├── modern-ui.el           # Icons, modeline, popups, themes
    ├── modern-languages.el    # Tree-sitter, Eglot, Python, C++, web
    ├── enhanced-colors.el     # Rainbow delimiters, syntax colors
    ├── robustness-enhancements.el  # Error tracking, health checks
    └── debug-support.el       # DAP debugging
```

| Phase | Trigger | What Loads |
|-------|---------|------------|
| **1 – Eager** | Startup | Performance, UI chrome, Evil, theme, dashboard |
| **2 – 0.5s idle** | After startup | Vertico/Consult completion, which-key, shell PATH |
| **3 – 1s idle** | 1s of inactivity | Languages (LSP, Tree-sitter), UI icons, colors |
| **4 – 2s idle** | 2s of inactivity | Robustness, Org-roam, Markdown, DAP debugging |

---

## Package Overview

### Core Infrastructure
| Package | Purpose |
|---------|---------|
| `straight.el` | Git-based reproducible package manager |
| `use-package` | Declarative, deferred package configuration |
| `general.el` | SPC leader key definitions |
| `diminish` | Hide minor mode indicators from modeline |
| `gcmh` | Intelligent garbage collection tuning |

### Evil (Vim Emulation)
| Package | Purpose |
|---------|---------|
| `evil` | Core Vim emulation (modal editing) |
| `evil-collection` | Vim keybindings for dired, magit, help, etc. |
| `evil-surround` | `cs`, `ds`, `ys` surround commands |
| `evil-commentary` | `gc{motion}` to toggle comments |
| `evil-numbers` | `C-c +/-` to increment/decrement numbers |

### Completion (Vertico Ecosystem)
| Package | Purpose |
|---------|---------|
| `vertico` | Vertical candidate list in minibuffer |
| `orderless` | Space-separated fuzzy/regex matching |
| `marginalia` | Rich annotations beside candidates |
| `consult` | 50+ enhanced search/buffer/goto commands |
| `embark` | Context-aware actions on completion candidates |
| `corfu` | Popup in-buffer completion while typing |
| `cape` | Extra completion backends (dabbrev, file, elisp) |
| `savehist` | Persist minibuffer history across sessions |

### UI & Visual
| Package | Purpose |
|---------|---------|
| `doom-themes` | Color schemes (doom-one dark/light) |
| `which-key` | Show available keybindings after prefix |
| `nerd-icons` | Icon font for dired, completion, modeline |
| `mood-line` | Lightweight modeline |
| `popper` | Ephemeral popup buffer management |
| `ace-window` | Jump to window by letter |
| `winner` | Undo/redo window layouts |
| `rainbow-delimiters` | Color-coded nested parentheses |
| `hl-todo` | Highlight TODO/FIXME/HACK keywords |
| `ligature` | Programming ligatures (Fira Code) |

### Languages & Code
| Package | Purpose |
|---------|---------|
| `treesit-auto` | Auto-install Tree-sitter grammars |
| `eglot` | Built-in LSP client (Emacs 29+) |
| `flymake` | Built-in diagnostics, integrates with Eglot |
| `apheleia` | Async code formatting (any formatter) |
| `smartparens` | Balanced parentheses/brackets |
| `yasnippet` + `yasnippet-snippets` | Code snippets (500+ languages) |
| `dap-mode` | Debug Adapter Protocol (GDB, LLDB, Python) |

### Python
| Package | Purpose |
|---------|---------|
| `jupyter` | Jupyter kernel REPL integration |
| `conda` | Conda environment management |
| `python-pytest` | pytest integration |
| `py-isort` | Import sorting |

### Web
| Package | Purpose |
|---------|---------|
| `web-mode` | Mixed PHP/JS/HTML/CSS in one buffer |
| `emmet-mode` | Zen coding (`div.class#id{text}` expansion) |
| `auto-rename-tag` | Rename paired HTML/JSX tags |
| `scss-mode` | SCSS/SASS syntax |

### Git
| Package | Purpose |
|---------|---------|
| `magit` | Full Git porcelain inside Emacs |
| `diff-hl` | Git diff hunks in the gutter |

### Org & Knowledge Management
| Package | Purpose |
|---------|---------|
| `org-roam` | Zettelkasten knowledge graph |
| `org-roam-ui` | Visual graph in browser |
| `org-journal` | Daily journaling |
| `org-super-agenda` | Group agenda by category/priority/tag |
| `org-download` | Drag-and-drop images into org files |
| `org-noter` | Annotate PDFs/EPUBs synced with org |
| `org-transclusion` | Embed/transclude content from other files |
| `org-pomodoro` | 25min/5min Pomodoro timer |
| `denote` | Simple flat-file note system |
| `consult-org-roam` | ripgrep search across roam notes |

### Editing Utilities
| Package | Purpose |
|---------|---------|
| `multiple-cursors` | Multi-cursor editing (Sublime-style) |
| `expand-region` | Expand selection by semantic units |
| `dumb-jump` | Regex-based definition jumping (LSP fallback) |
| `undo-tree` | Visualize undo history as a tree (`C-x u`) |
| `projectile` | Project-based navigation |
| `perspective` | Workspace tabs |

---

## Keymaps Reference

### Quick Access (No Prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `SPC SPC` | `execute-extended-command` | Execute any command (M-x) |
| `SPC '` | `eshell` | Open Eshell |
| `SPC .` | `find-file` | Open file |
| `SPC ,` | `persp-switch-to-buffer` | Switch buffer in workspace |
| `SPC <` | `consult-buffer` | Switch buffer (with preview) |
| `SPC ?` | `consult-line-multi` | Search all open buffers |
| `SPC ;` | `comment-line` | Toggle comment on line |
| `SPC u` | `universal-argument` | `C-u` equivalent |
| `SPC =` | `count-words-region` | Count words |

---

### SPC f - Files

| Key | Command | Description |
|-----|---------|-------------|
| `SPC f f` | `find-file` | Open file |
| `SPC f r` | `consult-recent-file` | Recent files |
| `SPC f s` | `save-buffer` | Save current file |
| `SPC f S` | `save-some-buffers` | Save all modified buffers |
| `SPC f d` | `dired` | Open directory browser |
| `SPC f D` | `delete-file` | Delete file |
| `SPC f R` | `rename-file` | Rename file |
| `SPC f c` | `copy-file` | Copy file |
| `SPC f F` | `consult-find` | Find file by name (project-wide) |
| `SPC f L` | `consult-locate` | Locate file (system-wide) |
| `SPC f j` | `dired-jump` | Open dired at current file |
| `SPC f J` | `dired-jump-other-window` | Dired in other window |
| `SPC f o` | `dired-other-window` | Open dired in other window |
| `SPC f m` | `dired-create-directory` | Create new directory |
| `SPC f y` | — | Copy file path to clipboard |
| `SPC f Y` | — | Copy `file:line` to clipboard |

---

### SPC b - Buffers

| Key | Command | Description |
|-----|---------|-------------|
| `SPC b b` | `consult-buffer` | Switch buffer with live preview |
| `SPC b k` | `kill-buffer` | Kill buffer |
| `SPC b K` | `kill-buffer-and-window` | Kill buffer and close window |
| `SPC b n` | `next-buffer` | Next buffer |
| `SPC b p` | `previous-buffer` | Previous buffer |
| `SPC b P` | `consult-project-buffer` | Switch to project buffer |
| `SPC b r` | `revert-buffer` | Reload file from disk |
| `SPC b s` | `scratch-buffer` | Open scratch buffer |
| `SPC b l` | `list-buffers` | List all buffers |
| `SPC b a` | `mark-whole-buffer` | Select all |
| `SPC b m` | `consult-bookmark` | Jump to bookmark |

---

### SPC w - Windows

| Key | Command | Description |
|-----|---------|-------------|
| `SPC w s` | `split-window-below` | Horizontal split |
| `SPC w v` | `split-window-right` | Vertical split |
| `SPC w d` | `delete-window` | Close window |
| `SPC w o` | `delete-other-windows` | Close all other windows |
| `SPC w w` | `other-window` | Cycle to next window |
| `SPC w h/j/k/l` | `windmove-*` | Move to window left/down/up/right |
| `SPC w =` | `balance-windows` | Equalize window sizes |
| `SPC w m` | `maximize-window` | Maximize current window |
| `SPC w t` | `my/toggle-window-split` | Toggle split direction (H ↔ V) |
| `SPC w a` | `ace-window` | Jump to window by letter |
| `SPC w u` | `winner-undo` | Undo window layout change |
| `SPC w r` | `winner-redo` | Redo window layout change |
| `SPC w p` | `popper-toggle-latest` | Show/hide last popup |
| `SPC w P` | `popper-cycle` | Cycle through popups |

**Global popup shortcuts:**

| Key | Description |
|-----|-------------|
| `` C-` `` | Toggle most recent popup |
| `` M-` `` | Cycle through all popups |
| `` C-M-` `` | Promote/demote popup |

---

### SPC t - Toggles

| Key | Command | Description |
|-----|---------|-------------|
| `SPC t l` | `display-line-numbers-mode` | Line numbers |
| `SPC t r` | `my/toggle-relative-line-numbers` | Relative/absolute line numbers |
| `SPC t w` | `whitespace-mode` | Whitespace display |
| `SPC t h` | `global-hl-line-mode` | Highlight current line |
| `SPC t f` | `auto-fill-mode` | Auto-fill mode |
| `SPC t T` | `my/toggle-theme` | Toggle dark/light theme |
| `SPC t L` | `load-theme` | Load any theme |
| `SPC t c` | `my/toggle-color-identifiers` | Color identifier highlighting |
| `SPC t R` | `my/toggle-rainbow-delimiters` | Rainbow delimiters |
| `SPC t C` | `my/enable-all-color-enhancements` | Enable all color enhancements |
| `SPC t X` | `my/reset-color-enhancements` | Reset color enhancements |
| `SPC t F` | `my/toggle-focus-mode` | Focus/distraction-free mode |
| `SPC t v` | `visual-line-mode` | Visual line mode (word wrap) |
| `SPC t H` | `hl-todo-mode` | Highlight TODO/FIXME keywords |
| `SPC t d` | `diff-hl-mode` | Git diff gutter |
| `SPC t S` | `flyspell-mode` | Flyspell (spell checking) |
| `SPC t A` | `auto-revert-mode` | Auto-revert (reload on disk change) |
| `SPC t o` | `follow-mode` | Follow mode (sync scrolling) |
| `SPC t B` | `abbrev-mode` | Abbreviation mode |
| `SPC t P` | `show-paren-mode` | Show matching parentheses |
| `SPC t E` | `electric-pair-mode` | Auto-close brackets |

---

### SPC s - Search

| Key | Command | Description |
|-----|---------|-------------|
| `SPC s s` | `consult-line` | Search current buffer |
| `SPC s S` | `consult-line-multi` | Search all open buffers |
| `SPC s r` | `consult-ripgrep` | Ripgrep in project |
| `SPC s g` | `consult-grep` | Grep in project |
| `SPC s i` | `consult-imenu` | Jump to symbol in buffer |
| `SPC s I` | `consult-imenu-multi` | Jump to symbol in project |
| `SPC s o` | `consult-outline` | Jump to heading |

---

### SPC r - Replace

| Key | Command | Description |
|-----|---------|-------------|
| `SPC r r` | `query-replace` | Interactive find & replace |
| `SPC r R` | `query-replace-regexp` | Regex find & replace |
| `SPC r w` | — | Query-replace (word at point as initial input) |
| `SPC r s` | `replace-string` | Replace all occurrences |
| `SPC r p` | `project-query-replace-regexp` | Replace across project |
| `SPC r d` | `dired-do-find-regexp-and-replace` | Replace in dired-marked files |
| `SPC r g` | `rgrep` | Grep and replace workflow |

---

### SPC j - Jump/Navigate

| Key | Command | Description |
|-----|---------|-------------|
| `SPC j j` | `consult-line` | Jump to line by content |
| `SPC j i` | `consult-imenu` | Jump to symbol |
| `SPC j o` | `consult-outline` | Jump to heading |
| `SPC j g` | `consult-goto-line` | Go to line number |
| `SPC j m` | `consult-mark` | Jump to mark |
| `SPC j k` | `consult-global-mark` | Jump to global mark |

---

### SPC p - Project

| Key | Command | Description |
|-----|---------|-------------|
| `SPC p f` | `projectile-find-file` | Find file in project |
| `SPC p s` | `consult-ripgrep` | Search in project |
| `SPC p b` | `consult-project-buffer` | Switch to project buffer |
| `SPC p p` | `projectile-switch-project` | Switch project |
| `SPC p c` | `projectile-compile-project` | Compile project |
| `SPC p t` | `projectile-test-project` | Run tests |
| `SPC p r` | `projectile-run-project` | Run project |
| `SPC p d` | `projectile-dired` | Browse project in Dired |
| `SPC p k` | `projectile-kill-buffers` | Kill all project buffers |
| `SPC p i` | `projectile-invalidate-cache` | Refresh project cache |
| `SPC p g` | `consult-find` | Find file by name in project |
| `SPC p C` | `my/setup-cpp-project` | Generate compile_commands.json for clangd |

---

### SPC g - Git

| Key | Command | Description |
|-----|---------|-------------|
| `SPC g s` | `magit-status` | Git status (main Magit view) |
| `SPC g b` | `magit-blame` | Inline git blame |
| `SPC g l` | `magit-log` | Git log |
| `SPC g f` | `magit-file-dispatch` | File-level git operations |
| `SPC g c` | `magit-commit` | Create commit |
| `SPC g p` | `magit-push` | Push to remote |
| `SPC g P` | `magit-pull` | Pull from remote |
| `SPC g F` | `magit-fetch` | Fetch from remote |
| `SPC g g` | `diff-hl-next-hunk` | Go to next changed hunk |
| `SPC g G` | `diff-hl-previous-hunk` | Go to previous changed hunk |
| `SPC g r` | `diff-hl-revert-hunk` | Revert current hunk |
| `SPC g d` | `diff-hl-diff-goto-hunk` | Show diff for hunk |

**Magit usage tips:**
- In `magit-status`: `s` stage, `u` unstage, `c c` commit, `P p` push, `F p` pull
- `TAB` to expand/collapse sections, `RET` to visit file
- `?` in any Magit buffer shows all available keys

---

### SPC c - Code

| Key | Command | Description |
|-----|---------|-------------|
| `SPC c c` | `compile` | Run compile command |
| `SPC c r` | `recompile` | Re-run last compile |
| `SPC c k` | `kill-compilation` | Stop compilation |
| `SPC c f` | `apheleia-format-buffer` | Auto-format buffer (async) |
| `SPC c a` | `eglot-code-actions` | Show LSP code actions |
| `SPC c R` | `eglot-rename` | LSP rename symbol |
| `SPC c i` | `py-isort-buffer` | Sort Python imports |
| `SPC c n` | `compilation-next-error` | Next compile error |
| `SPC c p` | `compilation-previous-error` | Previous compile error |
| `SPC c l` | `compilation-mode` | Switch buffer to compilation mode |

---

### SPC l - LSP/Eglot

| Key | Command | Description |
|-----|---------|-------------|
| `SPC l e` | `eglot` | Start LSP server |
| `SPC l E` | `eglot-ensure` | Auto-start LSP if not running |
| `SPC l d` | `eglot-find-definition` | Go to definition |
| `SPC l r` | `eglot-find-references` | Find all references |
| `SPC l i` | `eglot-find-implementation` | Go to implementation |
| `SPC l t` | `eglot-find-typeDefinition` | Go to type definition |
| `SPC l s` | `eldoc` | Show documentation/signature |
| `SPC l h` | `eldoc-doc-buffer` | Show docs in buffer |
| `SPC l f` | `eglot-format-buffer` | Format buffer via LSP |
| `SPC l R` | `eglot-reconnect` | Reconnect to LSP server |
| `SPC l S` | `eglot-shutdown` | Stop LSP server |
| `SPC l K` | `eglot-shutdown-all` | Stop all LSP servers |

**Quick LSP navigation (Evil normal mode):**

| Key | Description |
|-----|-------------|
| `g d` | Go to definition (`my/lookup-definition`) |
| `g D` | Find all references (`my/lookup-references`) |
| `K` | Show documentation (`my/lookup-documentation`) |

---

### SPC e - Errors/Diagnostics

| Key | Command | Description |
|-----|---------|-------------|
| `SPC e n` | `flymake-goto-next-error` | Next error |
| `SPC e p` | `flymake-goto-prev-error` | Previous error |
| `SPC e l` | `flymake-show-buffer-diagnostics` | List errors in buffer |
| `SPC e L` | `flymake-show-project-diagnostics` | List errors in project |

**Also available in normal mode:**

| Key | Description |
|-----|-------------|
| `] e` | Next error |
| `[ e` | Previous error |

---

### SPC d - Debug (DAP)

| Key | Command | Description |
|-----|---------|-------------|
| `SPC d d` | `my/dap-debug-current-file` | Debug current file (auto-detect type) |
| `SPC d D` | `dap-debug` | Debug with manual configuration |
| `SPC d b` | `dap-breakpoint-toggle` | Toggle breakpoint |
| `SPC d B` | `dap-breakpoint-condition` | Conditional breakpoint |
| `SPC d l` | `dap-breakpoint-log-message` | Log message breakpoint |
| `SPC d a` | `dap-breakpoint-add` | Add breakpoint |
| `SPC d r` | `dap-breakpoint-delete` | Delete breakpoint |
| `SPC d R` | `dap-breakpoint-delete-all` | Delete all breakpoints |
| `SPC d c` | `dap-continue` | Continue execution |
| `SPC d n` | `dap-next` | Step over |
| `SPC d s` | `dap-step-in` | Step into |
| `SPC d o` | `dap-step-out` | Step out |
| `SPC d t` | `dap-restart-frame` | Restart current frame |
| `SPC d q` | `dap-disconnect` | Stop debugging |
| `SPC d Q` | `dap-delete-all-sessions` | Close all debug sessions |
| `SPC d e` | `dap-eval` | Evaluate expression |
| `SPC d E` | `dap-eval-thing-at-point` | Evaluate symbol at cursor |
| `SPC d i` | `dap-ui-inspect-thing-at-point` | Inspect value at cursor |
| `SPC d w` | `dap-ui-expressions-add` | Watch expression |
| `SPC d W` | `dap-ui-expressions-remove` | Remove watched expression |
| `SPC d u` | `dap-ui-repl` | Open debug REPL |
| `SPC d h` | `dap-hydra` | Debug command hydra |
| `SPC d L` | `dap-ui-locals` | Show local variables |
| `SPC d S` | `dap-ui-sessions` | Show debug sessions |
| `SPC d p` | `my/dap-python-test-method` | Debug Python test at point |

---

### SPC / - Comments

| Key | Command | Description |
|-----|---------|-------------|
| `SPC / /` | `comment-or-uncomment-region` | Toggle comment on region |
| `SPC / l` | `comment-line` | Toggle comment on line |
| `SPC / r` | `comment-or-uncomment-region` | Comment/uncomment region |
| `SPC / b` | `comment-box` | Wrap in comment box |
| `SPC / d` | `comment-kill` | Delete comment on line |

**Also via evil-commentary (no prefix needed):**

| Key | Description |
|-----|-------------|
| `gcc` | Comment/uncomment current line |
| `gc{motion}` | Comment/uncomment motion (e.g., `gcap` for paragraph) |
| `gc` (visual) | Comment/uncomment selection |

---

### SPC m - Embark / Multiple Cursors

| Key | Command | Description |
|-----|---------|-------------|
| `SPC m a` | `embark-act` | Context actions on thing at point |
| `SPC m d` | `embark-dwim` | Do-what-I-mean action |
| `SPC m x` | `embark-export` | Export candidates to buffer |
| `SPC m n` | `mc/mark-next-like-this` | Add cursor at next match |
| `SPC m p` | `mc/mark-previous-like-this` | Add cursor at previous match |
| `SPC m A` | `mc/mark-all-like-this` | Add cursors at all matches |
| `SPC m u` | `mc/unmark-next-like-this` | Remove cursor at next match |
| `SPC m U` | `mc/unmark-previous-like-this` | Remove cursor at previous match |
| `SPC m s` | `mc/skip-to-next-like-this` | Skip to next match |
| `SPC m S` | `mc/skip-to-previous-like-this` | Skip to previous match |

**Embark usage**: Put cursor on a file path, URL, symbol, or any candidate in Vertico — press `SPC m a` to see all available actions (open, copy, delete, etc.).

---

### SPC o - Org-mode

| Key | Command | Description |
|-----|---------|-------------|
| `SPC o o` | `org-agenda` | Open agenda view |
| `SPC o c` | `org-capture` | Quick capture note/task |
| `SPC o l` | `org-store-link` | Store link to current location |
| `SPC o i` | `org-insert-link` | Insert stored link |
| `SPC o t` | `org-todo` | Cycle TODO state |
| `SPC o s` | `org-schedule` | Schedule a task |
| `SPC o d` | `org-deadline` | Set deadline |
| `SPC o r` | `org-refile` | Move heading to another file |
| `SPC o a` | `org-archive-subtree` | Archive completed item |
| `SPC o x` | `org-clock-in` | Start time tracking |
| `SPC o z` | `org-clock-out` | Stop time tracking |
| `SPC o p` | `org-pomodoro` | Start 25min Pomodoro timer |
| `SPC o w` | `my/org-writing-mode` | Toggle writing/olivetti mode |
| `SPC o W` | `my/org-word-count` | Count words in subtree/region |
| `SPC o e` | `my/org-export-to-writing-folder` | Export to writing-output/ folder |
| `SPC o n` | `org-noter` | PDF annotation mode |
| `SPC o N` | `org-noter-create-skeleton` | Create note skeleton from PDF headings |
| `SPC o T` | `term` | Terminal emulator |
| `SPC o E` | `eshell` | Eshell |
| `SPC o S` | `shell` | Shell buffer |
| `SPC o A` | `async-shell-command` | Run async shell command |
| `SPC o C` | `shell-command-on-region` | Run shell command on region |

**Quick Org templates** (type at beginning of line then `TAB`):

| Template | Expands to |
|----------|-----------|
| `<s` | Source code block |
| `<q` | Quote block |
| `<e` | Example block |
| `<l` | LaTeX block |

---

### SPC n - Notes/Org-roam

| Key | Command | Description |
|-----|---------|-------------|
| `SPC n f` | `org-roam-node-find` | Find/create roam note |
| `SPC n i` | `org-roam-node-insert` | Insert link to roam note |
| `SPC n I` | `my/org-roam-node-insert-immediate` | Insert link, skip capture buffer |
| `SPC n c` | `org-roam-capture` | Capture to roam note |
| `SPC n n` | `org-capture` | General org capture |
| `SPC n b` | `my/org-roam-open-backlinks` | Open backlinks side window |
| `SPC n B` | `org-roam-buffer-toggle` | Toggle raw roam buffer |
| `SPC n g` | `org-roam-graph` | Show knowledge graph |
| `SPC n u` | `org-roam-ui-mode` | Visual graph in browser |
| `SPC n d` | `org-roam-dailies-goto-today` | Today's daily note |
| `SPC n D` | `org-roam-dailies-goto-date` | Daily note for any date |
| `SPC n y` | `org-roam-dailies-goto-yesterday` | Yesterday's daily note |
| `SPC n t` | `org-roam-dailies-goto-tomorrow` | Tomorrow's daily note |
| `SPC n C` | `org-roam-dailies-capture-today` | Capture to today's daily note |
| `SPC n j` | `org-journal-new-entry` | New journal entry |
| `SPC n l` | `org-cliplink` | Paste URL with title |
| `SPC n L` | `my/org-roam-copy-node-link` | Copy roam link to clipboard |
| `SPC n s` | `org-super-agenda-mode` | Toggle super-agenda view |
| `SPC n /` | `consult-org-roam-search` | Ripgrep across all notes |
| `SPC n <` | `consult-org-roam-backlinks` | Search backlinks |
| `SPC n >` | `consult-org-roam-forward-links` | Search forward links |
| `SPC n F` | `consult-org-roam-file-find` | Find roam file by name |
| `SPC n T` | `org-transclusion-mode` | Toggle transclusion mode |
| `SPC n a` | `org-transclusion-add` | Embed content from another file |
| `SPC n S` | `org-download-screenshot` | Screenshot into note |
| `SPC n Y` | `org-download-yank` | Paste image into note |

---

### SPC N - Denote

| Key | Command | Description |
|-----|---------|-------------|
| `SPC N n` | `denote` | Create new note |
| `SPC N N` | `denote-open-or-create` | Open existing or create new |
| `SPC N f` | `denote-find-file` | Find existing note |
| `SPC N l` | `denote-link` | Link to another note |
| `SPC N b` | `denote-backlinks` | Show backlinks |
| `SPC N B` | `denote-find-backlink` | Find a backlink interactively |
| `SPC N k` | `denote-keywords-add` | Add keyword to note |
| `SPC N K` | `denote-keywords-remove` | Remove keyword from note |
| `SPC N r` | `denote-rename-file` | Rename note |
| `SPC N R` | `denote-rename-file-using-front-matter` | Sync filename from front-matter |
| `SPC N m` | `list-denotes` | List all notes |
| `SPC N s` | `denote-sort-files` | Sort notes by date/title/keywords |

---

### SPC P - Python REPL

| Key | Command | Description |
|-----|---------|-------------|
| `SPC P s` | `run-python` | Start Python REPL |
| `SPC P e` | `python-shell-send-buffer` | Send whole buffer to REPL |
| `SPC P r` | `python-shell-send-region` | Send selection to REPL |
| `SPC P d` | `python-shell-send-defun` | Send function to REPL |
| `SPC P l` | `python-shell-send-statement` | Send line to REPL |
| `SPC P i` | `python-shell-switch-to-shell` | Switch to REPL buffer |
| `SPC P R` | — | Kill and restart Python REPL |

---

### SPC J - Jupyter

| Key | Command | Description |
|-----|---------|-------------|
| `SPC J r` | `my/jupyter-run-python` | Start Python Jupyter kernel |
| `SPC J c` | `jupyter-connect-repl` | Connect to running kernel |
| `SPC J e` | `jupyter-eval-buffer` | Evaluate whole buffer |
| `SPC J s` | `my/jupyter-send-region-or-line` | Evaluate selection or current line |
| `SPC J d` | `jupyter-eval-defun` | Evaluate function |
| `SPC J i` | `jupyter-repl-pop-to-buffer` | Switch to Jupyter REPL |
| `SPC J a` | `my/jupyter-associate-python` | Associate buffer with REPL |
| `SPC J R` | `my/jupyter-restart-kernel` | Restart kernel |
| `SPC J I` | `jupyter-repl-interrupt-kernel` | Interrupt running cell |

---

### SPC v - Conda

| Key | Command | Description |
|-----|---------|-------------|
| `SPC v a` | `conda-env-activate` | Activate environment |
| `SPC v d` | `conda-env-deactivate` | Deactivate environment |
| `SPC v l` | `conda-env-list` | List all environments |
| `SPC v c` | `conda-env-activate-for-buffer` | Auto-activate for file |
| `SPC v r` | `my/conda-activate-and-restart-lsp` | Activate env **and** restart LSP |

---

### SPC M - Markdown

| Key | Command | Description |
|-----|---------|-------------|
| `SPC M m` | `markdown-mode` | Switch to Markdown mode |
| `SPC M p` | `markdown-preview` | Preview rendered Markdown |
| `SPC M t` | `markdown-toc-generate-toc` | Generate table of contents |
| `SPC M g` | `grip-mode` | Live GitHub-rendered preview |
| `SPC M e` | `my/markdown-edit-code-block` | Edit code block in native mode |
| `SPC M M` | `call-last-kbd-macro` | Replay last macro |

**Also:** `C-c '` to edit a fenced code block in its native language mode.

---

### SPC x - Text Manipulation

| Key | Command | Description |
|-----|---------|-------------|
| `SPC x d` | `delete-trailing-whitespace` | Remove trailing spaces |
| `SPC x w` | `whitespace-cleanup` | Clean all whitespace |
| `SPC x s` | `sort-lines` | Sort selected lines |
| `SPC x u` | `upcase-region` | UPPERCASE selection |
| `SPC x l` | `downcase-region` | lowercase selection |
| `SPC x c` | `capitalize-region` | Capitalize selection |
| `SPC x i` | `indent-region` | Re-indent selection |
| `SPC x r` | `rectangle-mark-mode` | Start rectangle selection |
| `SPC x R k` | `kill-rectangle` | Cut rectangle |
| `SPC x R y` | `yank-rectangle` | Paste rectangle |
| `SPC x R o` | `open-rectangle` | Insert blank rectangle |
| `SPC x R c` | `clear-rectangle` | Clear rectangle (blank out) |
| `SPC x R s` | `string-rectangle` | Replace rectangle with string |

---

### SPC i - Snippets

| Key | Command | Description |
|-----|---------|-------------|
| `SPC i s` | `yas-insert-snippet` | Insert a snippet |
| `SPC i n` | `yas-new-snippet` | Create new snippet |
| `SPC i v` | `yas-visit-snippet-file` | Edit existing snippet |

---

### SPC TAB - Workspaces

| Key | Command | Description |
|-----|---------|-------------|
| `SPC TAB TAB` | `persp-switch` | Switch workspace |
| `SPC TAB n` | `persp-next` | Next workspace |
| `SPC TAB p` | `persp-prev` | Previous workspace |
| `SPC TAB c` | `persp-kill` | Close workspace |
| `SPC TAB r` | `persp-rename` | Rename workspace |
| `SPC TAB 1-9` | `my/persp-switch-by-number` | Jump to workspace by number |

---

### SPC h - Help & Maintenance

| Key | Command | Description |
|-----|---------|-------------|
| `SPC h f` | `describe-function` | Document a function |
| `SPC h v` | `describe-variable` | Document a variable |
| `SPC h k` | `describe-key` | What does this key do? |
| `SPC h m` | `describe-mode` | Document current mode |
| `SPC h b` | `describe-bindings` | List all keybindings |
| `SPC h a` | `apropos` | Search documentation |
| `SPC h i` | `info` | Open Info manual browser |
| `SPC h h` | `helpful-at-point` | Rich help at cursor |
| `SPC h p` | `describe-package` | Document a package |
| `SPC h t` | `consult-theme` | Preview and switch theme |
| `SPC h w` | `where-is` | Find what key runs a command |
| `SPC h g` | `garbage-collect` | Manually run GC |
| `SPC h M` | `memory-report` | Show memory usage |
| `SPC h W` | `my/optimize-for-low-memory` | Optimize for low memory |
| `SPC h d` | `my/doctor` | Run health check |
| `SPC h e` | `my/show-error-summary` | Show error summary |
| `SPC h r` | `my/recover-from-errors` | Attempt error recovery |
| `SPC h U` | `my/upgrade` | Full upgrade (with progress) |
| `SPC h C` | `my/upgrade-check` | Check for updates (dry run) |
| `SPC h S` | `my/sync` | Sync/byte-compile packages |

---

### SPC k - Bookmarks/Calculator

| Key | Command | Description |
|-----|---------|-------------|
| `SPC k s` | `bookmark-set` | Set bookmark |
| `SPC k j` | `bookmark-jump` | Jump to bookmark |
| `SPC k d` | `bookmark-delete` | Delete bookmark |
| `SPC k l` | `bookmark-bmenu-list` | List bookmarks |
| `SPC k k` | `calc` | Open calculator |
| `SPC k q` | `quick-calc` | Quick calculation in minibuffer |
| `SPC k r` | `calc-grab-region` | Calculate selected expression |

---

### SPC a - Align & Editing

| Key | Command | Description |
|-----|---------|-------------|
| `SPC a l` | `align-regexp` | Align by regex |
| `SPC a r` | `align-region` | Align region |
| `SPC a c` | `align-current` | Align current section |
| `SPC a e` | `er/expand-region` | Expand selection by semantic unit |
| `SPC a i` | `aggressive-indent-mode` | Toggle aggressive auto-indent |
| `SPC a s` | `highlight-symbol-at-point` | Highlight all occurrences |
| `SPC a n` | `hi-lock-find-patterns` | Highlight by patterns |
| `SPC a p` | `unhighlight-regexp` | Remove highlight pattern |

---

### SPC @ - Registers

| Key | Command | Description |
|-----|---------|-------------|
| `SPC @ r` | `copy-to-register` | Save text to register |
| `SPC @ i` | `insert-register` | Insert text from register |
| `SPC @ j` | `jump-to-register` | Jump to position register |
| `SPC @ w` | `window-configuration-to-register` | Save window layout |
| `SPC @ f` | `frameset-to-register` | Save frameset to register |

---

### SPC z - Spelling

| Key | Command | Description |
|-----|---------|-------------|
| `SPC z z` | `ispell-word` | Check/fix word under cursor |
| `SPC z b` | `ispell-buffer` | Check whole buffer |
| `SPC z r` | `ispell-region` | Check selection |
| `SPC z d` | `ispell-change-dictionary` | Change spell dictionary |
| `SPC z n` | `flyspell-goto-next-error` | Next spelling error |
| `SPC z p` | `flyspell-check-previous-highlighted-word` | Previous spelling error |

---

### SPC y - System Utilities

| Key | Command | Description |
|-----|---------|-------------|
| `SPC y c` | `calendar` | Calendar view |
| `SPC y w` | `eww` | Built-in web browser |
| `SPC y p` | `proced` | Process monitor (like htop) |
| `SPC y l` | `list-packages` | Browse/install packages |
| `SPC y i` | `ibuffer` | Enhanced buffer list |
| `SPC y m` | `man` | View man page |
| `SPC y M` | `woman` | View man page (no external tool) |
| `SPC y s` | `shell-command` | Run shell command |
| `SPC y d` | `apropos-documentation` | Search documentation strings |
| `SPC y C` | `customize` | Emacs customization UI |
| `SPC y G` | `customize-group` | Customize a specific group |

---

### SPC D - Diff/Comparison

| Key | Command | Description |
|-----|---------|-------------|
| `SPC D d` | `diff` | Diff two files |
| `SPC D b` | `diff-buffer-with-file` | Diff buffer vs saved file |
| `SPC D e` | `ediff` | Interactive side-by-side diff |
| `SPC D f` | `ediff-files` | Ediff two files |
| `SPC D B` | `ediff-buffers` | Ediff two buffers |
| `SPC D w` | `compare-windows` | Compare two windows |

---

### SPC S - Search Analysis

| Key | Command | Description |
|-----|---------|-------------|
| `SPC S o` | `occur` | Show all matches in buffer |
| `SPC S m` | `multi-occur` | Occur across multiple buffers |
| `SPC S f` | `flush-lines` | Delete lines matching pattern |
| `SPC S k` | `keep-lines` | Delete lines NOT matching pattern |
| `SPC S h` | `how-many` | Count matches in buffer |
| `SPC S w` | `what-line` | Show current line number |
| `SPC S g` | `goto-char` | Go to character position |
| `SPC S u` | `browse-url` | Open URL in browser |
| `SPC S p` | `browse-url-at-point` | Open URL at cursor |
| `SPC S F` | `ffap` | Find file at point |

---

### SPC q - Quit

| Key | Command | Description |
|-----|---------|-------------|
| `SPC q q` | `my/smart-quit` | Smart quit (save if modified) |
| `SPC q !` | `my/quick-quit` | Force quit without saving (confirms) |
| `SPC q s` | `save-buffers-kill-terminal` | Save all and quit |
| `SPC q r` | `restart-emacs` | Restart Emacs |
| `SPC q x` | `save-buffers-kill-emacs` | Save all and kill Emacs |
| `SPC q d` | `bury-buffer` | Hide buffer without killing |

---

### Evil / Global Bindings

**Normal mode navigation:**

| Key | Description |
|-----|-------------|
| `C-h/j/k/l` | Move between windows |
| `C-u` | Scroll up half page |
| `C-d` | Scroll down half page |
| `C-a` | Mark whole buffer |
| `C-s` | Save buffer |
| `g d` | Go to definition (`my/lookup-definition`) |
| `g D` | Find all references (`my/lookup-references`) |
| `K` | Show documentation (`my/lookup-documentation`) |
| `] e` / `[ e` | Next/previous flymake error |
| `] h` / `[ h` | Next/previous git hunk |
| `] b` / `[ b` | Next/previous buffer |
| `] q` / `[ q` | Next/previous compilation error |
| `] s` / `[ s` | Next/previous spelling error |
| `C-c q` | Smart quit (also works in emacs state) |

**Evil surround (`evil-surround`):**

| Key | Example | Result |
|-----|---------|--------|
| `ys{motion}{char}` | `ysiw"` | Surround word with `"` |
| `cs{old}{new}` | `cs"'` | Change `"` to `'` |
| `ds{char}` | `ds"` | Delete surrounding `"` |
| `S{char}` (visual) | `S(` | Surround selection with `()` |

**evil-numbers:**

| Key | Description |
|-----|-------------|
| `C-c +` | Increment number at point |
| `C-c -` | Decrement number at point |

**Insert mode:**

| Key | Description |
|-----|-------------|
| `C-h` | Delete character backward (not help) |
| `C-w` | Delete word backward |
| `C-u` | Delete to start of line |
| `C-s` | Save buffer |

---

## Package Usage Guide

### Vertico + Orderless (Completion)

Press `M-x`, `SPC f f`, or any minibuffer command to activate Vertico.

- Type space-separated terms to filter: `find buf` matches "find-buffer"
- Use arrow keys or `C-n`/`C-p` to navigate candidates
- `RET` to select, `C-g` to cancel
- In `vertico-directory`: `DEL` goes up a directory

### Consult

`SPC s r` (ripgrep) supports:
- `#pattern` — filter by search term
- `#pattern#filter` — two-level filtering
- Preview with arrow keys, confirm with `RET`

`SPC b b` (buffer switch):
- Type buffer name to filter
- `#` to narrow by type
- Preview updates live as you navigate

### Embark (`SPC m a`)

Works on anything at point or in Vertico:
- On a **file**: open, copy path, delete, rename...
- On a **URL**: open in browser, copy...
- On a **symbol**: describe, find definition, grep...
- On **Vertico candidates**: act on any listed item without leaving completion

### Corfu (In-buffer Completion)

Appears automatically while typing. Navigate with:
- `TAB` / `S-TAB` — next/previous candidate
- `RET` — accept
- `C-g` — dismiss
- `C-h` — toggle documentation popup (via `corfu-popupinfo`)

### Magit

`SPC g s` opens the status buffer:
- `s` / `u` — stage/unstage file or hunk
- `c c` — commit (opens commit message buffer, `C-c C-c` to finalize)
- `P p` — push to tracking branch
- `F p` — pull
- `b b` — switch branch
- `t t` — create tag
- `?` — show all keybindings in current context
- `q` — quit/close Magit window

### Org-roam

1. `SPC n f` — find or create a note (type new name, `RET` to create)
2. `SPC n i` — insert a link to another note while writing
3. `SPC n B` — toggle backlinks panel (see what links to current note)
4. `SPC n u` — open visual graph in browser

Daily notes workflow:
- `SPC n d` — open today's daily note
- `SPC n C` — capture to today's note
- `SPC n y` / `SPC n t` — yesterday's / tomorrow's note

### DAP Debugging

1. Ensure LSP is running (`SPC l e`)
2. Set breakpoints with `SPC d b`
3. Start debug with `SPC d d` (auto-detects file type)
4. Use `SPC d n/s/o` for step over/into/out
5. `SPC d e` to evaluate expressions, `SPC d L` for locals
6. `SPC d q` to stop

### Multiple Cursors (`SPC m`)

1. Place cursor on a symbol
2. `SPC m n` to add cursor at next match, repeat to add more
3. `SPC m A` to instantly mark all matches
4. `SPC m u` / `SPC m s` to unmark/skip next match
5. Type normally — all cursors edit simultaneously
6. `ESC` to return to single cursor

### Popper (Popup Management)

Popups are temporary windows (compilation, help, shell, etc.):
- `` C-` `` — show/hide the most recent popup
- `` M-` `` — cycle through all popups
- `` C-M-` `` — promote popup to regular window (or demote back)
- `SPC w p` / `SPC w P` — same as above via leader key

### YASnippet

1. Type snippet key (e.g., `def` in Python, `fn` in Rust)
2. Press `TAB` to expand
3. Use `TAB` to jump between fields in the snippet
4. `SPC i s` to browse and insert any snippet interactively

### Perspective (Workspaces)

Like browser tabs for Emacs:
- `SPC TAB TAB` — create or switch workspace
- `SPC TAB 1-9` — jump directly to workspace by number
- Each workspace has its own buffer list

### Upgrade & Maintenance

```
SPC h U   Full upgrade (fetch, update, compile, report)
SPC h C   Check what would be updated (dry run)
SPC h S   Byte-compile all changed files
SPC h d   Run health checks
SPC h e   Show any errors encountered
SPC h r   Attempt error recovery
SPC h g   Force garbage collection
SPC h M   Show memory usage report
```

---

*Generated from `~/.emacs.d` configuration — cross-checked against source on 2026-03-01*
