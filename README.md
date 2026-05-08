# Emacs 30.2 Configuration

Vim-centric Emacs config with Evil, LSP, org-roam PKM, and Learning OS. Single file: `init.el`.

---

## Prerequisites

### Required

| Tool | Install | Purpose |
|------|---------|---------|
| Emacs 30.2+ | `brew install emacs-plus@30` | Editor |
| ripgrep | `brew install ripgrep` | Project search |
| Node.js | `brew install node` | LSP servers |
| pyright | `npm install -g pyright` | Python LSP |

### Per-language

| Tool | Install | Purpose |
|------|---------|---------|
| ruff | `brew install ruff` | Python formatter |
| clangd | `brew install llvm` | C/C++ LSP |
| clang-format | included with llvm | C/C++ formatter |
| prettier | `npm install -g prettier` | JS/TS/CSS/HTML formatter |
| shfmt | `brew install shfmt` | Shell formatter |
| pandoc | `brew install pandoc` | Markdown preview |
| graphviz | `brew install graphviz` | org-roam graph |
| miniconda | [docs.conda.io](https://docs.conda.io) | Python env management |
| LM Studio | [lmstudio.ai](https://lmstudio.ai) | Local AI assistant |

### Fonts

Install **Victor Mono** (preferred) or falls back to Menlo.

### Directories

```bash
mkdir -p ~/Documents/garden/learn ~/Documents/garden/images
mkdir -p ~/org-roam   # if using separate roam dir (currently uses garden)
```

---

## Installation

```bash
git clone <repo> ~/.emacs.d
emacs --daemon   # start server
```

Add to `~/.zshrc`:

```bash
emacs --daemon 2>/dev/null &
```

---

## Folder Structure

All notes live under one root:

```
~/Documents/garden/
├── *.org               ← general org notes, roam nodes
├── images/             ← org-download image attachments
└── learn/
    ├── inbox.org       ← raw input (process within 24h)
    ├── concepts.org    ← atomic concepts
    ├── questions.org   ← recall questions
    ├── reviews.org     ← spaced repetition schedule
    ├── projects.org    ← applied projects
    ├── journal.org     ← daily learning log
    └── los             ← CLI script
```

---

## Packages

### Core

| Package | Purpose |
|---------|---------|
| evil | Vim keybindings |
| evil-collection | Evil bindings for all major modes |
| evil-commentary | `gcc` to comment |
| evil-surround | `cs"'` to change surrounding quotes |
| evil-matchit | `%` to jump between matched pairs |
| general | Leader key (`SPC`) definitions |
| undo-fu | Undo/redo backend for evil |
| which-key | Shows key hints after delay |

### Completion

| Package | Purpose |
|---------|---------|
| vertico | Vertical completion UI |
| orderless | Fuzzy/space-separated completion matching |
| marginalia | Annotations in completion lists |
| consult | Enhanced search, buffer, imenu commands |

### UI

| Package | Purpose |
|---------|---------|
| doom-themes | `doom-one` dark theme |
| doom-modeline | Status bar with icons |
| nerd-icons | Icon font (run `M-x nerd-icons-install-fonts`) |

### Editing

| Package | Purpose |
|---------|---------|
| **electric-pair-mode** (built-in) | Auto-pair brackets |
| rainbow-delimiters | Color-coded bracket depth |
| **corfu** | In-buffer autocomplete (replaces company) |
| **cape** | Extra completion sources (file, dabbrev, keyword) |
| yasnippet | Code snippets |
| ws-butler | Strip trailing whitespace on save |
| highlight-indent-guides | Indent level lines |

### LSP & Languages

| Package | Purpose |
|---------|---------|
| **eglot** (built-in) | LSP client — auto-detects pyright, clangd |
| **flymake** (built-in) | Inline error checking |
| treesit-auto | Auto-install tree-sitter grammars |
| apheleia | Async format on save |

### Git

| Package | Purpose |
|---------|---------|
| magit | Full git UI |
| diff-hl | Gutter diff indicators |

### Org / PKM

| Package | Purpose |
|---------|---------|
| org | Built-in org mode |
| org-roam | Linked notes (Zettelkasten) |
| org-roam-ui | Visual graph of notes |
| consult-org-roam | Ripgrep search across roam |
| org-super-agenda | Grouped agenda view |
| org-modern | Modern org visual style |
| org-pomodoro | Pomodoro timer tied to tasks |
| org-appear | Reveal emphasis markers when cursor is on text |
| org-download | Paste/drag images into org buffers |
| org-transclusion | Embed content from other org files inline |
| org-cliplink | Paste a URL and auto-fetch its title |
| deft | Fast full-text note search |
| olivetti | Centered writing mode |

### Tools

| Package | Purpose |
|---------|---------|
| **project.el** (built-in) | Project management (replaces projectile) |
| **which-key** (built-in in 30) | Key hints |
| **pixel-scroll-precision-mode** (built-in) | Smooth pixel-level scrolling |
| **repeat-mode** (built-in) | Repeat last command without prefix |
| avy | Jump to any visible char |
| rg | ripgrep integration |
| helpful | Better help buffers |
| restart-emacs | Restart from inside Emacs |
| gptel | AI chat via LM Studio |
| conda | Conda env management |
| exec-path-from-shell | Inherit shell PATH on macOS |

---

## Keybindings

### Leader (`SPC`)

#### Top-level

| Key | Action |
|-----|--------|
| `SPC SPC` | M-x |
| `SPC TAB` | last buffer |
| `SPC ;` | comment line |

#### Files (`SPC f`)

| Key | Action |
|-----|--------|
| `SPC f f` | find file |
| `SPC f r` | recent files |

#### Buffers (`SPC b`)

| Key | Action |
|-----|--------|
| `SPC b b` | switch buffer |
| `SPC b k` | kill buffer |

#### Windows (`SPC w`)

| Key | Action |
|-----|--------|
| `SPC w h/j/k/l` | navigate windows |
| `SPC w s` | split horizontal |
| `SPC w v` | split vertical |
| `SPC w d` | close window |
| `SPC w o` | close others |
| `SPC w =` | balance windows |
| `SPC w f` | toggle fullscreen |

#### Search (`SPC s`)

| Key | Action |
|-----|--------|
| `SPC s s` | search buffer |
| `SPC s r` | search project (ripgrep) |
| `SPC s i` | jump to symbol |

#### Code (`SPC c`)

| Key | Action |
|-----|--------|
| `SPC c f` | format buffer |
| `SPC c p` | run python file |
| `SPC c c` | compile & run c++ |

#### LSP — eglot (`SPC l`)

| Key | Action |
|-----|--------|
| `SPC l r` | rename symbol |
| `SPC l d` | find references |
| `SPC l a` | code action |
| `SPC l i` | find implementation |
| `SPC l f` | format buffer |

#### Errors — flymake (`SPC e`)

| Key | Action |
|-----|--------|
| `SPC e l` | list errors (consult-flymake) |
| `SPC e n` | next error |
| `SPC e p` | previous error |

#### Git (`SPC g`)

| Key | Action |
|-----|--------|
| `SPC g g` | magit status |
| `SPC g c` | commit |
| `SPC g p` | push |
| `SPC g i` | pull |
| `SPC g f` | fetch |
| `SPC g l` | log |
| `SPC g d` | diff |
| `SPC g s` | stage |
| `SPC g u` | unstage |
| `SPC g b` | branch |
| `SPC g a` | blame |
| `SPC g r` | rebase |
| `SPC g m` | merge |
| `SPC g t` | stash |

#### Project — project.el (`SPC p`)

| Key | Action |
|-----|--------|
| `SPC p p` | switch project |
| `SPC p f` | find file in project |
| `SPC p b` | switch project buffer |
| `SPC p k` | kill project buffers |
| `SPC p s` | project shell |
| `SPC p c` | compile in project |

#### Org / Notes (`SPC o`)

| Key | Action |
|-----|--------|
| `SPC o a` | agenda |
| `SPC o c` | capture |
| `SPC o r` | roam find node |
| `SPC o i` | roam insert link |
| `SPC o s` | roam search |
| `SPC o d` | deft (fast search) |
| `SPC o u` | roam graph (browser) |
| `SPC o l` | paste URL as org link |
| `SPC o t` | toggle transclusion mode |
| `SPC o y` | paste image from clipboard |

#### Learning OS (`SPC n`)

| Key | Action |
|-----|--------|
| `SPC n i` | open inbox |
| `SPC n k` | capture new concept |
| `SPC n q` | capture recall question |
| `SPC n v` | schedule review item |
| `SPC n p` | capture new project |
| `SPC n j` | write journal entry |
| `SPC n r` | open review session |
| `SPC n d` | mark current item reviewed |
| `SPC n s` | search all notes |
| `SPC n c` | open concepts file |
| `SPC n f` | open questions file |
| `SPC n t` | open projects file |

#### AI Assistant (`SPC a`)

| Key | Action |
|-----|--------|
| `SPC a c` | open AI chat |
| `SPC a s` | send prompt |
| `SPC a r` | rewrite selected region |
| `SPC a e` | AI menu |

#### Conda (`SPC m`)

| Key | Action |
|-----|--------|
| `SPC m a` | activate conda env |
| `SPC m d` | deactivate conda env |

#### Jump (`SPC j`)

| Key | Action |
|-----|--------|
| `SPC j j` | jump to char (avy) |

#### Help (`SPC h`)

| Key | Action |
|-----|--------|
| `SPC h k` | describe key |
| `SPC h f` | describe function |
| `SPC h v` | describe variable |
| `SPC h .` | help at point |

#### Quit (`SPC q`)

| Key | Action |
|-----|--------|
| `SPC q q` | quit emacs |
| `SPC q r` | restart emacs |

---

### LSP Navigation (normal mode)

| Key | Action |
|-----|--------|
| `gd` | go to definition |
| `gD` | go to declaration |
| `gr` | find references |
| `gi` | find implementation |
| `K` | hover docs |

### Vim Extras (normal mode)

| Key | Action |
|-----|--------|
| `j / k` | move by visual line |
| `gj / gk` | move by real line |
| `gh / gl` | start / end of line |
| `Y` | yank to end of line |
| `U` | redo |
| `Q` | replay macro |
| `n / N` | search next/prev + center |
| `* / #` | search word + center |
| `]h / [h` | next/prev git hunk |
| `]e / [e` | next/prev error |

### Visual Mode

| Key | Action |
|-----|--------|
| `> / <` | indent/dedent, keep selection |

### Inside Magit

| Key | Action |
|-----|--------|
| `s` | stage file/hunk |
| `u` | unstage |
| `c c` | commit |
| `P p` | push |
| `F p` | pull |
| `b b` | switch branch |
| `b c` | create branch |
| `l l` | log |
| `d d` | diff |
| `z z` | stash |
| `q` | quit magit |

---

## Formatters (auto on save)

| Language | Formatter |
|----------|-----------|
| Python | ruff |
| C / C++ | clang-format |
| JavaScript / TypeScript | prettier |
| CSS / HTML / JSON | prettier |
| Markdown | prettier |
| Shell | shfmt |
| Rust | rustfmt |
| Go | gofmt |

---

## Org Capture Templates

| Key | Template |
|-----|---------|
| `t` | Todo (general) |
| `n` | Note (general) |
| `j` | Journal entry (general) |
| `i` | Learning inbox |
| `k` | Atomic concept |
| `q` | Recall question |
| `v` | Review item (scheduled) |
| `P` | Project |
| `J` | Learning journal |

---

## Learning OS

A structured learning system inside Emacs. Forces the full loop:

```
Read/Watch → los inbox → los learn → los question → los review → los projects
```

### CLI (`los`)

Add to PATH:
```bash
echo 'export PATH="$HOME/Documents/garden/learn:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

| Command | Action |
|---------|--------|
| `los inbox` | Open raw input buffer |
| `los learn` | Capture new concept (What/Why/When/Code) |
| `los question` | Capture recall question |
| `los review` | Open today's review session |
| `los search <q>` | Search all notes |
| `los concepts` | Open concepts file |
| `los questions` | Open questions file |
| `los projects` | Open projects file |
| `los journal` | Write daily journal |
| `los stats` | Show counts |

### Spaced Repetition

`SPC n d` on a review item advances the schedule:

| Review # | Next interval |
|----------|--------------|
| 1 | 1 day |
| 2 | 3 days |
| 3 | 7 days |
| 4 | 14 days |
| 5 | 30 days |
| 6 | 60 days → MASTERED |

### TODO States

| State | Meaning |
|-------|---------|
| `TODO` | General task |
| `NEXT` | Next action |
| `DONE` | Completed |
| `NEW` | Just captured, not processed |
| `LEARNING` | Actively studying |
| `REVIEW` | In spaced repetition loop |
| `APPLY` | Building something with it |
| `MASTERED` | Recalled 6+ times |
| `DROPPED` | Abandoned |

---

## AI Assistant (gptel + LM Studio)

1. Download [LM Studio](https://lmstudio.ai), load a model, start local server (port 1234)
2. Update model name in `init.el` section 21:
   ```elisp
   :models '(your-model-name)
   gptel-model 'your-model-name
   ```
3. `SPC a c` to open chat, `SPC a s` to send

---

## Theme

`doom-one` (dark). Change in section 7:

```elisp
(load-theme 'doom-one t)  ; doom-dracula, doom-gruvbox, doom-nord, etc.
```

---

## Troubleshooting

**`gd` shows "Visit tags table"**: eglot not connected. Run `M-x eglot` manually. Verify LSP server installed: `which pyright-langserver` (Python) or `which clangd` (C/C++).

**LSP slow / not starting**: check `*EGLOT events*` buffer and `M-x eglot-list-connections`. Eglot auto-detects servers from `eglot-server-programs`.

**Packages not installing**: `M-x package-refresh-contents` then restart.

**Icons broken**: `M-x nerd-icons-install-fonts`.

**Tree-sitter grammars missing**: `M-x treesit-auto-install-all`.

**org-roam DB stale**: `M-x org-roam-db-sync`.

**gptel not connecting**: Make sure LM Studio local server is running on port 1234.
