# Emacs 30.2 Configuration

Pure-Emacs config (no Evil) with `C-c` keybindings, built-in `eglot` LSP,
org-roam PKM, local AI via gptel, and a lightweight spaced-repetition
learning loop. Single file: `init.el`.

---

## Prerequisites

### Required

| Tool | Install | Purpose |
|------|---------|---------|
| Emacs 30.2+ | `brew install emacs-plus@30` | Editor |
| ripgrep | `brew install ripgrep` | Project / note search (consult-ripgrep) |
| Node.js | `brew install node` | Several LSP servers + prettier |
| Python language server | `npm install -g pyright` (or `pip install basedpyright`) | Python LSP — eglot auto-detects |

### Per-language (optional, install what you use)

| Tool | Install | Purpose |
|------|---------|---------|
| ruff | `brew install ruff` | Python formatter (apheleia) |
| clangd | `brew install llvm` | C/C++ LSP |
| clang-format | included with llvm | C/C++ formatter |
| prettier | `npm install -g prettier` | JS/TS/CSS/HTML/JSON/Markdown formatter |
| shfmt | `brew install shfmt` | Shell formatter |
| rustfmt | `rustup component add rustfmt` | Rust formatter |
| gofmt | included with Go | Go formatter |
| pandoc | `brew install pandoc` | Markdown export (`markdown-command`) |
| grip | `pip install grip` | Live GitHub-flavored Markdown preview (grip-mode) |
| graphviz | `brew install graphviz` | org-roam-ui graph |
| enchant + dict | `brew install enchant` | Spell-check backend (jinx) |
| miniconda | [docs.conda.io](https://docs.conda.io) | Python env management |
| LM Studio | [lmstudio.ai](https://lmstudio.ai) | Local AI assistant (gptel) |

### Fonts

Install **Victor Mono** (preferred); falls back to **Menlo**. Run
`M-x nerd-icons-install-fonts` once for modeline icons.

### Directories

The config expects a single notes root at `~/Documents/garden/`:

```bash
mkdir -p ~/Documents/garden/{work,personal}/projects
mkdir -p ~/Documents/garden/{images,people}
```

---

## Installation

```bash
git clone <repo> ~/.emacs.d
emacs --daemon          # start the server (first launch installs packages)
```

Add to `~/.zshrc`:

```bash
emacs --daemon 2>/dev/null &
alias e='emacsclient -nw'      # terminal
alias ec='emacsclient -c'      # new GUI frame
```

---

## Folder Structure

All notes live under one root. org-roam indexes the whole tree recursively.

```
~/Documents/garden/
├── inbox.org          ← quick-capture todos        (C-c n i)
├── journal.org        ← daily datetree journal      (C-c n j)
├── reading.org        ← books / resources           (C-c n r)
├── reviews.org        ← spaced-repetition schedule   (C-c n v)
├── *.org              ← org-roam nodes (Zettelkasten)
├── images/            ← org-download attachments
├── people/            ← org-roam person nodes        (C-c n P)
├── work/
│   ├── tasks.org      ← work tasks                   (C-c n w)
│   ├── projects.org   ← work projects                (C-c n W)
│   └── projects/      ← per-project org files (auto-added to agenda)
└── personal/
    ├── tasks.org      ← personal tasks               (C-c n t)
    ├── projects.org   ← personal projects            (C-c n p)
    └── projects/      ← per-project org files (auto-added to agenda)
```

---

## Packages

### Completion & Minibuffer

| Package | Purpose |
|---------|---------|
| vertico | Vertical minibuffer completion UI |
| orderless | Space-separated / fuzzy matching |
| marginalia | Rich annotations in completion lists |
| consult | Search, buffer switch, imenu, ripgrep, flymake |
| corfu | In-buffer popup autocomplete |
| cape | Extra completion sources (file, dabbrev, keyword, elisp) |
| embark / embark-consult | Act on completion candidates (`C-.`) |
| wgrep | Editable grep buffers for project-wide replace |
| consult-dir | Fast directory jumping |
| consult-todo | Jump to TODO/FIXME across project |

### UI

| Package | Purpose |
|---------|---------|
| **tab-bar** (built-in) | Tab workspaces |
| doom-themes | `doom-one` dark theme |
| doom-modeline | Status bar with icons |
| nerd-icons | Icon font |

### Editing

| Package | Purpose |
|---------|---------|
| **electric-pair-mode** (built-in) | Auto-pair brackets |
| rainbow-delimiters | Color-coded bracket depth |
| yasnippet / yasnippet-snippets | Code snippets |
| ws-butler | Strip trailing whitespace (only on touched lines) |
| highlight-indent-guides | Indent level lines |
| ligature | Programming ligatures (GUI only) |
| hl-todo | Highlight TODO/FIXME/HACK keywords |
| expand-region | Grow selection semantically (`C-=`) |
| symbol-overlay | Highlight & navigate symbol occurrences |

### LSP & Languages

| Package | Purpose |
|---------|---------|
| **eglot** (built-in) | LSP client — auto-detects clangd, pyright |
| **flymake** (built-in) | Inline diagnostics |
| treesit-auto | Auto-install tree-sitter grammars |
| apheleia | Async format-on-save |
| dape | Debug Adapter Protocol client |
| conda | Conda env management |

### Git

| Package | Purpose |
|---------|---------|
| magit | Full git UI |
| diff-hl | Gutter diff indicators |

### Org / PKM

| Package | Purpose |
|---------|---------|
| **org** (built-in) | Org mode |
| org-roam | Linked notes (Zettelkasten), native sqlite |
| org-roam-ui | Visual graph of notes (browser) |
| consult-org-roam | Ripgrep search across roam |
| org-super-agenda | Grouped agenda view |
| org-modern | Modern org visual style |
| org-pomodoro | Pomodoro timer tied to clock |
| org-appear | Reveal emphasis markers under cursor |
| org-download | Paste/drag images into org |
| org-transclusion | Embed content from other org files |
| org-cliplink | Paste URL, auto-fetch title |

### Markdown & Writing

| Package | Purpose |
|---------|---------|
| markdown-mode | Markdown editing, native code-block fontification |
| grip-mode | Live GitHub-flavored preview in browser |
| olivetti | Centered writing mode |
| jinx | Fast spell-check (enchant) |

### Tools

| Package | Purpose |
|---------|---------|
| **project.el** (built-in) | Project management |
| **which-key** (built-in) | Key hints after a short delay |
| **pixel-scroll-precision-mode** (built-in) | Smooth scrolling |
| **repeat-mode** (built-in) | Repeat commands without prefix |
| avy | Jump to any visible char (`C-c j`) |
| helpful | Better help buffers |
| vundo | Visual undo tree (`C-x u`) |
| restart-emacs | Restart from inside Emacs |
| gptel | AI chat via local LM Studio |
| exec-path-from-shell | Inherit shell PATH on macOS |

---

## Keybindings

All custom bindings live under the `C-c` prefix, grouped by letter. Press a
prefix (e.g. `C-c g`) and wait — **which-key** shows the rest.

### Top-level

| Key | Action |
|-----|--------|
| `M-o` | other window |
| `C-s` | search line (consult-line) |
| `C-.` | embark act on candidate |
| `C-=` | expand region |
| `C-x u` | visual undo tree (vundo) |
| `C-?` | redo (undo-redo) |
| `C-x C-d` | jump to directory (consult-dir) |
| `M-$` | spell-correct word (jinx) |
| `C-c i r` / `C-c i l` | indent region right / left |

### Buffers (`C-c b`)

| Key | Action |
|-----|--------|
| `C-c b b` | switch buffer |
| `C-c b k` | kill current buffer |
| `C-c b TAB` | last buffer |

### Code (`C-c c`)

| Key | Action |
|-----|--------|
| `C-c c r` | rename symbol (eglot) |
| `C-c c a` | code action (eglot) |
| `C-c c i` | find implementation |
| `C-c c d` | find references |
| `C-c c f` | format buffer (apheleia) |
| `C-c c p` | run current Python file |
| `C-c c c` | compile & run current C++ file |
| `C-c c b` | toggle breakpoint (dape) |
| `C-c c B` | start debugger (dape) |

Native xref also works everywhere: `M-.` definition, `M-,` back, `M-?` references.

### Errors (`C-c e`)

| Key | Action |
|-----|--------|
| `C-c e l` | list errors (consult-flymake) |
| `C-c e n` / `C-c e p` | next / previous error |

### Files & Search

| Key | Action |
|-----|--------|
| `C-c f r` | recent files |
| `C-c s s` | search lines in buffer |
| `C-c s r` | search project (ripgrep) |
| `C-c s i` | jump to symbol (imenu) |
| `C-c s t` | jump to TODO |

### Git (`C-c g`)

| Key | Action |
|-----|--------|
| `C-c g g` | magit status |
| `C-c g c` | commit |
| `C-c g p` | push |
| `C-c g u` | pull |
| `C-c g f` | fetch |
| `C-c g l` | log |
| `C-c g d` | diff |
| `C-c g b` | branch |
| `C-c g a` | blame |
| `C-c g s` | stash |
| `C-c g n` / `C-c g N` | next / previous hunk |

### Projects (`C-c p`)

| Key | Action |
|-----|--------|
| `C-c p p` | switch project |
| `C-c p f` | find file in project |
| `C-c p b` | switch project buffer |
| `C-c p k` | kill project buffers |
| `C-c p s` | project eshell |
| `C-c p c` | compile in project |

### Windows (`C-c w`)

| Key | Action |
|-----|--------|
| `C-c w h/j/k/l` | move to window left/down/up/right |
| `C-c w H/J/K/L` | resize window |
| `C-c w u` / `C-c w U` | winner undo / redo |
| `C-c w =` | balance windows |
| `C-c w f` | toggle fullscreen |

Native splits: `C-x 2` (below), `C-x 3` (right), `C-x 0` (close), `C-x 1` (only).

### Org (`C-c o`)

| Key | Action |
|-----|--------|
| `C-c o a` | agenda |
| `C-c o c` | capture |
| `C-c o r` | roam find node |
| `C-c o i` | roam insert link |
| `C-c o s` | roam search |
| `C-c o u` | roam graph (browser) |
| `C-c o l` | paste URL as org link |
| `C-c o t` | toggle transclusion mode |
| `C-c o y` | paste image from clipboard |
| `C-c o I` / `C-c o O` | clock in / out |
| `C-c o R` | clock report |
| `C-c o e` | set effort estimate |

### Notes & Learning (`C-c n`)

| Key | Action |
|-----|--------|
| `C-c n i` | capture → inbox |
| `C-c n j` | capture → journal |
| `C-c n r` | capture → reading |
| `C-c n v` | capture → review item |
| `C-c n w` / `C-c n W` | capture → work task / project |
| `C-c n t` / `C-c n p` | capture → personal task / project |
| `C-c n k` | new roam concept node |
| `C-c n q` | new roam question node |
| `C-c n P` | new roam person node |
| `C-c n x` | log interaction with a person |
| `C-c n a` | open today's review session |
| `C-c n d` | mark current item reviewed (advance schedule) |
| `C-c n s` | search all notes |

### AI (`C-c a`)

| Key | Action |
|-----|--------|
| `C-c a c` | open AI chat |
| `C-c a s` | send prompt |
| `C-c a r` | rewrite region |
| `C-c a m` | gptel menu |

### Virtual env (`C-c v`)

| Key | Action |
|-----|--------|
| `C-c v a` / `C-c v d` | activate / deactivate conda env |

### Help (`C-c h`) & Quit (`C-c q`)

| Key | Action |
|-----|--------|
| `C-c h k` / `C-c h f` / `C-c h v` / `C-c h .` | describe key / function / variable / at-point |
| `C-c q q` / `C-c q r` | quit / restart emacs |
| `C-c j` | jump to char (avy) |

### Inside Magit

| Key | Action |
|-----|--------|
| `s` / `u` | stage / unstage |
| `c c` | commit |
| `P p` / `F p` | push / pull |
| `b b` / `b c` | switch / create branch |
| `l l` | log |
| `d d` | diff |
| `z z` | stash |
| `q` | quit magit |

---

## Formatters (auto on save — apheleia)

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

Both classic (`python-mode`) and tree-sitter (`python-ts-mode`) major modes are
mapped, so formatting fires regardless of which mode treesit-auto selects.

---

## Org Capture Templates (`C-c o c`)

| Key | Template | Destination |
|-----|----------|-------------|
| `i` | Inbox todo | `inbox.org` |
| `j` | Journal entry (datetree) | `journal.org` |
| `v` | Review item (scheduled) | `reviews.org` |
| `r` | Reading note | `reading.org` |
| `w` | Work task | `work/tasks.org` |
| `W` | Work project | `work/projects.org` |
| `t` | Personal task | `personal/tasks.org` |
| `p` | Personal project | `personal/projects.org` |

### Roam capture nodes

| Command | Node type |
|---------|-----------|
| `C-c n k` | Concept (What / Why / When / Code / Links) |
| `C-c n q` | Question (Question / Answer / Related) |
| `C-c n P` | Person (`people/`, interactions log) |
| `C-c o i` | default node (insert link) |

---

## Spaced Repetition

`C-c n d` on a review item advances its schedule via the `REVIEW_COUNT` property:

| Review # | Next interval |
|----------|--------------|
| 1 | 1 day |
| 2 | 3 days |
| 3 | 7 days |
| 4 | 14 days |
| 5 | 30 days |
| 6 | 60 days → **MASTERED** |

### TODO States

| State | Meaning |
|-------|---------|
| `TODO` / `NEXT` / `DONE` | General task flow |
| `NEW` | Just captured, not processed |
| `LEARNING` | Actively studying |
| `REVIEW` | In spaced-repetition loop |
| `APPLY` | Building something with it |
| `MASTERED` | Recalled 6+ times |
| `DROPPED` | Abandoned |

---

## AI Assistant (gptel + LM Studio)

1. Download [LM Studio](https://lmstudio.ai), load a model, start the local
   server (port 1234).
2. Update the model name in `init.el` (section 21, the `gptel` block):
   ```elisp
   :models '(your-model-name)
   gptel-model 'your-model-name
   ```
3. `C-c a c` opens chat, `C-c a s` sends.

---

## Theme

`doom-one` (dark). Change in the `doom-themes` block (section 6):

```elisp
(load-theme 'doom-one t)  ; doom-dracula, doom-gruvbox, doom-nord, etc.
```

---

## Troubleshooting

**LSP not connecting**: run `M-x eglot` manually. Verify the server is on PATH:
`which clangd` (C/C++) or `which pyright-langserver` (Python). Inspect with
`M-x eglot-list-connections`.

**Packages not installing**: `M-x package-refresh-contents`, then restart.

**Icons broken**: `M-x nerd-icons-install-fonts`.

**Tree-sitter grammars missing**: `M-x treesit-auto-install-all`, or accept the
prompt when opening a file. JSON grammar for Markdown code blocks:
`M-x treesit-install-language-grammar RET json`.

**org-roam DB stale**: `M-x org-roam-db-sync`.

**Refile targets out of date** (after adding files): `C-0 C-c C-w` clears the
refile cache.

**gptel not connecting**: ensure the LM Studio local server is running on port 1234.
