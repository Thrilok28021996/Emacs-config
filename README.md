# Emacs 30.2 Configuration

Vim-centric Emacs config with Evil mode, LSP, and org-roam. Single-file setup (`init.el`).

## Prerequisites

### Required

| Tool | Install | Purpose |
|------|---------|---------|
| Emacs 30.2+ | `brew install emacs-plus@30` | Editor |
| ripgrep | `brew install ripgrep` | Project-wide search (`SPC s r`) |
| Node.js | `brew install node` | Required by pyright, prettier |
| pyright | `npm install -g pyright` | Python LSP server |

### Per-language (install as needed)

| Tool | Install | Purpose |
|------|---------|---------|
| ruff | `brew install ruff` | Python formatter/linter |
| clangd | `brew install llvm` | C/C++ LSP server |
| clang-format | Included with llvm | C/C++ formatter |
| prettier | `npm install -g prettier` | JS/TS/HTML/CSS/JSON/Markdown formatter |
| shfmt | `brew install shfmt` | Shell script formatter |
| pandoc | `brew install pandoc` | Markdown preview |
| graphviz | `brew install graphviz` | org-roam graph (`dot` command) |
| miniconda | [miniconda3](https://docs.conda.io/) | Python env management |

### Fonts

- **Victor Mono** (primary, size 18) — `brew install --cask font-victor-mono`
- **Menlo** (fallback, size 12) — ships with macOS

### Directories (create before first launch)

```sh
mkdir -p ~/org ~/org-roam ~/projects ~/code
touch ~/org/inbox.org ~/org/notes.org ~/org/journal.org
```

## Installation

```sh
git clone <this-repo> ~/.emacs.d
emacs
```

First launch installs all packages automatically. Takes 2-3 minutes.

## Structure

Single file: `init.el` with 20 numbered sections.

| Section | What |
|---------|------|
| 0 | Frame (fullscreen) |
| 1 | Package bootstrap (MELPA, use-package) |
| 2 | PATH inheritance (macOS shell env) |
| 3 | Basic UI (no menu/toolbar, relative line numbers) |
| 4 | Evil mode + leader keybindings |
| 5 | Vim movement extras |
| 6 | Completion (vertico, orderless, consult, marginalia) |
| 7 | Theme (doom-one) + modeline |
| 8 | Editing (smartparens, rainbow-delimiters, company, flycheck, yasnippet) |
| 9 | LSP (lsp-mode, lsp-pyright, lsp-ui) |
| 10 | Tree-sitter (built-in treesit via treesit-auto) |
| 11 | IDE tools (dap, avy, rg, projectile, magit, diff-hl, helpful) |
| 12 | Org mode (org-roam, org-roam-ui, org-modern, org-super-agenda) |
| 13 | Markdown (markdown-mode, pandoc, deft) |
| 14 | Writing mode (olivetti — centered text) |
| 15 | Python / Conda |
| 16 | Compile & run helpers |
| 17 | Formatters (apheleia — async format on save) |
| 18 | Persistence (save-place, recentf, savehist) |
| 19 | Defaults (indent, scroll, backups) |
| 20 | Custom file redirect |

## Keybindings

Leader key: `SPC` (normal/visual mode). Press `SPC` and wait for which-key popup.

### Leader Keybindings (`SPC + ...`)

#### Top-level

| Key | Action |
|-----|--------|
| `SPC SPC` | M-x (command palette) |
| `SPC TAB` | switch to last buffer |
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
| `SPC w o` | close other windows |
| `SPC w =` | balance windows |
| `SPC w f` | toggle fullscreen |

#### Search (`SPC s`)

| Key | Action |
|-----|--------|
| `SPC s s` | search current buffer |
| `SPC s r` | ripgrep across project |
| `SPC s i` | jump to symbol (imenu) |

#### Code (`SPC c`)

| Key | Action |
|-----|--------|
| `SPC c f` | format buffer |
| `SPC c p` | run current Python file |
| `SPC c c` | compile & run current C++ file |

#### LSP (`SPC l`)

| Key | Action |
|-----|--------|
| `SPC l r` | rename symbol |
| `SPC l d` | find references |
| `SPC l a` | code action |
| `SPC l i` | find implementation |

#### Errors (`SPC e`)

| Key | Action |
|-----|--------|
| `SPC e l` | list all errors |

#### Git (`SPC g`)

| Key | Action |
|-----|--------|
| `SPC g g` | magit status |
| `SPC g c` | commit |
| `SPC g p` | push |
| `SPC g i` | pull |
| `SPC g f` | fetch |
| `SPC g b` | branch |
| `SPC g l` | log |
| `SPC g d` | diff |
| `SPC g s` | stage |
| `SPC g u` | unstage |
| `SPC g a` | blame |
| `SPC g r` | rebase |
| `SPC g m` | merge |
| `SPC g t` | stash |

#### Project (`SPC p`)

| Key | Action |
|-----|--------|
| `SPC p p` | switch project |
| `SPC p f` | find file in project |

#### Org / Notes (`SPC o`)

| Key | Action |
|-----|--------|
| `SPC o a` | org agenda |
| `SPC o c` | org capture |
| `SPC o r` | find org-roam note |
| `SPC o i` | insert org-roam link |
| `SPC o s` | search org-roam notes |
| `SPC o d` | deft (quick note search) |
| `SPC o u` | org-roam graph (browser) |

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

### LSP Navigation (no leader, normal mode)

| Key | Action |
|-----|--------|
| `gd` | go to definition |
| `gD` | go to declaration |
| `gr` | find references |
| `gi` | find implementation |
| `K` | hover documentation |
| `C-o` | jump back |
| `C-i` | jump forward |

### Vim Extras (no leader, normal mode)

| Key | Action |
|-----|--------|
| `j/k` | move by visual line |
| `gj/gk` | move by actual line |
| `gh` | beginning of line |
| `gl` | end of line |
| `Q` | replay last macro |
| `U` | redo |
| `Y` | yank to end of line |
| `n/N` | search next/prev (centered) |
| `*/\#` | search word forward/backward (centered) |
| `C-u/C-d` | scroll half page up/down |
| `]h/[h` | next/prev git hunk |
| `]e/[e` | next/prev error |
| `C-s` | search buffer (consult-line) |

### Visual Mode

| Key | Action |
|-----|--------|
| `>/<` | indent/dedent (keeps selection) |

### Evil Operators

| Key | Action |
|-----|--------|
| `gcc` | comment line |
| `gc{motion}` | comment region |
| `ys{motion}{char}` | add surround |
| `cs{old}{new}` | change surround |
| `ds{char}` | delete surround |
| `%` | jump to matching delimiter |

### Magit (inside magit status buffer)

| Key | Action |
|-----|--------|
| `s` | stage hunk/file |
| `S` | stage all |
| `u` | unstage hunk/file |
| `c c` | create commit |
| `C-c C-c` | confirm commit message |
| `C-c C-k` | cancel commit |
| `P p` | push |
| `F p` | pull |
| `b c` | create branch |
| `TAB` | expand/collapse diff |
| `g` | refresh |
| `q` | quit |
| `$` | show git command log |
| `?` | show all options |

## Formatters (auto-format on save)

| Language | Formatter |
|----------|-----------|
| Python | ruff |
| C/C++ | clang-format |
| JS/TS/HTML/CSS/JSON/Markdown | prettier |
| Shell | shfmt |
| Rust | rustfmt |
| Go | gofmt |

## Org Capture Templates

| Key | Template |
|-----|----------|
| `t` | Todo → `~/org/inbox.org` |
| `n` | Note → `~/org/notes.org` |
| `j` | Journal → `~/org/journal.org` (datetree) |

Trigger with `SPC o c`, select template letter.

## Debugging (DAP)

DAP mode loads after lsp-mode. Use `M-x dap-debug` to start a debug session.

## Theme

**doom-one** (dark). Change in section 7:

```elisp
(load-theme 'doom-one t)  ; replace with doom-dracula, doom-gruvbox, etc.
```

Available themes: `M-x load-theme TAB` to browse.

## Troubleshooting

**LSP not starting**: Run `M-x lsp` manually. Check `*lsp-log*` buffer for errors.

**gd shows "Visit tags table"**: LSP isn't connected. Verify with `M-x lsp-describe-session`.

**Packages not installing**: Run `M-x package-refresh-contents`, then restart.

**Icons broken**: Run `M-x nerd-icons-install-fonts`.

**Tree-sitter grammars missing**: Run `M-x treesit-auto-install-all`.
