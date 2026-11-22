# Emacs 31.1 hybrid configuration

This configuration uses a small hybrid: built-in Emacs features for startup-critical workflows, plus Doom themes and Org-roam where they provide capabilities that have no equally good built-in replacement.

## Built-in features used

- `eglot` + `flymake` for LSP and diagnostics
- `project.el`, `xref`, `compile`, and `winner`
- Magit for Git, loaded lazily on the first Git command
- Built-in Modus themes, with Doom themes preferred when installed
- Org-roam linked notes and SQLite index, loaded after startup
- `fido-vertical-mode`, `completion-preview-mode`, `imenu`, and `recentf`
- `org`, agenda, capture, clocking, and built-in SQLite support
- `which-key`, `tab-bar`, `repeat`, `savehist`, `save-place`, and `autorevert`

## Selected external packages

- `doom-themes`: retained because the original configuration explicitly used `doom-one`; it is loaded at startup for the requested appearance.
- `org-roam`: retained because the existing `org-roam.db` and original bindings show that linked-note navigation is intentional. It loads after an idle delay and uses the Emacs 31 SQLite module.
- `conda.el`: retained because your Python environments live in Conda and Emacs has no equivalent built-in environment switcher.
- `magit`: retained for the interactive Git workflow; only its autoloads are loaded during startup.

The configuration deliberately does not restore Vertico, Corfu, Consult, or formatting frameworks: their extra capabilities are useful, but the built-in alternatives are lighter and already cover the current workflow.

## External tools

These are optional executables, not Emacs plugins:

- `rg` for project search from shell commands
- `clangd` or a Python language server for Eglot
- `clang-format`, `ruff`, or other formatters when invoked manually
- `pandoc`, `make`, `cmake`, and language runtimes as needed

The config does not install or manage any of them.

## Files

- `early-init.el`: startup GC, file-handler, package-startup suppression, and frame defaults
- `init.el`: the complete built-in-only configuration
- `emacs-sync.sh`: compatibility helper for checking the configuration; it no longer installs packages

## Notes directory

Org capture uses `~/Documents/garden/` and these files when needed:

- `inbox.org`
- `journal.org`
- `reading.org`
- `work/tasks.org`
- `personal/tasks.org`

Create the directories once:

    mkdir -p ~/Documents/garden/{work,personal}

## Useful keys

- `C-x p p`: switch project
- `C-c p`: project command map
- `C-c f r`: recent files
- `C-c s s`: search in the current buffer
- `C-c s i`: imenu
- `C-c c r`: rename with Eglot
- `C-c c a`: Eglot code actions
- `C-c c f`: format with Eglot
- `C-c c p`: run the current Python file
- `C-c g g`: Magit status
- `C-c g c`: commit
- `C-c g p` / `C-c g u`: push / pull
- `C-c g l`: log
- `C-c g d`: diff
- `C-c t t`: toggle the Doom dark/light theme
- `C-c e n` / `C-c e p`: next / previous Flymake diagnostic
- `C-c o a`: agenda
- `C-c o c`: capture
- `C-c n i`: inbox capture
- `C-c w u` / `C-c w U`: window undo / redo

## Verification

Load without opening a GUI frame:

    emacs --batch -Q --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' -l early-init.el -l init.el

Compile the configuration:

    emacs --batch -Q --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' -l early-init.el --eval '(byte-compile-file "init.el")'

Measure startup:

    emacs --batch -Q --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' -l early-init.el --eval '(let ((start (float-time))) (load "init.el") (princ (format "load-seconds=%.3f\\n" (- (float-time) start))))'

## Org knowledge & productivity system

This configuration also runs a plain-text knowledge system in `~/knowledge/`:
projects, learning subjects, permanent notes, Kanban boards, dashboards, reading
and a local git history. It is loaded from `lisp/` by a single block at the end
of `init.el`, so the coding setup above is untouched.

- Architecture and keys: `ORG-PKM.org` (or `C-c k H` in Emacs)
- Evidence for every acceptance criterion: `VERIFICATION.org`
- Every binding this configuration defines: `C-c ?`
- Gates: `./emacs-sync.sh verify` (check + 128 tests + benchmark)
- Data: `~/knowledge/README.org`

The old `cheatsheet.org` and `tutorial.org` describe the previous configuration
and now open with a correction banner.
