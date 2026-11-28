# Emacs configuration

Built for Emacs 31.1. The coding side uses built-ins: Eglot, Flymake, project,
Magit, tree-sitter, completion-preview, which-key and icomplete. Doom One
provides the dark theme; Modus is the fallback.

The Org side is deliberately small. `~/knowledge` is a zettelkasten of numbered
notes; Org-roam indexes it into `~/.emacs.d/org-roam.db`. Notes are created by
hand and linked with `id:` links.

## Key groups

- `C-c k` — zettelkasten: find, backlinks, sync, graph, open the docs
- `C-c o` — Org clocking and link insertion
- `C-c c` — compile, run Python, Eglot, xref
- `C-c e` — Flymake diagnostics
- `C-c g` — Magit
- `C-c p` — project commands (same map as `C-x p`)
- `C-c v` — Conda
- `C-c w` — windows
- `C-c f r` — recent files; `C-c s i` — imenu; `C-c t t` — theme

`cheatsheet.org` lists every binding. `ORG-PKM.org` explains the layout, and
`VERIFICATION.org` records the checks.

## What was removed

Keys for project creation, capture, kanban boards, dashboards and the reading
queue are gone. They pointed at a tree layout that no longer exists. The EPUB
file association stays because it works on any file.

## Checks

```bash
./emacs-sync.sh verify
```

The suite checks the key contract, the workflow states, the Org-roam index in a
temporary tree, EPUB rendering, and that the docs teach only live keys.
`./emacs-sync.sh doctor` shows which optional external tools are installed.
