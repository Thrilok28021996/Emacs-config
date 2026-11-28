# Prune unnecessary keybindings

## Intent

Remove keybindings and code that no longer work with the current zettelkasten-only
layout, delete the shadowed help-listing and line-moving chords, and bring the
docs and stage checks into line. Keep every binding that still works. Change
nothing outside `~/.emacs.d`.

## Acceptance

- `emacs-sync.sh verify` exits 0, or the remaining failure is reported rather
  than claimed green.
- No binding or command in the config points into the old notes or templates
  tree.
- No document in the repo teaches a removed key.
- The surviving prefixes stay readable: `C-c k` is zettelkasten only, `C-c o`
  is Org/linking/clocking, and `C-c c/e/g/p/v/w` is code/errors/git/projects/
  environments/windows.
- No file outside this repository is edited.

## Decisions

- Delete stale keys; do not repoint them at `~/projects`.
- Delete the shadowed help-listing and line-moving chords even though they are
  useful.
- Delete the three redundant aliases that duplicate built-in commands.
- Keep working keys, including `C-c k f/v/g/s/H`, `C-c o i/I/O/R`, all coding,
  Magit, project, window, conda and theme keys.
- Remove stale commands and their check cases so the gates describe what the
  config still does. Do not weaken any check for retained behavior.

## Sequence

1. Isolate a branch from `origin/main`.
2. Remove the stale, redundant and shadowed bindings.
3. Shrink `lisp/my-org-pkm.el` to layout and the 8-state workflow; reduce
   `lisp/my-help.el` to the doc opener; delete `lisp/my-dashboards.el` and
   `lisp/my-kanban.el`; keep only EPUB routing in `lisp/my-reading.el` because
   it still works on any file; drop stale loads and settings.
4. Delete `stage2/4/6-check.el`; rewrite `stage1` as the exact key contract,
   `stage3` against a temporary Org-roam tree, `stage5` as the EPUB check, and
   `stage7` doc checks; update `emacs-sync.sh`.
5. Update `cheatsheet.org`, `tutorial.org`, `ORG-PKM.org`, `README.md`,
   `VERIFICATION.org` — remove every removed key and generated key table, and
   describe the surviving prefix scheme.
6. Run `emacs-sync.sh verify`, report honestly, and present the diff for review.

## Risks

- Project creation, capture, dashboards and reading workflows disappear from
  Emacs until a later task reintroduces them against `~/projects`.
- Dynamic blocks already in `~/projects/dashboards/` will not refresh.
- Python Eglot bindings stay but no Python LSP server is installed.
