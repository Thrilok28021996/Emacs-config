# Plan — Org knowledge & productivity system (Stage 1)

Approved scope for this branch. Later stages get their own plan update.

## Intent

A separate, plain-text Org system at `~/knowledge`, wired into the existing
built-in-only, startup-fast Emacs config. Terminal Emacs only. The Markdown
garden at `~/Documents/garden` stays untouched and is not migrated.

## Decisions taken with the user

| # | Decision |
|---|---|
| 1 | Global 8 states: `BACKLOG(b) TODO(t) NEXT(n) DOING(o) WAITING(w) REVIEW(r) \| DONE(d) CANCELLED(c)` |
| 2 | `WAITING` blocks subtasks (Emacs default, documented, not hacked around) |
| 3 | Logging: `CLOSED:` on DONE/CANCELLED only; no per-transition logging |
| 4 | Recurring: plain `+1w` repeaters; no `org-habit` |
| 5 | Dependencies: parent/child + `ORDERED` property; no `org-depend` (removed from Emacs 31) |
| 6 | Config split into `lisp/my-*.el`, loaded from one block at the end of `init.el` |
| 7 | `~/knowledge` is local git only: no remote, no push |
| 8 | `ORG-PKM.org` is authoritative docs; `C-c ?` builds a keymap buffer from live keymaps |
| 9 | No example project files; templates only; harness uses a `/tmp` fixture |
| 10 | Backup: `backup/pre-org-2026-11-22` branch + tarball in `~/.emacs.d-backups/` |
| 11 | Pending config work committed untouched first (`5824f8f`) |
| 12 | Branch `agent/org-knowledge-s1-1122`, never `main` |
| 13 | Kanban keeps all 7 columns; width measured and reported, not silently narrowed |
| 14 | PDF, `pdf-tools`, `org-noter` removed from scope entirely |
| 15 | `org-fc` deferred to a separate later pass |
| 16 | Startup gate: batch load ≤ 0.25 s (baseline 0.191 s) |

## Out of scope for Stage 1

Kanban, org-roam, dashboard content, `nov.el`, `org-fc`, git remote,
migration from garden.

## Stage 5 deliverables

- `lisp/my-reading.el` — nov.el for EPUB (autoloads only at startup),
  `org-attach` into `~/knowledge/attachments/`, STATUS/FINISHED properties,
  reading queue, keys `C-c k A/o/R/S`.
- `lisp/tests/make-test-epub.sh` — builds a real EPUB 2 fixture, so the reader
  is tested against a file, not a mock.
- `lisp/tests/stage5-check.el` — 20 checks: nov-mode activation, metadata,
  spine, rendered chapter text, attachment placement, status transitions,
  queue entry, and the dashboard reading block.
- `my-tasks` gained a `:files` parameter so a block can query directories that
  are not agenda files.

Out of scope by decision: PDF, pdf-tools, org-noter. nov.el has no annotation
feature at all, so "annotations attached to the book" is not implementable and
is not faked.

Measured: startup 0.150s (0.164s with nov loaded eagerly).

## Stage 4 deliverables

- `lisp/my-dashboards.el` — three generated block types (`my-tasks`,
  `project-health`, `learning-progress`) driven by org-ql queries, and three
  agenda dashboards (`C-c k D/P/L`) grouped with org-super-agenda.
- Dashboard files carry the blocks; content is computed at refresh, never typed.
- `my/refresh-dblocks-in-file` / `my/refresh-all-dblocks` in `my-org-pkm.el`
  replace the kanban-only refresher; `C-c k b` refreshes everything.
- `lisp/tests/stage4-check.el` — 21 headless checks: table counts against
  fixtures, overdue detection, missing-NEXT detection, progress percentages,
  and all three agenda dashboards building with the right contents.

Deferred load: org-ql and org-super-agenda are required on first use, not at
startup (0.178s -> 0.138s). Measured table width: 106 chars for the project
health table, 87 for the others.

## Stage 3 deliverables

- `lisp/my-roam.el` — Org-roam against `~/knowledge/`, index at
  `~/.emacs.d/org-roam.db`, dashboards/templates/archive/attachments excluded.
- File-level IDs: templates start with a `:PROPERTIES: :ID: {{ID}} :END:`
  drawer and `my/knowledge-create-file` fills in a fresh UUID.
- `my/roam-sync`, `my/roam-ensure-file-id`, `my/roam-insert-node`,
  `my/roam-find-node`, `my/roam-toggle-buffer`, `my/roam-open-ui`;
  keys `C-c k f/v/s/g`.
- `lisp/tests/stage3-check.el` — 21 headless checks, including deleting the
  database, rebuilding it, and comparing node counts, link counts and the md5
  of every knowledge file.

The garden-era `my/org-roam-enable` idle timer was removed from `init.el`: it
re-set `org-roam-directory` to `~/Documents/garden/` five seconds after startup.
The old index was moved to `~/.emacs.d/org-roam.db.pre-knowledge.bak`.

## Stage 2 deliverables

- `lisp/my-kanban.el` — board params, refresh commands, `C-c k b` / `C-c k B`.
- Per-file boards in the project and learning templates, with
  `:skip-columns ("CANCELLED")`.
- Consolidated boards in `dashboards/projects.org` (projects + learning) and
  `dashboards/learning.org` (learning only), scoped to directories.
- `lisp/tests/stage2-check.el` — 14 headless checks, including a move through
  the consolidated board that must edit the right task in the right file.
- Fix: org-kanban resolves a row to a neighbouring heading depending on the
  target buffer's point. Overridden with a deterministic resolver that refuses
  to guess.

Measured board width: 305 chars project board, 324 chars consolidated. Does not
fit a terminal; the one-line `:range` knob is documented.

## Stage 1 deliverables

```
~/knowledge/
├── inbox.org
├── notes/projects/
├── notes/learning/
├── notes/knowledge/{artificial-intelligence,software-engineering,business,economics,productivity}/
├── notes/references/
├── notes/daily/
├── dashboards/{home,projects,learning}.org
├── attachments/
├── archive/
├── templates/{project,learning,knowledge-note,reference}.org
├── README.org
└── .gitignore
```

Config:

- `lisp/my-org-pkm.el` — directory, 8-state workflow, priorities, archive,
  refile, clock, agenda discovery, capture templates, `my/new-project`,
  `my/new-learning-subject`.
- `lisp/my-help.el` — `C-c ?` keymap listing derived from live keymaps.
- `init.el` — one added load block at the end; nothing else changes.

## Verification

1. `./emacs-sync.sh check` → `CONFIG_OK`
2. `./emacs-sync.sh benchmark` → ≤ 0.25 s
3. Headless harness `lisp/tests/stage1-check.el` asserting:
   - the 8 TODO states exist, in order
   - `my/new-project` creates a file carrying its `#+TODO:` line
   - agenda discovery picks the new file up without any config edit
   - every capture template target resolves
   - fixture removed afterwards
4. `C-c ?` opens a buffer listing the new bindings.

## Risks

- Seven-column boards will overflow a terminal; measured in Stage 2 with a
  concrete number and one-line tunable `:range`.
- Org-roam autosync over `~/knowledge` adds idle work; measured in Stage 3.
