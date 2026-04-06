# Emacs Setup Instructions

## Context

You are working inside my local development environment.

I want to build a **minimal, fast, and robust Emacs setup** for:

- Python (ML/AI workflows)
- C++ development
- PKM (notes + linking)
- Task management
- Git workflows

## Constraints

- Emacs 29+
- Prioritize performance and low startup time
- Avoid unnecessary packages
- Use `use-package`
- Lazy-load wherever possible
- Keep config modular and maintainable

---

## Step 1: Inspect Current Setup

- Check if Emacs is installed
- Check for existing config:
  - ~/.emacs.d/init.el
  - ~/.config/emacs/init.el

If config exists:

- Analyze it before modifying

If not:

- Create a clean config structure

---

## Step 2: Core Configuration

Set up:

- Sensible defaults (encoding, backups, autosave)
- Disable UI clutter (menu/tool/scroll bars if GUI)
- Startup optimizations (GC tuning, native comp)

---

## Step 3: Programming Support

### LSP

- Use `eglot` (preferred)
- Ensure it works for:
  - Python (pyright)
  - C++ (clangd)

### Autocomplete

- Use `corfu` (lightweight)

### Syntax Highlighting

- Enable tree-sitter

---

## Step 4: ML/AI Workflow

- Configure Org-babel for Python execution
- Ensure code blocks run correctly
- Optional: integrate Jupyter if needed

---

## Step 5: PKM System

Use Org mode:

- Clean note structure
- Tags and properties

Add:

- `org-roam` for linking notes
- Daily notes support

---

## Step 6: Task Management

- Configure TODO states:
  - TODO → IN-PROGRESS → DONE

- Enable:
  - Deadlines
  - Scheduling
  - Agenda view

---

## Step 7: Terminal

- Install and configure `vterm`

---

## Step 8: Git

- Install and configure Magit

---

## Step 9: Navigation & Search

- Install:
  - `vertico`
  - `orderless`
  - `consult`

- Enable fast search across:
  - Files
  - Notes
  - Projects

---

## Step 10: External Dependencies

Ensure installed:

- pyright
- clangd
- ripgrep
- cmake (for C++)

Install if missing.

---

## Step 11: Performance Optimization

- Lazy load all non-core packages
- Reduce startup time
- Avoid blocking operations

---

## Step 12: Validation

Verify:

- Emacs starts without errors
- LSP works for Python and C++
- Org-babel executes Python code
- org-roam creates and links notes
- Magit works
- vterm works

---

## Step 13: Output

After completing:

- Summarize changes
- Explain design decisions
- Mention trade-offs
- Report startup time

---

## Important

Do NOT:

- Dump a huge config at once
- Add unnecessary packages
- Ignore performance

Work iteratively and verify each step.
