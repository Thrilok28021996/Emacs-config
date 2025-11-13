# Org-Mode Templates Guide

## 📚 Quick Access

Use `SPC o c` (or `M-x org-capture`) to access templates.

## Available Templates

### 📖 Learning Templates (prefix: `l`)

#### `lc` - Course Template
**Use for:** Online courses, video series, boot camps
**Includes:**
- Course metadata (platform, instructor, duration)
- Module breakdown with notes
- Assignment tracking
- Progress monitoring
- Key takeaways

**Example:** Coursera course, Udemy tutorial, YouTube series

---

#### `le` - Learning Extract
**Use for:** Extract key learnings from any source
**Includes:**
- Source information
- Core concepts with examples
- Code snippets (with syntax highlighting)
- Personal insights & connections
- Action items

**Example:** Book summary, article notes, video takeaways

---

#### `lt` - Tutorial Template
**Use for:** Following step-by-step guides
**Includes:**
- Setup & prerequisites
- Step-by-step instructions
- Code examples
- Troubleshooting
- Practice exercises

**Example:** Programming tutorial, how-to guide

---

#### `ln` - Universal Note
**Use for:** Quick notes that don't fit other categories
**Includes:**
- Context & category
- Main content
- Links to related notes
- Action items

**Example:** Meeting notes, quick ideas, references

---

### 💻 Project Template (key: `p`)
**Use for:** Software development projects
**Includes:**
- Technical stack & architecture
- Requirements & milestones
- Task breakdown
- Issue tracking
- Testing & deployment
- Lessons learned

**Example:** Side project, open source contribution

---

### 📅 Weekly Flow (key: `f`)
**Use for:** Weekly planning & review
**Includes:**
- Top 3 weekly goals
- Task prioritization
- Learning objectives
- Daily breakdown
- Time allocation
- Progress metrics
- Work-life balance

**Example:** Weekly planning, productivity tracking

---

### ✍️ Writing Templates (prefix: `w`)

- `wi` - Idea capture
- `wa` - Article draft
- `wj` - Journal entry
- `wr` - Research note
- `wq` - Quote collection

---

### 📋 Basic Templates

- `t` - Quick task
- `n` - Quick note
- `m` - Meeting notes

---

## File Organization

Templates save to:
```
~/Documents/org/
├── learning/
│   ├── courses.org     # Course notes
│   ├── extracts.org    # Learning extracts
│   ├── tutorials.org   # Tutorials
│   └── notes.org       # Universal notes
├── projects/
│   └── projects.org    # Projects
└── planning/
    └── weekly.org      # Weekly flows
```

## Usage Examples

### Capture a Course
```
1. SPC o c (or M-x org-capture)
2. Press l (Learning Templates)
3. Press c (Course)
4. Fill in course details
5. C-c C-c to save
```

### Extract Learning from Article
```
1. SPC o c
2. Press l then e (Learning Extract)
3. Add source & key concepts
4. C-c C-c to save
```

### Plan Your Week
```
1. SPC o c
2. Press f (Weekly Flow)
3. Set top 3 goals
4. Break into daily tasks
5. C-c C-c to save
```

## Tips & Tricks

### Checkboxes
Track progress with checkboxes:
```org
- [ ] Task 1
- [X] Task 2 (completed)
- [ ] Task 3
```

### Execute Code
Execute code blocks with `C-c C-c`:
```org
#+begin_src python
print("Hello, World!")
#+end_src
```

### Link Notes
Link between notes:
```org
[[file:~/Documents/org/learning/courses.org][My Courses]]
```

### Schedule Reviews
Set review dates:
```
C-c C-s (schedule)
C-c C-d (deadline)
```

### Tags
Add tags for organization:
```org
* Note Title :tag1:tag2:learning:
```

## Keybindings

| Key | Action |
|-----|--------|
| `SPC o c` | org-capture |
| `SPC o o` | org-agenda |
| `C-c C-c` | Finish capture |
| `C-c C-k` | Abort capture |
| `C-c C-s` | Schedule |
| `C-c C-d` | Deadline |

## Org-roam Integration

For knowledge base (org-roam):
```
SPC n f - Find/create roam note
SPC n i - Insert roam link
```

Org-roam templates:
- `d` - Default note
- `c` - Course note
- `l` - Learning extract
- `t` - Tutorial
- `n` - Universal note

## Template Location

Templates stored at:
```
~/.emacs.d/templates/notes/
```

Files:
- `course-template.org`
- `learning-extract.org`
- `project-template.org`
- `tutorial-template.org`
- `universal-note.org`
- `weekly-flow.org`

## Customization

Edit templates:
1. Open template file: `~/.emacs.d/templates/notes/[template-name].org`
2. Modify as needed
3. Save

Add new template:
1. Create template file in `~/.emacs.d/templates/notes/`
2. Edit `~/.emacs.d/config/org-config.el`
3. Add to `org-capture-templates`
4. Reload config: `M-x load-file RET ~/.emacs.d/config/org-config.el RET`

---

**Happy Learning & Organizing! 🚀**
