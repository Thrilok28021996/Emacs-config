# Org-mode vs Markdown: Comprehensive Comparison

## Quick Summary

**TL;DR:**
- **Markdown**: Simple, universal, great for writing and publishing
- **Org-mode**: Powerful, programmable, great for knowledge management and productivity

---

## 📊 Feature Comparison Table

| Feature | Org-mode | Markdown |
|---------|----------|----------|
| **Learning Curve** | Steep | Gentle |
| **Readability** | Excellent | Excellent |
| **Export Formats** | 10+ (HTML, PDF, LaTeX, etc.) | 2-3 (HTML, PDF via pandoc) |
| **Task Management** | ✅ Native TODO system | ❌ Need plugins |
| **Tables** | ✅ Advanced (formulas, calc) | ⚠️ Basic only |
| **Code Execution** | ✅ Babel (Python, C++, etc.) | ❌ No execution |
| **Linking** | ✅ Advanced (backlinks, IDs) | ⚠️ Basic links only |
| **Scheduling** | ✅ Dates, deadlines, recurring | ❌ None |
| **Agenda Views** | ✅ Powerful agenda system | ❌ None |
| **Portability** | ⚠️ Emacs-specific | ✅ Universal |
| **GitHub Support** | ⚠️ Basic rendering | ✅ Native support |
| **Mobile Apps** | ⚠️ Limited | ✅ Many options |
| **Outlining** | ✅ Advanced folding | ⚠️ Basic headings |
| **Properties** | ✅ Custom metadata | ❌ Limited YAML |
| **Capture Templates** | ✅ Sophisticated | ❌ None |

---

## 📝 Syntax Comparison

### Headings

**Markdown:**
```markdown
# Heading 1
## Heading 2
### Heading 3
```

**Org-mode:**
```org
* Heading 1
** Heading 2
*** Heading 3
```

---

### Emphasis

**Markdown:**
```markdown
**bold** or __bold__
*italic* or _italic_
~~strikethrough~~
`code`
```

**Org-mode:**
```org
*bold*
/italic/
+strikethrough+
~code~
=verbatim=
```

---

### Lists

**Markdown:**
```markdown
- Unordered item
- Another item
  - Nested item

1. Ordered item
2. Another item
```

**Org-mode:**
```org
- Unordered item
- Another item
  - Nested item

1. Ordered item
2. Another item
3. [@10] Start at 10
```

**Org-mode Advantage:** Checkboxes!
```org
- [ ] Todo item
- [X] Done item
- [-] Partial (1/2)
  - [X] Sub-task 1
  - [ ] Sub-task 2
```

---

### Links

**Markdown:**
```markdown
[Link text](https://example.com)
![Image alt](image.png)
```

**Org-mode:**
```org
[[https://example.com][Link text]]
[[file:image.png][Image description]]
[[id:unique-id][Link to another note]]
[[file:~/Documents/notes.org::*Heading][Link to heading]]
```

**Org-mode Advantage:** Internal linking, IDs, heading links

---

### Code Blocks

**Markdown:**
```markdown
​```python
def hello():
    print("Hello, World!")
​```
```

**Org-mode:**
```org
#+begin_src python
def hello():
    print("Hello, World!")
#+end_src
```

**Org-mode Advantage:** Execute code with `C-c C-c`!
```org
#+begin_src python :results output
print("This actually runs!")
#+end_src

#+RESULTS:
: This actually runs!
```

---

### Tables

**Markdown:**
```markdown
| Name | Age | City |
|------|-----|------|
| John | 30  | NYC  |
| Jane | 25  | LA   |
```

**Org-mode:**
```org
| Name | Age | City |
|------+-----+------|
| John |  30 | NYC  |
| Jane |  25 | LA   |
|------+-----+------|
| Sum  |  55 |      |
#+TBLFM: @4$2=vsum(@2..@3)
```

**Org-mode Advantages:**
- Auto-formatting (press TAB)
- Spreadsheet formulas
- Column operations
- Row sorting

---

## 🎯 Task Management

### Markdown
❌ **No native task management**

Need external tools like:
- Obsidian Tasks plugin
- Notion
- Linear

Example (requires plugin):
```markdown
- [ ] Buy groceries
- [x] Write report
```

---

### Org-mode
✅ **Built-in TODO system**

```org
* TODO Buy groceries
  DEADLINE: <2025-01-15 Wed>
* DONE Write report
  CLOSED: [2025-01-13 Mon 14:30]
* WAITING Call client
  SCHEDULED: <2025-01-16 Thu>
* TODO Project Planning [1/3]
** DONE Research
** TODO Design
** TODO Implement
```

**Org-mode Features:**
- TODO states (TODO, DONE, WAITING, etc.)
- Deadlines and scheduling
- Recurring tasks
- Progress tracking
- Priorities (A, B, C)
- Time tracking
- Agenda views

**Agenda View Example:**
```
Week-agenda (W02):
Monday     13 January 2025
  tasks:      10:00...... Scheduled:  TODO Team meeting
  tasks:      Deadline:   TODO Submit report
Tuesday    14 January 2025
  tasks:      TODO Review code
```

---

## 📅 Scheduling & Time Management

### Markdown
❌ No native scheduling

### Org-mode
✅ Advanced scheduling system

```org
* TODO Team Meeting
  SCHEDULED: <2025-01-15 Wed 10:00-11:00>
  :PROPERTIES:
  :LOCATION: Conference Room A
  :END:

* TODO Weekly Report
  DEADLINE: <2025-01-17 Fri>
  SCHEDULED: <2025-01-15 Wed>

* TODO Gym
  SCHEDULED: <2025-01-13 Mon +1w>
  (Repeats weekly)

* DONE Morning Review
  CLOSED: [2025-01-13 Mon 09:15]
  :LOGBOOK:
  CLOCK: [2025-01-13 Mon 09:00]--[2025-01-13 Mon 09:15] =>  0:15
  :END:
```

---

## 🔗 Linking & Knowledge Management

### Markdown
**Basic linking:**
```markdown
[Link to another file](./notes/file.md)
[Link to section](./notes/file.md#section)
```

**Limitations:**
- No backlinks (without tools like Obsidian)
- No unique IDs
- Manual link maintenance

---

### Org-mode
**Advanced linking:**

```org
# Link by ID (persistent)
[[id:abc123-def456][My Note]]

# Link to file
[[file:~/notes/project.org][Project]]

# Link to specific heading
[[file:~/notes/project.org::*Implementation][Implementation Section]]

# Link to line number
[[file:~/code/app.py::42][Line 42 in app.py]]

# Custom link types
[[elisp:(message "Hello")][Click to run code]]
[[man:git][Git manual]]
```

**With Org-roam:**
- Automatic backlinks
- Graph visualization
- Zettelkasten method
- Knowledge base

---

## 💻 Code Execution (Literate Programming)

### Markdown
❌ **No code execution**

Code blocks are display-only:
```markdown
​```python
result = 2 + 2
print(result)
​```
```

Must copy-paste to execute elsewhere.

---

### Org-mode
✅ **Execute code in-place with Babel**

```org
#+begin_src python :results output
result = 2 + 2
print(f"The answer is {result}")
#+end_src

#+RESULTS:
: The answer is 4
```

**Multi-language support:**
```org
# Python
#+begin_src python :results value
import pandas as pd
df = pd.DataFrame({'a': [1,2,3]})
return df.sum()
#+end_src

# C++
#+begin_src cpp :includes <iostream> :results output
std::cout << "Hello from C++!" << std::endl;
#+end_src

# Shell
#+begin_src bash :results output
ls -la | head -5
#+end_src

# SQL
#+begin_src sql :engine postgresql :database mydb
SELECT * FROM users LIMIT 5;
#+end_src
```

**Pass data between blocks:**
```org
#+name: data
#+begin_src python :results value
return [1, 2, 3, 4, 5]
#+end_src

#+begin_src python :var nums=data :results output
print(f"Sum: {sum(nums)}")
#+end_src

#+RESULTS:
: Sum: 15
```

---

## 📤 Export & Publishing

### Markdown
**Export options:**
- HTML (via pandoc or static site generators)
- PDF (via pandoc + LaTeX)
- GitHub rendering

**Tools:**
- Jekyll, Hugo, MkDocs (static sites)
- Pandoc (conversion)

---

### Org-mode
**Native export to:**
- HTML5 (with custom CSS)
- LaTeX → PDF
- Beamer (presentations)
- ODT (LibreOffice)
- Markdown
- Plain text
- man pages
- iCalendar
- Org → Reveal.js slides
- And more...

**Example:**
```org
#+TITLE: My Document
#+AUTHOR: Your Name
#+OPTIONS: toc:2 num:nil
#+LATEX_CLASS: article
#+LATEX_HEADER: \usepackage{custom}

* Introduction
Content here...
```

Export with: `C-c C-e`
- `h o` → HTML and open
- `l p` → LaTeX → PDF
- `m` → Markdown
- `t` → Plain text

---

## 🎓 Capture & Templates

### Markdown
❌ **No native capture system**

Must manually:
1. Open file
2. Navigate to location
3. Type content

---

### Org-mode
✅ **Sophisticated capture system**

**Quick capture from anywhere:**
```
SPC o c (or M-x org-capture)
```

**Templates:**
```elisp
(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/tasks.org")
         "* TODO %?\n  %u\n  %a")

        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* %? :meeting:\n  %U\n  ** Attendees\n  - \n  ** Notes\n  - ")

        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %U %?\n  %i")))
```

**Capture from anywhere:**
- Web browser → save link + selection
- Email → create task from email
- Terminal → quick note
- Instant template insertion

---

## 🔍 Search & Filtering

### Markdown
**Search options:**
- `grep` or `ripgrep`
- Editor search
- External tools (Obsidian, etc.)

---

### Org-mode
**Powerful search:**

1. **Agenda search:**
   ```
   C-c a s python      # Search for "python"
   C-c a m +project-done  # Tags: has project, not done
   ```

2. **Tag searches:**
   ```
   C-c a m work+urgent   # work AND urgent
   C-c a m python|cpp    # python OR cpp
   ```

3. **Property searches:**
   ```org
   * Task
     :PROPERTIES:
     :CATEGORY: Work
     :PRIORITY: A
     :EFFORT:   2:00
     :END:
   ```

   Search: `C-c a m CATEGORY="Work"`

4. **Sparse trees:**
   ```
   C-c / /    # Search and fold
   C-c / t    # Show TODOs only
   ```

---

## 📊 Tables & Spreadsheets

### Markdown
**Basic tables only:**
```markdown
| Item   | Price |
|--------|-------|
| Apple  | $1.00 |
| Banana | $0.50 |
```

- No calculations
- No auto-formatting
- Manual alignment

---

### Org-mode
**Advanced table features:**

```org
| Item   | Qty | Price | Total |
|--------+-----+-------+-------|
| Apple  |   3 |  1.00 |  3.00 |
| Banana |   5 |  0.50 |  2.50 |
|--------+-----+-------+-------|
| Total  |     |       |  5.50 |
#+TBLFM: $4=$2*$3::@5$4=vsum(@2..@3)
```

**Features:**
- Auto-formatting (press TAB)
- Spreadsheet formulas
- Column formulas
- Row operations
- Sorting
- Field references
- Calc integration

**Example formulas:**
```org
#+TBLFM: $4=$2*$3           # Column 4 = Column 2 × Column 3
#+TBLFM: @>$4=vsum(@2..@-1) # Last row = sum of above
#+TBLFM: $5=$4*0.1          # 10% tax
```

---

## 🗂️ Properties & Metadata

### Markdown
**YAML frontmatter (limited):**
```markdown
---
title: My Note
date: 2025-01-13
tags: [python, learning]
---

# Content here
```

- Only at file start
- Limited support
- Not queryable

---

### Org-mode
**Rich property system:**

```org
* Project: Build Dashboard
  :PROPERTIES:
  :CATEGORY: Work
  :EFFORT:   8:00
  :ASSIGNED: John
  :PRIORITY: A
  :DEADLINE: <2025-01-20>
  :CUSTOM:   Any value
  :ID:       unique-id-123
  :END:

  Task details here...
```

**Benefits:**
- Attach to any heading
- Query in agenda
- Custom properties
- Column view
- Inheritance

**Column view:**
```
ITEM                  | CATEGORY | EFFORT | ASSIGNED
Build Dashboard       | Work     | 8:00   | John
  Design mockups      | Work     | 2:00   | Jane
  Implement API       | Work     | 4:00   | John
```

---

## 🌐 Platform Support

### Markdown
✅ **Universal support:**
- GitHub, GitLab, Bitbucket
- VS Code, Sublime, Atom
- Obsidian, Notion, Bear
- iOS, Android apps
- Web browsers
- Static site generators

**Best for:**
- Documentation
- README files
- GitHub repos
- Blogs
- Cross-platform notes

---

### Org-mode
⚠️ **Primarily Emacs:**
- Emacs (best experience)
- Orgzly (Android)
- Beorg (iOS - limited)
- GitHub (basic rendering)
- Some VS Code support

**Best for:**
- Emacs users
- Knowledge management
- Task management
- Literate programming
- Academic writing

---

## 🎯 Use Cases: When to Use Which?

### Use **Markdown** when:

1. ✅ Writing for GitHub/GitLab
2. ✅ Creating README files
3. ✅ Building static websites
4. ✅ Cross-platform collaboration
5. ✅ Simple documentation
6. ✅ Mobile-first workflow
7. ✅ Quick blog posts
8. ✅ Team wikis (non-Emacs users)

**Example scenarios:**
- Project README
- Technical documentation
- Blog posts
- Knowledge base for teams
- Quick notes on mobile

---

### Use **Org-mode** when:

1. ✅ Personal knowledge management
2. ✅ Task and project management
3. ✅ Academic writing (LaTeX)
4. ✅ Literate programming
5. ✅ Research notes with code
6. ✅ Time tracking
7. ✅ Planning and scheduling
8. ✅ Building a "second brain"
9. ✅ Data analysis notebooks
10. ✅ Complex outlining

**Example scenarios:**
- PhD thesis
- Personal task management
- Zettelkasten note-taking
- Course notes with executable code
- Weekly planning
- Research journal
- GTD system

---

## 🔄 Can They Work Together?

**Yes!** Here's how:

### Export Org to Markdown
```elisp
C-c C-e m m    # Export to Markdown
```

### Use both strategically:
```
Your Workflow:
├── Org-mode (Personal)
│   ├── Tasks & TODOs
│   ├── Private notes
│   ├── Research with code
│   └── Time tracking
└── Markdown (Public)
    ├── GitHub READMEs
    ├── Documentation
    ├── Blog posts
    └── Team wiki
```

### Org-mode for input, Markdown for output:
1. Take notes in Org-mode (with all features)
2. Export to Markdown for publishing
3. Best of both worlds!

---

## 📈 Learning Curve

### Markdown
```
Time to productivity: 30 minutes
Mastery: 2-3 hours
```

**Learning path:**
1. Headings, lists, links (10 min)
2. Code blocks, tables (10 min)
3. Advanced formatting (10 min)
4. Done!

---

### Org-mode
```
Time to productivity: 2-3 hours
Mastery: Weeks to months
```

**Learning path:**
1. Basic formatting (1 hour)
2. TODO system (1 hour)
3. Agenda (2 hours)
4. Capture templates (1 hour)
5. Babel/code execution (2 hours)
6. Advanced features (ongoing)

**But:** The investment pays off with powerful features!

---

## 🏆 Verdict

### Choose **Markdown** if you:
- Need universal compatibility
- Work in teams (non-Emacs)
- Prioritize simplicity
- Focus on publishing
- Need mobile apps
- Want quick setup

### Choose **Org-mode** if you:
- Use Emacs
- Need powerful task management
- Want executable notebooks
- Build knowledge bases
- Do academic writing
- Need advanced features
- Prioritize power over portability

### Use **Both** if you:
- Write in Org, export to Markdown
- Keep private notes in Org
- Publish documentation in Markdown
- Want the best of both worlds

---

## 💡 Migration Path

### From Markdown to Org-mode:

```bash
# Convert with pandoc
pandoc -f markdown -t org input.md -o output.org
```

### From Org-mode to Markdown:

```elisp
# In Emacs
C-c C-e m m    # Export to Markdown
```

---

## 🔗 Your Current Setup

**You have BOTH configured!**

**Markdown:**
- Config: `~/.emacs.d/config/markdown.el`
- Use for: GitHub, documentation, simple notes

**Org-mode:**
- Config: `~/.emacs.d/config/org-config.el`
- Templates: `~/.emacs.d/templates/notes/`
- Use for: Knowledge management, tasks, learning notes

**Recommendation:**
- **Learning notes, courses, tutorials** → Org-mode (use the templates!)
- **Project READMEs, documentation** → Markdown
- **Personal tasks, planning** → Org-mode
- **Public writing** → Write in Org, export to Markdown

---

## 📚 Resources

### Markdown
- [Markdown Guide](https://www.markdownguide.org/)
- [GitHub Flavored Markdown](https://github.github.com/gfm/)
- [CommonMark](https://commonmark.org/)

### Org-mode
- [Org Manual](https://orgmode.org/manual/)
- [Org-roam Documentation](https://www.orgroam.com/)
- Your templates: `~/.emacs.d/templates/TEMPLATES-GUIDE.md`
- Your tags: `~/.emacs.d/templates/TAGS-REFERENCE.org`

---

## 🎓 Quick Start Guide

### For Course Notes:
**Use Org-mode!**
```
SPC o c → l c    # Course template
```
Why? Executable code, better organization, progress tracking

### For GitHub README:
**Use Markdown!**
```markdown
# Project Name
Simple, universal, renders on GitHub
```

### For Weekly Planning:
**Use Org-mode!**
```
SPC o c → f      # Weekly flow template
```
Why? Task management, scheduling, time tracking

### For Quick Documentation:
**Use Markdown!**
Faster to write, more portable

---

**Summary:** Org-mode is a powerhouse for personal productivity and knowledge management. Markdown is simpler and more universal. Use both strategically based on your needs!

*Last updated: 2025-01-13*
