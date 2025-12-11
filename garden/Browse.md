# Browse & Find

*Quick navigation to find exactly what you need*

## 🔍 Quick Find

### By Type
- **Daily stuff:** [[Templates/Daily Practice]] | [[Templates/Daily Idea Process]]
- **Weekly reviews:** [[Templates/Weekly Review]] | [[Templates/Weekly Insight Review]]
- **New items:** [[Templates/Quick Idea]] | [[Templates/Quick Note]]
- **Learning:** [[Templates/Course Template]] | [[Templates/Book Template]] | [[Templates/Tutorial Template]]

### By Status
- **Active projects:** [[Projects/Project Pipeline#Active Projects]]
- **Current skills:** [[Skill Building/Skill Tracker#Current Active Sprint]]
- **Learning in progress:** [[Learning Content/Learning Dashboard#Active Learning]]
- **Recent ideas:** [[Ideas Inbox/Ideas Dashboard#Recent Captures]]

### By Action Needed
- **Need decisions:** [[Projects/Project Pipeline#Project Backlog]]
- **Need processing:** [[Ideas Inbox/Ideas Dashboard#Ideas Ready for Projects]]
- **Need review:** [[Misc/Misc Dashboard#Need Processing]]

## 📊 Dashboards (Your Control Centers)

### Main Dashboards
- 🏠 **Start Here:** [[START HERE]] - System overview
- 💡 **Ideas:** [[Ideas Inbox/Ideas Dashboard]] - Idea management
- 🚀 **Projects:** [[Projects/Project Pipeline]] - Project tracking  
- 🎯 **Skills:** [[Skill Building/Skill Tracker]] - Skill development
- 📚 **Learning:** [[Learning Content/Learning Dashboard]] - Content consumption
- 🧠 **Thinking:** [[Thought Analysis/Thinking Insights]] - Meta-cognition
- 📦 **Misc:** [[Misc/Misc Dashboard]] - Everything else

## 🎯 By Current Focus

### I Want to...
- **Start my day:** [[Templates/Daily Practice]] + [[Templates/Daily Idea Process]]
- **Capture an idea:** [[Templates/Quick Idea]]
- **Start a new project:** [[Templates/Project Template]]
- **Begin learning:** [[Templates/Course Template]] | [[Templates/Book Template]]
- **Review my week:** [[Templates/Weekly Review]] + [[Templates/Weekly Insight Review]]
- **Analyze my thinking:** [[Templates/Thought Pattern Analysis]]
- **Find something quickly:** Browse sections below ⬇️

### I'm Looking for...
- **My current sprint:** [[Skill Building/Skill Tracker#Current Active Sprint]]
- **Active projects:** [[Projects/Project Pipeline#Active Projects]]
- **Books I'm reading:** [[Learning Content/Learning Dashboard#Active Learning]]
- **Recent breakthrough:** [[Thought Analysis/Thinking Insights#Recent Breakthroughs]]
- **Ideas to develop:** [[Ideas Inbox/Ideas Dashboard#Hot Ideas]]

## 📁 Folder Quick Access

### Daily Practice
*Your skill-building system*
- Current practice notes
- Skill tracking data

### Ideas Inbox  
*Raw idea capture*
- New ideas needing processing
- Idea clustering and connections

### Projects
*Project development pipeline*
- Active, backlog, and completed projects
- Project evolution tracking

### Learning Content
*Books, courses, tutorials*
- Active learning materials
- Completion tracking

### Skill Building
*Deliberate practice system*
- Skill tracker and sprint files
- Progress monitoring

### Thought Analysis
*Meta-cognitive insights*
- Thinking pattern analysis
- Cognitive bias tracking

### Templates
*Reusable structures*
- All template files for quick copying

### Misc
*Everything else*
- Tools, lists, random notes
- Items needing categorization

## 🔗 Smart Connections

### Recently Modified
```dataview
TABLE file.mtime as "Last Modified"
FROM ""
WHERE file.name != "Browse"
SORT file.mtime DESC
LIMIT 10
```

### High Energy Items
```dataview
LIST
FROM ""
WHERE contains(energy, "High") OR contains(energy, "🔥")
LIMIT 5
```

### Items Needing Action
```dataview
LIST
FROM ""
WHERE contains(file.tasks, "[ ]")
LIMIT 10
```

## 🚀 Quick Start Workflows

### Morning Routine
1. [[Templates/Daily Practice]] - Set skill target
2. [[Templates/Daily Idea Process]] - Review yesterday's ideas
3. [[Ideas Inbox/Ideas Dashboard]] - Check new captures

### Evening Routine  
1. [[Templates/Daily Practice]] - Reflect on skill progress
2. [[Templates/Daily Idea Process]] - Process today's ideas
3. [[Projects/Project Pipeline]] - Update project status

### Weekly Planning
1. [[Templates/Weekly Review]] - Skill development review
2. [[Templates/Weekly Insight Review]] - Idea and thought analysis
3. [[Learning Content/Learning Dashboard]] - Plan learning time

---
*When you know where to look, you find what you need*