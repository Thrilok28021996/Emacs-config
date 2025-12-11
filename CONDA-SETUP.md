# Conda Environment Setup for Emacs

**Your Emacs is now configured to auto-activate conda environments!**

---

## 🚀 How It Works

When you open a Python file, Emacs automatically:
1. Looks for `environment.yml` in your project root
2. Reads the environment name from the file
3. Activates that conda environment
4. Shows message: "Activated conda environment: myproject"

---

## 📋 Quick Start

### 1. Create Conda Environment

**Option A: From environment.yml (Recommended)**
```bash
cd ~/my-project

# Create environment.yml
cat > environment.yml << EOF
name: myproject
dependencies:
  - python=3.11
  - numpy
  - pandas
  - pytest
  - ipython
EOF

# Create environment
conda env create -f environment.yml
```

**Option B: Quick create**
```bash
conda create -n myproject python=3.11 numpy pandas pytest ipython
```

### 2. Open Python File

```bash
cd ~/my-project
emacs main.py
```

**Result:** Conda environment activates automatically! 🎉

---

## 🔧 Manual Activation

If auto-activation doesn't work or you need to switch:

```
M-x conda-env-activate RET myproject RET
```

**List available environments:**
```
M-x conda-env-list RET
```

**Deactivate current environment:**
```
M-x conda-env-deactivate RET
```

---

## 📂 Project Structure

For auto-activation to work, your project should look like:

```
my-project/
├── environment.yml        # Conda will read this!
├── main.py
├── src/
│   └── module.py
└── tests/
    └── test_module.py
```

**Important:** `environment.yml` must be in the project root!

---

## 🐍 Example environment.yml

### Basic Python Project:
```yaml
name: myproject
dependencies:
  - python=3.11
  - pip
  - pip:
    - requests
```

### Data Science Project:
```yaml
name: ds-project
dependencies:
  - python=3.11
  - numpy
  - pandas
  - matplotlib
  - jupyter
  - scikit-learn
  - ipython
```

### Machine Learning Project:
```yaml
name: ml-project
dependencies:
  - python=3.10
  - pytorch
  - torchvision
  - numpy
  - pandas
  - matplotlib
  - jupyter
  - ipython
  - pip:
    - transformers
    - datasets
```

### Web Development Project:
```yaml
name: webapp
dependencies:
  - python=3.11
  - flask
  - sqlalchemy
  - pytest
  - ipython
  - pip:
    - flask-cors
    - python-dotenv
```

---

## ✅ Verify Activation

**In Emacs:**
1. Open a Python file in your project
2. Look for message: `Activated conda environment: myproject`
3. Check modeline - should show env name

**Or run Python shell:**
```
M-x run-python RET
```

In the Python REPL:
```python
import sys
print(sys.prefix)  # Should show your conda env path
```

---

## 🛠️ Troubleshooting

### Environment not activating?

**Check 1: Is environment.yml in project root?**
```bash
ls ~/my-project/environment.yml
```

**Check 2: Does environment exist?**
```bash
conda env list
```

**Check 3: Is conda detected?**
```elisp
;; In Emacs, evaluate (M-x eval-expression):
conda-anaconda-home
;; Should show path like: "/Users/thrilok/anaconda3"
```

### Manual fix:

```
M-x conda-env-activate RET myproject RET
```

---

## 🎯 Workflow Example

### Starting New Project:

```bash
# 1. Create project directory
mkdir ~/my-awesome-project
cd ~/my-awesome-project

# 2. Create environment.yml
cat > environment.yml << EOF
name: awesome
dependencies:
  - python=3.11
  - numpy
  - pandas
  - pytest
  - ipython
EOF

# 3. Create conda environment
conda env create -f environment.yml

# 4. Create Python file
cat > main.py << EOF
import numpy as np
import pandas as pd

def main():
    print("Hello from conda environment!")
    print(f"NumPy version: {np.__version__}")
    print(f"Pandas version: {pd.__version__}")

if __name__ == "__main__":
    main()
EOF

# 5. Open in Emacs
emacs main.py

# Environment activates automatically!
# Press C-c C-c to run the code in org-mode
# Or M-x run-python to start REPL
```

---

## 🔥 Pro Tips

### 1. Use iPython for better REPL:
```yaml
dependencies:
  - ipython  # Add this to environment.yml
```

Emacs will automatically use iPython if installed!

### 2. Add pytest for testing:
```yaml
dependencies:
  - pytest
  - pytest-cov
```

Run tests: `M-x python-pytest`

### 3. Multiple environments:

```bash
# Work environment
conda create -n work python=3.11 pandas numpy

# Learning environment
conda create -n learning python=3.11 jupyter matplotlib

# Switch between them:
M-x conda-env-activate RET work RET
M-x conda-env-activate RET learning RET
```

### 4. Org-mode executable code blocks:

```org
#+begin_src python :results output
import numpy as np
print(f"NumPy from conda: {np.__version__}")
#+end_src

# Press C-c C-c to execute!
# Uses your activated conda environment!
```

---

## 📚 Integration with Other Tools

### Jupyter Notebooks:
```bash
# Install in conda env:
conda install jupyter

# Launch from Emacs:
M-x run-python RET
# Or
M-! jupyter notebook RET
```

### Pytest:
```bash
# Install in conda env:
conda install pytest

# Run from Emacs:
M-x python-pytest RET
```

### Black formatter:
```bash
# Install in conda env:
conda install black

# Formats automatically on save (apheleia)!
```

---

## 🎓 Learning Resources

**Conda Cheat Sheet:**
```bash
conda env list              # List environments
conda create -n NAME python=3.11
conda activate NAME
conda deactivate
conda remove -n NAME --all  # Delete environment
conda env export > environment.yml  # Save current env
```

**Emacs Commands:**
```
M-x conda-env-activate
M-x conda-env-deactivate
M-x conda-env-list
M-x run-python
M-x python-pytest
```

---

## ✨ Summary

Your Emacs now:
- ✅ Auto-activates conda environments from `environment.yml`
- ✅ Works with iPython if installed
- ✅ Runs pytest with optimal settings
- ✅ Executes code blocks in conda environment
- ✅ Integrates with eshell and REPL

**Just create `environment.yml` and start coding!** 🚀

---

*Last updated: 2025-01-13*
*Configured for conda workflows*
