#!/bin/bash
# Emacs Configuration Byte-Compilation Checker
# Run this script to verify all configuration files compile cleanly

cd "$(dirname "$0")"

echo "=========================================="
echo " Emacs Configuration Compilation Check"
echo "=========================================="
echo ""

TOTAL=0
PASS=0
WARN=0
FAIL=0

check_file() {
    local file="$1"
    local name=$(basename "$file")
    TOTAL=$((TOTAL + 1))

    if [ ! -f "$file" ]; then
        echo "❌ $name - File not found"
        FAIL=$((FAIL + 1))
        return
    fi

    # Compile and check for warnings
    output=$(emacs --batch --eval "(progn (add-to-list 'load-path \"modules\") (require 'utilities nil t) (byte-compile-file \"$file\"))" 2>&1)

    # Filter out expected use-package :straight warnings
    warnings=$(echo "$output" | grep -E "Warning:" | grep -v "use-package.*:straight" | grep -v "not known to be defined")

    if [ -z "$warnings" ]; then
        echo "✅ $name"
        PASS=$((PASS + 1))
    else
        echo "⚠️  $name"
        echo "$warnings" | sed 's/^/   /'
        WARN=$((WARN + 1))
    fi
}

echo "Checking root files..."
check_file "early-init.el"
check_file "init.el"

echo ""
echo "Checking modules..."
for file in modules/*.el; do
    check_file "$file"
done

echo ""
echo "Checking config files..."
for file in config/*.el; do
    check_file "$file"
done

echo ""
echo "=========================================="
echo " Results"
echo "=========================================="
echo "Total:    $TOTAL files"
echo "Passed:   $PASS files ✅"
echo "Warnings: $WARN files ⚠️"
echo "Failed:   $FAIL files ❌"
echo ""

if [ $WARN -eq 0 ] && [ $FAIL -eq 0 ]; then
    echo "🎉 All files compile cleanly!"
    exit 0
else
    echo "⚠️  Some files have issues - see details above"
    exit 1
fi
