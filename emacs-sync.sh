#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# emacs-sync.sh — Doom-style package reconciliation for a hand-rolled init.el
#
# One authority: init.el. The package list is PARSED out of your use-package
# forms at run time. Nothing is hardcoded here, so this file cannot drift.
#
#   ./emacs-sync.sh check        dry run: missing / outdated / orphans / native comp
#   ./emacs-sync.sh install      install what init.el declares but elpa/ lacks
#   ./emacs-sync.sh upgrade      upgrade installed packages (ELPA, then :vc)
#   ./emacs-sync.sh sync [-u]    install + purge + build + snapshot + check
#                                sync alone never moves versions; -u adds upgrade
#   ./emacs-sync.sh purge        remove orphans (dependency-aware)
#   ./emacs-sync.sh build        native-compile the tree, then verify it worked
#   ./emacs-sync.sh snapshot     freeze the installed set now (lock + vault copy)
#   ./emacs-sync.sh snapshots    list generations
#   ./emacs-sync.sh restore [GEN] [--only a,b]
#   ./emacs-sync.sh doctor       external tools your config shells out to
#
# Flags:  -y/--yes  answer yes to prompts     -n  force archive index refresh
#         -v        keep Emacs chatter         -u  let sync also upgrade
#         --only a,b   restore only these (may skew dependencies — see below)
# Env:    EMACS=/path/to/emacs    SYNC_DIR=~/.emacs.d    SYNC_KEEP=2
#         SYNC_BUILD_WAIT=600  (build wait cap, seconds)
#
# Exit status is non-zero if anything failed. Failures are collected, listed and
# counted; the script never prints a happy summary on top of a failed run.
#
# A snapshot is a record, never a constraint: install and upgrade ignore the lock
# entirely, so "get the latest" is always an explicit `upgrade`. What the lock
# buys you is a way back, and it has to carry FILES, not just versions — MELPA
# serves only its current build (an older tarball 404s), so a version list could
# report a regression but never undo one. A generation therefore holds a copy of
# every installed package dir, restorable offline. Whole set, not just the
# declared names: putting one package back while its dependencies stay current
# is dependency skew, which is why --only warns.
#
# Why purge is safe: Emacs defines an orphan in `package--removable-packages`
# as "not in package-selected-packages AND not in the dependency closure of
# package-selected-packages AND user-installed". This script let-binds
# package-selected-packages to the set parsed from init.el and hands the job to
# `package-autoremove`, so magit's transient/with-editor/magit-section, org-roam's
# emacsql, cape's compat and friends are protected. No name-diff purge anywhere.
#
# Why the env guard: Emacs.app's bundled libgccjit passes clang
# -mmacosx-version-min=18.0 when MACOSX_DEPLOYMENT_TARGET is unset, and clang
# rejects 18.0 (no macOS 18 exists). Set before Emacs starts; verified after.
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

D="${SYNC_DIR:-${HOME}/.emacs.d}"
VAULT="${D}/emacs-sync-vault"
KEEP="${SYNC_KEEP:-2}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
FAIL_LOG="${TMP}/failures.log"
: > "$FAIL_LOG"
ROWS="${TMP}/installed.tsv"

CMD="${1:-help}"; shift || true
ASSUME_YES=0; VERBOSE=0; FORCE_REFRESH=0; DO_UPGRADE=0; ONLY=""; GEN=""
while (( $# )); do
  case "$1" in
    -y|--yes)     ASSUME_YES=1 ;;
    -n)           FORCE_REFRESH=1 ;;
    -v)           VERBOSE=1 ;;
    -u|--upgrade) DO_UPGRADE=1 ;;
    --only)       ONLY="${2:-}"; shift ;;
    --only=*)     ONLY="${1#--only=}" ;;
    -*)           echo "unknown flag: $1" >&2; exit 2 ;;
    *)            GEN="$1" ;;
  esac
  shift
done

if [[ -n "${EMACS:-}" && -x "${EMACS}" ]]; then EMACS_BIN="$EMACS"
elif [[ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]]; then EMACS_BIN=/Applications/Emacs.app/Contents/MacOS/Emacs
elif command -v emacs >/dev/null 2>&1; then EMACS_BIN="$(command -v emacs)"
else echo "emacs not found; set EMACS=/path/to/emacs" >&2; exit 1; fi

if [[ "$(uname -s)" == "Darwin" && -z "${MACOSX_DEPLOYMENT_TARGET:-}" ]]; then
  export MACOSX_DEPLOYMENT_TARGET="$(sw_vers -productVersion)"
fi

# APFS clone support: makes a ~30 MB generation nearly free until it diverges.
CLONE_OK=0
if cp -c "${EMACS_BIN}" "${TMP}/.clone-probe" 2>/dev/null; then CLONE_OK=1; fi
rm -f "${TMP}/.clone-probe"

head1()  { printf '\n━━━ %s\n' "$*"; }
fail()   { printf '  ! %s\n' "$*"; printf '%s\n' "$*" >> "$FAIL_LOG"; }
filter() { if (( VERBOSE )); then cat; else sed '/^Loading /d; /^Cleaning up/d; /^Deprecated/d; /^Contacting host/d; /^Package refresh/d; /^Importing package-keyring/d; /^$/d'; fi; }
copy_tree() {  # copy_tree SRC DESTDIR
  if (( CLONE_OK )); then
    cp -Rc "$1" "$2" 2>/dev/null && return 0
    rm -rf "${2%/}/$(basename "$1")"
  fi
  cp -R "$1" "$2"
}

# Streams Emacs output; records a non-zero Emacs exit as a failure but never
# aborts the script, so one bad step cannot hide the report of the others.
run_elisp() {
  local st=0
  "$EMACS_BIN" --batch -Q -l "$1" 2>&1 | filter || st=$?
  (( st == 0 )) || fail "emacs exited ${st} running $(basename "$1")"
  return 0
}

# ── prelude: shared batch environment, failure sink, verification helper ─────
cat > "$TMP/prelude.el" <<'EOF'
;;; prelude.el --- shared batch environment -*- lexical-binding: t; -*-
;; NOTE: user-emacs-directory is set by the generated step that loads this file,
;; so SYNC_DIR is honoured. native-comp-eln-load-path is computed at Emacs startup
;; and therefore always points at the real cache, whatever SYNC_DIR says.
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(package-initialize)
(require 'cl-lib)
(defvar sync--init-file (expand-file-name "init.el" user-emacs-directory))
(defvar sync--rows-file "")
(defvar sync--fail-file "")

(defun sync--installed-p (sym) (package-installed-p sym))

(defun sync--fail (fmt &rest args)
  (let ((line (apply (quote format) fmt args)))
    (princ (format "  ! %s\n" line))
    (when (and sync--fail-file (file-writable-p sync--fail-file))
      (write-region (concat line "\n") nil sync--fail-file t (quote silent)))))

(defun sync--refresh-needed-p ()
  (let ((idx (expand-file-name "archives/melpa/archive-contents" user-emacs-directory)))
    (or (not (file-exists-p idx))
        (> (- (float-time) (float-time (file-attribute-modification-time (file-attributes idx))))
           (* 24 60 60)))))

;; Compile one trivial file and report the truth, including the exact clang error.
(defun sync--native-check ()
  (if (not (featurep 'native-compile))
      (princ "  - native comp not enabled in this Emacs build (skipped)\n")
    (let* ((src (expand-file-name "sync-native-probe.el" temporary-file-directory))
           (out (expand-file-name "sync-native-probe.eln" temporary-file-directory)))
      (ignore-errors (delete-file out))
      (with-temp-file src
        (insert ";;; probe -*- lexical-binding: t; -*-\n(defun sync-probe () 1)\n(provide 'sync-probe)\n"))
      (let ((res (condition-case err
                     (progn (native-compile src out) :ok)
                   (error (error-message-string err)))))
        (if (and (eq res :ok) (file-exists-p out))
            (princ "  ✓ native comp works (probe file compiled)\n")
          (sync--fail "native comp BROKEN: %s (MACOSX_DEPLOYMENT_TARGET=%S)"
                      (car (split-string (format "%s" res) "\n")) (getenv "MACOSX_DEPLOYMENT_TARGET")))))))

(defun sync--eln-count ()
  (let ((cache (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path))))
    (if (and cache (file-exists-p cache))
        (length (directory-files-recursively cache "\\\\.eln\\\\'"))
      0)))

(defun sync--tmp-count ()
  (let ((cache (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path))))
    (if (and cache (file-exists-p cache))
        (length (directory-files-recursively cache "\\\\.eln\\\\.tmp\\\\'"))
      0)))

(defun sync--debris ()
  (let ((tmp (sync--tmp-count))
        (cache (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path))))
    (when (> tmp 0)
      (princ (format "  ! %d stale .eln.tmp files in %s (failed compiles)\n"
                     tmp cache)))))
EOF

# prologue every generated step needs: honour SYNC_DIR, wire the failure sink
emit_prologue() {
  printf '(setq user-emacs-directory "%s/")\n' "$D"
  printf '(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))\n'
  printf '(defvar sync--fail-file "%s")\n' "$FAIL_LOG"
  printf '(defvar sync--rows-file "%s")\n' "$ROWS"
  printf '(load (expand-file-name "prelude.el" "%s"))\n' "$TMP"
}

# ── parse init.el (read, never eval) ────────────────────────────────────────
{
  printf ';;; declared.el --- parse init.el -*- lexical-binding: t; -*-\n'
  emit_prologue
  cat <<'EOF'
(defun sync--forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))
(dolist (form (sync--forms sync--init-file))
  (when (eq (car-safe form) 'use-package)
    (let* ((name (cadr form))
           (args (cddr form))
           (builtin (and (memq :ensure args) (null (plist-get args :ensure))))
           (vc (plist-get args :vc)))
      (when (and (symbolp name) (not builtin))
        (if vc
            (princ (format "%s\tvc\t%s\t%s\n" name (or (plist-get vc :url) "")
                           (let ((r (plist-get vc :rev))) (if (stringp r) r ""))))
          (princ (format "%s\telpa\t\t\n" name)))))))
EOF
} > "$TMP/declared.el"

if [[ ! -f "${D}/init.el" ]]; then
  echo "✗ no init.el at ${D}/init.el — nothing to reconcile"; exit 1
fi
run_elisp "$TMP/declared.el" | grep -E '^[A-Za-z0-9@/._-]+\t(elpa|vc)\t' > "$TMP/declared.tsv" || true
DECLARED_COUNT=$(wc -l < "$TMP/declared.tsv" | tr -d ' ')
if (( DECLARED_COUNT == 0 )); then
  echo "✗ parsed 0 packages from ${D}/init.el — refusing to run (a wrong authority is worse than none)"; exit 1
fi
ELPA_NAMES=$(awk -F'\t' '$2=="elpa"{printf "%s ", $1}' "$TMP/declared.tsv")
VC_NAMES=$(awk   -F'\t' '$2=="vc"  {printf "%s ", $1}' "$TMP/declared.tsv")
VC_ALIST=$(awk -F'\t' '$2=="vc"{n=$1; u=$3; r=$4; if (r!="") printf "(%s :url \"%s\" :rev \"%s\") ", n, u, r; else printf "(%s :url \"%s\") ", n, u}' "$TMP/declared.tsv")

# ── build one elisp step file with the parsed lists bound ───────────────────
emit_plan() {
  {
    printf ';;; step.el --- generated -*- lexical-binding: t; -*-\n'
    emit_prologue
    printf '(defvar sync--elpa (quote (%s)))\n' "$ELPA_NAMES"
    printf '(defvar sync--vc   (quote (%s)))\n' "$VC_NAMES"
    printf '(defvar sync--vc-recipes (quote (%s)))\n' "$VC_ALIST"
    printf '(defvar sync--assume-yes %s)\n' "$([[ $ASSUME_YES == 1 ]] && echo t || echo nil)"
    printf '(defvar sync--max-wait %s)\n' "${SYNC_BUILD_WAIT:-600}"
    printf '(defun sync--declared () (append sync--elpa sync--vc))\n'
    printf '%s\n' "${1:-}"
  } > "$TMP/step.el"
  run_elisp "$TMP/step.el"
}

maybe_refresh() {
  { printf ';;; needed.el -*- lexical-binding: t; -*-\n'; emit_prologue
    printf '(princ (if (sync--refresh-needed-p) "yes" "no"))\n'; } > "$TMP/needed.el"
  local needed
  needed=$(run_elisp "$TMP/needed.el" | tail -1)
  if [[ "$needed" == "yes" || $FORCE_REFRESH == 1 ]]; then
    head1 "refreshing package archive index (30-60s)"
    emit_plan '(package-refresh-contents)
(princ "  ✓ index refreshed\n")'
  else
    printf '  (archive index is fresh; use -n to force a refresh)\n'
  fi
}

# ── commands ────────────────────────────────────────────────────────────────
cmd_check() {
  head1 "authority: init.el — $DECLARED_COUNT packages ($(wc -w <<<"$ELPA_NAMES" | tr -d ' ') ELPA, $(wc -w <<<"$VC_NAMES" | tr -d ' ') :vc)"
  emit_plan '
(let ((missing nil))
  (princ "\n── missing (declared, not installed) ──\n")
  (dolist (p (sync--declared))
    (unless (sync--installed-p p) (push p missing) (princ (format "  + %s\n" p))))
  (when missing
    (sync--fail "%d declared package(s) not installed: %s" (length missing) missing)))
(princ "\n── outdated (installed, newer in archive) ──\n")
(dolist (p sync--elpa)
  (let* ((inst  (cadr (assq p package-alist)))
         (avail (cadr (assq p package-archive-contents))))
    (when (and inst avail (version-list-< (package-desc-version inst) (package-desc-version avail)))
      (princ (format "  ^ %-22s %s -> %s\n" p
                     (package-version-join (package-desc-version inst))
                     (package-version-join (package-desc-version avail)))))))
(princ "\n── orphans (what purge would remove) ──\n")
(let ((package-selected-packages (sync--declared)))
  (if (fboundp (quote package--removable-packages))
      (let ((r (package--removable-packages)))
        (if r (dolist (d r) (princ (format "  - %s\n" (package-desc-name d))))
          (princ "  none\n")))
    (princ "  (package--removable-packages unavailable)\n")))
(princ "\n── native comp health ──\n")
(sync--native-check)
(sync--debris)
'
}

cmd_install() {
  head1 "installing what init.el declares but elpa/ lacks"
  maybe_refresh
  emit_plan '
(dolist (p sync--elpa)
  (unless (sync--installed-p p)
    (princ (format "  + %s\n" p))
    (condition-case err (package-install p t)
      (error (sync--fail "FAILED install %s: %s" p (error-message-string err))))))
(dolist (r sync--vc-recipes)
  (let ((p (car r)))
    (unless (sync--installed-p p)
      (princ (format "  + %s (vc: %s)\n" p (plist-get (cdr r) :url)))
      (condition-case err (package-vc-install r)
        (error (sync--fail "FAILED vc-install %s: %s" p (error-message-string err)))))))
(princ "  done\n")
'
}

cmd_upgrade() {
  head1 "upgrading ELPA packages"
  maybe_refresh
  emit_plan '
(dolist (p sync--elpa)
  (let* ((inst  (cadr (assq p package-alist)))
         (avail (cadr (assq p package-archive-contents))))
    (when (and inst avail (version-list-< (package-desc-version inst) (package-desc-version avail)))
      (condition-case err
          (progn (package-upgrade p)
                 (princ (format "  ^ %s -> %s\n" p
                                (package-version-join (package-desc-version avail)))))
        (error (sync--fail "FAILED upgrade %s: %s" p (error-message-string err)))))))
(princ "  done\n")
'
  head1 "upgrading :vc packages"
  emit_plan '
(require (quote package-vc))
(dolist (p sync--vc)
  (condition-case err
      (progn (package-vc-upgrade (cadr (assq p package-alist)))
             (princ (format "  ^ %s (vc)\n" p)))
    (error (sync--fail "FAILED vc-upgrade %s: %s" p (error-message-string err)))))
'
}

cmd_purge() {
  head1 "purging orphans — the declared set is the keep set, dependencies protected"
  emit_plan '
(let* ((package-selected-packages (sync--declared))
       (removable (and (fboundp (quote package--removable-packages))
                       (package--removable-packages))))
  (cond
   ((null removable) (princ "  nothing to remove\n"))
   (t (princ (format "  %d candidate(s):\n" (length removable)))
      (dolist (d removable) (princ (format "    - %s\n" (package-desc-name d))))
      (if sync--assume-yes
          (progn (mapc (quote package-delete) removable) (princ "  removed\n"))
          (princ "  (nothing removed — pass -y)\n")))))
'
}

cmd_build() {
  head1 "native-compiling the package tree"
  emit_plan '
(if (not (featurep (quote native-compile)))
    (princ "  - native compilation not supported in this Emacs build (skipped)\n")
  (require (quote comp))
  (defun sync--queued ()
    ;; comp--async-runnings returns a COUNT, not a boolean: 0 is truthy in Elisp.
    (+ (if (fboundp (quote comp--async-runnings)) (comp--async-runnings) 0)
       (if (boundp (quote comp-files-queue)) (length comp-files-queue) 0)))
  (let* ((dir (expand-file-name "elpa/" user-emacs-directory))
         (cache (and (boundp (quote native-comp-eln-load-path)) (car native-comp-eln-load-path)))
         (before (sync--eln-count))
         (waited 0)
         (hit-cap nil))
    (princ (format "  cache: %s\n  .eln before: %d\n" cache before))
    (native-compile-async dir t)
    (princ (format "  queued jobs: %d\n" (sync--queued)))
    (while (and (< waited sync--max-wait) (> (sync--queued) 0))
      (sleep-for 1) (setq waited (1+ waited)))
    (when (and (> (sync--queued) 0) (>= waited sync--max-wait))
      (setq hit-cap t))
    (let ((after (sync--eln-count)))
      (princ (format "  .eln after : %d  (%+d, waited %ds%s)\n" after (- after before) waited
                     (if hit-cap " — HIT THE WAIT CAP, compilation may still be running" ""))))))
(princ "\n── verification ──\n")
(sync--native-check)
(sync--debris)
'
}

# ── snapshots: version lock + a real copy of every installed package dir ─────
cmd_snapshot() {
  head1 "snapshotting the installed package set"
  {
    printf ';;; step.el --- generated -*- lexical-binding: t; -*-\n'
    emit_prologue
    cat <<'ELISP'
(with-temp-file sync--rows-file
  (dolist (entry package-alist)
    (let* ((name (car entry))
           (desc (cadr entry)))
      (when (and desc (fboundp (quote package--user-installed-p))
                 (package--user-installed-p name))
        (let ((dir (package-desc-dir desc)))
          (when (and dir (file-directory-p dir))
            ;; kind and commit both matter: a :vc package has no version in its
            ;; dir name and carries its revision in the desc extras
            (insert (format "%s\t%s\t%s\t%s\t%s\n"
                            (file-name-nondirectory (directory-file-name dir))
                            name (package-version-join (package-desc-version desc))
                            (or (package-desc-kind desc) (quote elpa))
                            (or (alist-get :commit (package-desc-extras desc)) "")))))))))
(with-temp-buffer
  (insert-file-contents sync--rows-file)
  (princ (format "  %d package dirs to freeze\n" (count-lines (point-min) (point-max)))))
ELISP
  } > "$TMP/step.el"
  run_elisp "$TMP/step.el"
  local rows_count gen gen_dir
  rows_count=$(wc -l < "$ROWS" | tr -d ' ')
  if (( rows_count == 0 )); then
    fail "snapshot produced 0 rows — refusing to write an empty generation"
    return 0
  fi
  gen="$(date +%Y%m%d-%H%M%S)"
  gen_dir="${VAULT}/${gen}"
  mkdir -p "${gen_dir}/packages"
  cp "$ROWS" "${gen_dir}/installed.tsv"
  {
    printf 'generation\t%s\ncreated\t%s\nemacs\t%s\npackages\t%s\n' \
      "$gen" "$(date '+%Y-%m-%d %H:%M:%S')" "$("$EMACS_BIN" --version | head -1)" "$rows_count"
  } > "${gen_dir}/MANIFEST.tsv"
  {
    printf ';;; emacs-sync lock — generation %s -*- lexical-binding: t; -*-\n' "$gen"
    printf ';; Generated by emacs-sync.sh snapshot. A record, not a constraint:\n'
    printf ';; install and upgrade ignore this file; it exists so restore has a target.\n'
    printf '((packages\n'
    awk -F'\t' '{printf "  (%s . \"%s\")%s\n", $2, $3, ($5 != "" ? "   ; commit " $5 : "")}' "$ROWS"
    printf '  )\n (vc\n'
    awk -F'\t' '$2=="vc"{printf "  (%s . \"%s\")\n", $1, $3}' "$TMP/declared.tsv"
    printf '  ))\n'
  } > "${gen_dir}/lock.el"

  local base copied=0 failed=0
  while IFS=$'\t' read -r base _name _ver _kind _commit; do
    [[ -n "$base" && -d "${D}/elpa/${base}" ]] || continue
    if copy_tree "${D}/elpa/${base}" "${gen_dir}/packages/"; then
      copied=$((copied+1))
    else
      failed=$((failed+1)); fail "could not copy ${base} into the generation"
    fi
  done < "$ROWS"
  printf '  %d dirs copied, %d failed\n' "$copied" "$failed"
  printf '  generation %s: %s apparent (%s)\n' "$gen" "$(du -sh "$gen_dir" | cut -f1)" \
         "$( (( CLONE_OK )) && echo 'APFS clones — real cost is far lower until files diverge' || echo 'full copies')"

  # prune: keep the newest $KEEP generations (portable, no head -n -N)
  ls -1d "${VAULT}"/*/ 2>/dev/null | sort | awk -v keep="$KEEP" '{a[NR]=$0} END{for(i=1;i<=NR-keep;i++) print a[i]}' \
    | while read -r g; do rm -rf "$g"; printf '    pruned %s\n' "$(basename "$g")"; done
}

cmd_snapshots() {
  head1 "snapshot generations in $VAULT"
  if [[ ! -d "$VAULT" ]] || [[ -z "$(ls -A "$VAULT" 2>/dev/null)" ]]; then
    printf '  none yet — one is taken automatically after a successful sync/upgrade\n'; return 0
  fi
  local g
  for g in $(ls -1d "${VAULT}"/*/ 2>/dev/null | sort); do
    printf '  %-18s %6s  %s packages  %s\n' \
      "$(basename "$g")" "$(du -sh "$g" | cut -f1)" \
      "$(wc -l < "${g}installed.tsv" 2>/dev/null | tr -d ' ')" \
      "$(awk -F'\t' '$1=="emacs"{print $2}' "${g}MANIFEST.tsv" 2>/dev/null)"
  done
}

cmd_restore() {
  head1 "restoring packages from a generation"
  if [[ ! -d "$VAULT" ]]; then fail "no vault at $VAULT"; return 0; fi
  local gen_dir
  if [[ -n "$GEN" ]]; then
    gen_dir="$(ls -1d "${VAULT}/${GEN}"* 2>/dev/null | head -1 || true)"
    if [[ -z "$gen_dir" ]]; then fail "no generation matching '${GEN}'"; return 0; fi
  else
    gen_dir="$(ls -1d "${VAULT}"/*/ 2>/dev/null | sort | tail -1)"
    if [[ -z "$gen_dir" ]]; then fail "vault is empty"; return 0; fi
  fi
  printf '  source: %s\n' "$(basename "$gen_dir")"
  if [[ -n "$ONLY" ]]; then
    printf '  partial restore (--only %s): dependencies of the selected packages stay current\n' "$ONLY"
  fi

  local want=0 already=0 restored=0
  local base name ver kind commit old short
  while IFS=$'\t' read -r base name ver kind commit; do
    [[ -n "$base" ]] || continue
    if [[ -n "$ONLY" ]] && [[ ",${ONLY}," != *",${name},"* ]]; then continue; fi
    want=$((want+1))
    short=""
    if [[ -n "$commit" ]]; then short=" @${commit:0:8}"; fi
    if [[ "$kind" == "vc" ]]; then
      # A :vc package's dir is named after the package with no version in it, so
      # "already present" says nothing about which revision is inside: replace it.
      if [[ -d "${D}/elpa/${base}" ]]; then
        printf '  - replacing %s (vc, locked commit%s)\n' "$base" "${short:- unknown}"
        rm -rf "${D}/elpa/${base}"
      fi
    elif [[ -d "${D}/elpa/${base}" ]]; then
      already=$((already+1)); printf '  = %s already present\n' "$base"; continue
    else
      for old in "${D}/elpa/${name}"-*; do
        [[ -e "$old" ]] || continue
        printf '  - removing current %s\n' "$(basename "$old")"
        rm -rf "$old"
      done
    fi
    if copy_tree "${gen_dir}packages/${base}" "${D}/elpa/"; then
      restored=$((restored+1)); printf '  ← restored %s%s\n' "$base" "$short"
    else
      fail "could not restore ${base}"
    fi
  done < "${gen_dir}installed.tsv"
  printf '  %d selected, %d already present, %d restored\n' "$want" "$already" "$restored"
  if (( restored > 0 )); then
    printf '  restored versions have no .eln yet — run ./emacs-sync.sh build\n'
  fi
}

cmd_doctor() {
  head1 "external tools your config calls"
  for b in node basedpyright clangd ruff prettierd prettier shfmt \
           clang-format pandoc rg make cmake sqlite3 gopls; do
    if command -v "$b" >/dev/null 2>&1; then printf '  ✓ %-20s %s\n' "$b" "$(command -v "$b")"
    else printf '  ✗ %-20s missing\n' "$b"; fi
  done
  echo ""
  # helpful hint for missing web tools
  if ! command -v prettierd >/dev/null 2>&1 && ! command -v prettier >/dev/null 2>&1; then
    printf '  ℹ️  Install prettierd for JS/CSS/HTML/Markdown formatting:\n'
    printf '     npm install -g @fsouza/prettierd\n'
  fi
  if ! command -v shfmt >/dev/null 2>&1; then
    printf '  ℹ️  Install shfmt for shell script formatting:\n'
    printf '     brew install shfmt\n'
  fi
}

# a bad state is not worth keeping
snapshot_if_clean() {
  if [[ -s "$FAIL_LOG" ]]; then
    printf '\n(skipping snapshot: %s failure(s) this run)\n' "$(wc -l < "$FAIL_LOG" | tr -d ' ')"
    return 0
  fi
  cmd_snapshot
}

case "$CMD" in
  check)           cmd_check ;;
  install)         cmd_install ;;
  upgrade)         cmd_upgrade; snapshot_if_clean ;;
  purge)           cmd_purge ;;
  build)           cmd_build ;;
  snapshot|freeze) cmd_snapshot ;;
  snapshots)       cmd_snapshots ;;
  restore)         cmd_restore ;;
  sync)
    if (( DO_UPGRADE )); then cmd_upgrade; fi
    cmd_install
    cmd_purge
    cmd_build
    snapshot_if_clean
    cmd_check ;;
  doctor)          cmd_doctor ;;
  *) sed -n '3,32p' "$0"; exit 0 ;;
esac

if [[ -s "$FAIL_LOG" ]]; then
  printf '\n✗ %s failure(s):\n' "$(wc -l < "$FAIL_LOG" | tr -d ' ')"
  sed 's/^/    /' "$FAIL_LOG"
  exit 1
fi
exit 0
