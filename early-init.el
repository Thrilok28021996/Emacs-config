;;; early-init.el --- Pre-package GC + UI suppression + startup optimizations -*- lexical-binding: t; -*-

;; Suppress file-name handlers during startup to avoid regex matching on every loaded file
(defvar my/file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Maximize GC during startup — reset after init completes
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist-backup)))

;; Always prefer newer source file (.el) over byte-compiled (.elc)
(setq load-prefer-newer t)

;; Inhibit font cache compacting to prevent micro-stutters during GC on macOS
(setq inhibit-compacting-font-caches t)

;; Suppress UI before frame draws (avoids flash)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . fullboth) default-frame-alist)

(setq package-enable-at-startup nil)
