;;; early-init.el --- Pre-package GC + UI suppression -*- lexical-binding: t; -*-

;; Maximize GC during startup — reset after init completes
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Suppress UI before frame draws (avoids flash)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . fullboth) default-frame-alist)

(setq package-enable-at-startup nil)
