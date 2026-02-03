;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;;; Runs before init.el for maximum startup performance.
;;; Handles GC, UI suppression, and native-comp settings.

;;; Code:

;; --- GC: Set to maximum during init ---
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; --- Disable package.el (we use straight.el) ---
(setq package-enable-at-startup nil)

;; --- Suppress UI elements early to prevent flicker ---
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Prevent frame resize on font/UI changes during init
(setq frame-inhibit-implied-resize t)

;; --- Temporarily disable file-name-handler-alist for faster loading ---
(defvar my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/saved-file-name-handler-alist)))

;; --- Native compilation settings (Emacs 28+) ---
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation t))

;; --- Suppress cosmetic warnings during init ---
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

(provide 'early-init)
;;; early-init.el ends here
