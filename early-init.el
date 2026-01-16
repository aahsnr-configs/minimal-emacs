;;; early-init.el --- Doom-Style Early Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; CRITICAL: Wrap in let to restore file-name-handler-alist
(let ((file-name-handler-alist-original file-name-handler-alist))

  ;; ====================
  ;; MAXIMUM GC DEFERRAL (Doom Strategy)
  ;; ====================
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)

  ;; Disable file name handlers completely during startup
  (setq file-name-handler-alist nil)

  ;; ====================
  ;; REDISPLAY OPTIMIZATIONS (Doom's Secret Sauce)
  ;; ====================
  (setq redisplay-skip-fontification-on-input t)  ; Skip font-lock during fast input
  (setq fast-but-imprecise-scrolling t)
  (setq inhibit-compacting-font-caches t)

  ;; ====================
  ;; FRAME OPTIMIZATION
  ;; ====================
  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)

  ;; ====================
  ;; PACKAGE SYSTEM
  ;; ====================
  (setq package-enable-at-startup nil)

  ;; ====================
  ;; NATIVE COMPILATION
  ;; ====================
  (setq native-comp-async-query-on-exit t
        native-comp-speed 2)
  ;; Ensure Emacs loads the most recent byte-compiled files.
  (setq load-prefer-newer t)

  ;; Make Emacs Native-compile .elc files asynchronously by setting
  ;; `native-comp-jit-compilation' to t.
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated



  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

  ;; ====================
  ;; UI INITIALIZATION
  ;; ====================
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(mouse-color . "white") default-frame-alist)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; ====================
  ;; STARTUP SCREEN
  ;; ====================
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-splash-screen t
        initial-scratch-message nil)

  ;; ====================
  ;; WARNINGS
  ;; ====================
  (setq warning-suppress-types '((org-element) (comp)))
  (setq warning-minimum-level :error)

  ;; ====================
  ;; SITE FILES
  ;; ====================
  (setq site-run-file nil)

  ;; ====================
  ;; RESTORE AFTER STARTUP
  ;; ====================
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-original))
            101))  ; Run late

(when (and (fboundp 'startup-redirect-eln-cache)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
