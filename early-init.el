;;; early-init.el --- Early Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ====================
;; MAXIMUM GC DEFERRAL & SUBPROCESS OPTIMIZATION
;; ====================
;; These are global settings and belong OUTSIDE the `let` block, as they
;; have nothing to do with lexical variable capture.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; Bumped to 4MB. Default is 4KB, which causes the main thread to block
;; and stutter when reading massive JSON-RPC payloads from lsp-mode,
;; ripgrep, or package archives during startup and general usage.
(setq read-process-output-max (* 4 1024 1024))

;; ====================
;; FILE NAME HANDLERS (Lexical Capture)
;; ====================
;; We use `let` strictly to lexically capture the original handler alist
;; so we can restore it in the `emacs-startup-hook` closure below.
(let ((file-name-handler-alist-original file-name-handler-alist))

  ;; Disable file name handlers completely during startup to prevent
  ;; regex checks on every file path (e.g. TRAMP, magic modes).
  (setq file-name-handler-alist nil)

  ;; ====================
  ;; REDISPLAY OPTIMIZATIONS
  ;; ====================
  (setq redisplay-skip-fontification-on-input t)
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

  ;; Pre-compute load paths and autoloads into a single compiled file
  ;; to bypass expensive directory iteration during startup.
  ;;(setq package-quickstart t)

  (setq package-enable-at-startup nil)
  ;; ====================
  ;; NATIVE COMPILATION
  ;; ====================
  (when (and (fboundp 'startup-redirect-eln-cache)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

  ;; ====================
  ;; UI INITIALIZATION
  ;; ====================
  ;; Disable UI elements via default-frame-alist BEFORE the first frame is drawn.
  ;; This is significantly faster than calling (menu-bar-mode -1) etc.
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars . nil) default-frame-alist)
  (push '(mouse-color . "white") default-frame-alist)
  (push '(internal-border-width . 1) default-frame-alist)

  ;; ====================
  ;; STARTUP SCREEN
  ;; ====================
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        initial-scratch-message nil)

  ;; ====================
  ;; WARNINGS
  ;; ====================
  (setq warning-suppress-types '((org-element) (comp)))

  ;; ====================
  ;; SITE FILES
  ;; ====================
  (setq site-run-file nil)


  (setenv "LSP_USE_PLISTS" "true")
  ;; ====================
  ;; RESTORE AFTER STARTUP
  ;; ====================
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-original)
              (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                    gc-cons-percentage 0.1))
            101))  ; Run late

;;; early-init.el ends here
