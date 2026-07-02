;;; early-init.el --- Early Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; CRITICAL: Wrap in let to restore file-name-handler-alist
(let ((file-name-handler-alist-original file-name-handler-alist))

  ;; ====================
  ;; MAXIMUM GC DEFERRAL (startup-optimization trick, framework-agnostic)
  ;; ====================
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0)

  ;; Disable file name handlers completely during startup
  (setq file-name-handler-alist nil)

  ;; ====================
  ;; REDISPLAY OPTIMIZATIONS
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
  ;; package-enable-at-startup nil means Emacs will NOT auto-call
  ;; `package-initialize' before loading init.el. Because of that, init.el
  ;; MUST call `(package-initialize)' itself, after setting `package-archives'
  ;; (which must include "melpa" -- no-littering is not on GNU/NonGNU ELPA)
  ;; and BEFORE `(require 'no-littering)'. This is not optional bookkeeping;
  ;; without it, `package-install' has nothing to install into/from.
  (setq package-enable-at-startup nil)

  ;; ====================
  ;; NATIVE COMPILATION
  ;; ====================

  ;; Redirect the eln-cache into the no-littering var/ tree. This has to
  ;; happen here, in early-init.el, before native compilation of anything
  ;; (including no-littering itself) can occur -- no-littering can't theme
  ;; this variable for us because it isn't installed/loaded yet at this
  ;; point in startup. The path below is written out by hand to match what
  ;; `no-littering-var-directory' will resolve to once init.el runs
  ;; (default: "var/" under `user-emacs-directory'). If you override
  ;; `no-littering-var-directory' in init.el, update this path to match.
  (when (and (fboundp 'startup-redirect-eln-cache)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

  ;; ;; Make Emacs native-compile .elc files asynchronously by setting
  ;; ;; `native-comp-jit-compilation' to t.
  ;; ;; Left commented out deliberately: this is Emacs's own default since
  ;; ;; 29+, and compile-angel.el (configured separately in init.el) expects
  ;; ;; it to stay at its default rather than being disabled.
  ;; (setq native-comp-jit-compilation t)
  ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

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

  ;; (setq inhibit-message t)

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
              (setq file-name-handler-alist file-name-handler-alist-original)
              (setq inhibit-message nil)
              )
            101))  ; Run late

;;; early-init.el ends here
