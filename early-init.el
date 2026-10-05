;;; early-init.el --- Pre-startup GC, process-I/O, package and frame tuning -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by Emacs 31.1 before init.el, before package activation and
;; before the first frame.  This file and init.el are the only two entry
;; points Emacs loads directly; every feature-providing module lives in
;; lisp/ and is wired by init.el.

;;; Code:

;; GC deferral: unlimited threshold during startup (target ≤1 collection),
;; capped in batch so smoke-test installs cannot grow an unbounded heap.
;; Steady state is owned by gcmh once it activates (core module).
(setopt gc-cons-threshold (if noninteractive (* 256 1024 1024) most-positive-fixnum)
        gc-cons-percentage 1.0)

;; Process I/O and load discipline: 4 MiB keeps JSON-RPC/ripgrep output off
;; the main thread; a stale .elc never wins over fresher source.
(setopt read-process-output-max (* 4 1024 1024)
        load-prefer-newer t)

;; Package startup: straight.el owns packages entirely; no package.el
;; activation in any session.  Silence async native-compilation report
;; popups (still logged to the *Async Native Compilation Log* buffer).
(defvar native-comp-async-report-warnings-errors)
(setq package-enable-at-startup nil
      native-comp-async-report-warnings-errors 'silent)

;; lsp-mode's plist deserialization: lsp-mode branches on the COMPILE-TIME
;; value, so it must be set in the environment before straight builds
;; lsp-mode.  After changing it: M-x straight-rebuild-package RET lsp-mode.
(setenv "LSP_USE_PLISTS" "true")

;; State and configuration directories — decluttering without no-littering:
;; `user-emacs-directory' itself is swapped to var/ so EVERYTHING deriving
;; from it (straight repos/builds, eln-cache, histories, autosaves) lands
;; under var/ automatically, while `ar/config-dir' keeps pointing at the
;; real configuration root.  snippets/ deliberately stays at the root (user
;; content).
(defvar ar/config-dir (file-name-as-directory (file-truename user-emacs-directory))
  "The real configuration root: source files, lisp/, snippets/, etc/.
Truenamed so every derived path survives symlinked homes (e.g. /home
-> /var/home on Silverblue) -- git, magit and external tools resolve
to the canonical path regardless of how the session was started.")
(setq user-emacs-directory (expand-file-name "var/" ar/config-dir))
(defvar ar/etc-dir (expand-file-name "etc/" ar/config-dir)
  "Directory for configuration files a user might read or edit.")
(defvar ar/var-dir (file-name-as-directory user-emacs-directory)
  "Directory for machine-written state: packages, caches, histories.")

(defun ar/etc-file (name)
  "Expand NAME relative to `ar/etc-dir'."
  (expand-file-name name ar/etc-dir))

(defun ar/var-file (name)
  "Expand NAME relative to `ar/var-dir'."
  (expand-file-name name ar/var-dir))

;; Declared so the routing setq below byte-compiles without free-variable
;; warnings; each package's own defcustom (evaluated when that package
;; loads) keeps the values set here.
(defvar tramp-backup-directory-alist)
(defvar tramp-auto-save-directory)
(defvar recentf-save-file)
(defvar savehist-file)
(defvar save-place-file)
(defvar bookmark-default-file)
(defvar tramp-persistency-file-name)
(defvar url-configuration-directory)
(defvar transient-history-file)
(defvar transient-values-file)
(defvar transient-levels-file)
(defvar nsm-settings-file)
(defvar eshell-directory-name)
(defvar abbrev-file-name)
(defvar gcmh-high-cons-threshold)

(dolist (dir (list ar/etc-dir ar/var-dir
                   (ar/var-file "backup/")
                   (ar/var-file "autosave/")
                   (ar/var-file "tramp-autosave/")
                   (ar/var-file "url/")
                   (ar/var-file "transient/")
                   (ar/var-file "eshell/")
                   (ar/var-file "org/")
                   (ar/var-file "persp/")
                   (ar/var-file "eln-cache/")))
  (make-directory dir t))

;; The native-comp machinery writes advice trampolines into
;; `<config-root>/eln-cache/' through a code path neither the directory swap
;; nor `startup-redirect-eln-cache' can influence.  Keep the root clean by
;; making that one path a symlink into var/eln-cache.
(let ((link (expand-file-name "eln-cache" ar/config-dir))
      (target (ar/var-file "eln-cache/")))
  (when (native-comp-available-p)
    (if (file-symlink-p link)
        (unless (string= (file-truename link) (file-truename target))
          (delete-file link)
          (make-symbolic-link target link))
      (when (file-directory-p link)
        (delete-directory link 'recursive)) ;; stale cache; regenerable
      (make-symbolic-link target link))))

;; straight's `straight-base-dir' defaults to `user-emacs-directory', which
;; is now var/ -- repositories and builds land in var/straight/.

;; The user's custom-file belongs to etc/.
(setopt custom-file (ar/etc-file "custom.el"))

;; Backups, auto-saves, and per-package state files into var/ (core module
;; disables backups/auto-save in favor of super-save; routing stays in place
;; for anyone re-enabling them).
(setq backup-directory-alist `(("." . ,(ar/var-file "backup")))
      tramp-backup-directory-alist backup-directory-alist
      auto-save-list-file-prefix (ar/var-file "autosave/")
      tramp-auto-save-directory (ar/var-file "tramp-autosave/")
      recentf-save-file (ar/var-file "recentf-save.el")
      savehist-file (ar/var-file "savehist.el")
      save-place-file (ar/var-file "places")
      bookmark-default-file (ar/var-file "bookmarks")
      tramp-persistency-file-name (ar/var-file "tramp")
      url-configuration-directory (ar/var-file "url/")
      transient-history-file (ar/var-file "transient/history.el")
      transient-values-file (ar/var-file "transient/values.el")
      transient-levels-file (ar/var-file "transient/levels.el")
      nsm-settings-file (ar/var-file "network-security.data")
      eshell-directory-name (ar/var-file "eshell/")
      abbrev-file-name (ar/etc-file "abbrev_defs"))
(when (and (fboundp 'startup-redirect-eln-cache) (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename (ar/var-file "eln-cache/"))))

;; Deterministic startup, quiet warnings.  Since 31.1 site-start.el runs
;; BEFORE this file, nil'ing it here is parity only; it must be setq (its
;; custom setter errors).  (org-element) and (comp) byte-comp noise stays off.
(setq site-run-file nil
      warning-suppress-types '((org-element) (comp)))

;; Defer file-name handlers (Tramp, epa, jka-compr) until startup finishes;
;; restore the original alist together with the GC handoff to gcmh.  Batch
;; never defers: startup hooks do not fire there, so nil'ing the alist
;; would leave Tramp/jka-compr/epa dead for the whole batch session.
(defvar ar/startup-file-name-handler-alist file-name-handler-alist
  "Original `file-name-handler-alist', captured before startup deferral.")

(defun ar/startup-restore-file-name-handlers ()
  "Restore `file-name-handler-alist' and settle GC after startup."
  (setq file-name-handler-alist ar/startup-file-name-handler-alist
        gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.1))

(unless noninteractive
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook #'ar/startup-restore-file-name-handlers 101))

;; Display-only startup tuning -- guarded so batch (which -l's this file) is
;; a no-op here.  Menu/tool/scrollbar stripping goes through
;; default-frame-alist BEFORE the first frame.
(unless noninteractive
  (setq redisplay-skip-fontification-on-input t
        fast-but-imprecise-scrolling t
        inhibit-compacting-font-caches t
        frame-inhibit-implied-resize t
        frame-resize-pixelwise t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        initial-scratch-message nil)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars . nil) default-frame-alist)
  (push '(internal-border-width . 1) default-frame-alist))

;; Entry-point marker; nothing ever requires this feature.
(provide 'early-init)
;;; early-init.el ends here
