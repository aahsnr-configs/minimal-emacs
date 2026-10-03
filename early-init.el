;;; early-init.el --- Pre-startup GC, process-I/O, package and frame tuning -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by Emacs 31.1 before init.el, before package activation and
;; before the first frame.  One section per topic below.  This file and
;; init.el are the only two entry points Emacs loads directly; every
;; feature-providing module lives in lisp/.

;;; Code:

;; Loaded before init.el, package activation and the first frame.

;; GC deferral (research-packages §4: the ~0.8 MB default costs 5 GCs/~29 ms
;; on one heavy load — the settings below keep startup at ≤1 collection).
;; Interactively GC is disabled until the restore hook at the bottom of this
;; file; batch caps at 256 MB so smoke-test installs cannot grow an unbounded
;; heap (minimal-emacs.d pattern, research-packages §4/§5).
(setopt gc-cons-threshold (if noninteractive (* 256 1024 1024) most-positive-fixnum)
        gc-cons-percentage 1.0)

;; Process I/O and load discipline.  read-process-output-max 4 MiB is parity
;; with the old early-init.el:16 (the research floor is 1 MiB; 4 MiB has run
;; fine for a year and keeps JSON-RPC/ripgrep output off the main thread).
;; load-prefer-newer makes a stale .elc never win over fresher source
;; (research-packages §5).
(setopt read-process-output-max (* 4 1024 1024)
        load-prefer-newer t)

;; Package startup: package-enable-at-startup is consulted before init.el
;; runs, so it must be nil here — straight.el owns packages entirely and
;; no package.el activation happens in any session.  The eln-cache
;; follows `user-emacs-directory' into var/ automatically (see the
;; directory swap below).  Silence async native-compilation reports:
;; byte-compile warnings from straight-built packages must not pop up in
;; *Warnings* at startup ('silent still logs to the *Async Native
;; Compilation Log* buffer for debugging).
(defvar native-comp-async-report-warnings-errors)
(setq package-enable-at-startup nil
      native-comp-async-report-warnings-errors 'silent)

;; lsp-mode's plist deserialization mode.  The variable is a plain defvar
;; inside `eval-and-compile' in lsp-protocol.el whose initial value is read
;; from the environment (lsp-protocol.el:37-46, quoted in
;; research2/lsp-facts.md §1.3), and lsp-mode's generated accessor macros
;; branch on the COMPILE-TIME value (lsp-protocol.el:76-86, §1.4): the value
;; must therefore be set before straight builds lsp-mode (this file runs
;; before init.el's straight pass) and stays set at run time; after any
;; change, rebuild (M-x straight-rebuild-package RET lsp-mode) — the docs'
;; "delete and reinstall" step (performance.md:41-47).
(setenv "LSP_USE_PLISTS" "true")

;; State and configuration directories — decluttering without the
;; no-littering package, following minimal-emacs.d's documented
;; approach: `user-emacs-directory' itself is swapped to var/ so that
;; EVERYTHING deriving from it lands under var/ automatically —
;; straight's repositories and builds, the eln-cache (including advice
;; trampolines, which are compiled against `user-emacs-directory' and
;; would otherwise land in the configuration root), histories,
;; auto-saves — while `ar/config-dir' keeps pointing at the real
;; configuration root for source files.  This lives in early-init.el on
;; purpose: the swap must be active before init.el bootstraps straight
;; and before any package is installed.  `snippets/' deliberately stays
;; at the configuration root (it holds user content, see
;; my/snippets-directory in dev-config).
(defvar ar/config-dir (file-name-as-directory user-emacs-directory)
  "The real configuration root: source files, lisp/, snippets/, etc/.")
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
;; warnings; each package's own defvar/defcustom (evaluated when that
;; package loads) keeps the values set here.
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

(dolist (dir (list ar/etc-dir ar/var-dir
                   (ar/var-file "backup/")
                   (ar/var-file "autosave/")
                   (ar/var-file "tramp-autosave/")
                   (ar/var-file "url/")
                   (ar/var-file "transient/")
                   (ar/var-file "eshell/")
                   (ar/var-file "org/")
                   (ar/var-file "eln-cache/")))
  (make-directory dir t))

;; The native-comp machinery writes advice trampolines into
;; `<config-root>/eln-cache/' through a code path that neither the
;; `user-emacs-directory' swap above nor `startup-redirect-eln-cache'
;; can influence (verified on 31.1: trampolines land at the original
;; location in every routing scheme).  Keep the configuration root
;; clean by making that one path a symlink into var/eln-cache — writes
;; follow the link into var/, and the root keeps a single pointer
;; instead of a cache directory.
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

;; straight's `straight-base-dir' defaults to `user-emacs-directory',
;; which is now var/ — repositories and builds land in var/straight/.

;; The user's custom-file belongs to etc/.
(setopt custom-file (ar/etc-file "custom.el"))

;; Backups, auto-saves, and per-package state files into var/ (the
;; per-variable set follows minimal-emacs.d's native routing).  core-config
;; disables make-backup-files and auto-save-default (super-save owns the
;; save strategy), but the routing is in place for anyone re-enabling them.
;; Lockfiles are already disabled (create-lockfiles nil in core-config).
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
   (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Deterministic startup, quiet warnings.  site-run-file: since 31.1
;; site-start.el is processed BEFORE this file (NEWS:55-59; startup.el loads
;; site-run at :1526 and early-init at :1540), so nil'ing it here no longer
;; suppresses the site load — kept per design §4.1 as parity, and it must be
;; setq: site-run-file's custom setter errors ("Customizing `site-run-file'
;; does not work", startup.el:381).  warning-suppress-types is parity with the
;; old early-init.el:91: org-element's byte-comp noise and native-comp *display*
;; warnings stay off without touching async-comp behavior.
(setq site-run-file nil
      warning-suppress-types '((org-element) (comp)))

;; Defer file-name handlers (Tramp, epa, jka-compr) until startup finishes:
;; save/nil now, lexically capture and restore the original alist together
;; with the GC steady state (research-packages §4: restore, never leave nil).
;; emacs-startup-hook does not run in batch, which is correct here: batch
;; harness runs (package bootstrap, byte-compilation) touch
;; only local files, and built-in .elc files win the load-prefer-newer tie
;; against same-mtime .el.gz sources, so no decompression handler is needed.
;; DEPTH 101 runs the restore after user startup hooks (old early-init.el:103).
(let ((fnha file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist fnha
                    gc-cons-threshold 100000000 ; 100 MB steady state, must match ar/gc-steady-cons-threshold
                    gc-cons-percentage 0.1))
            101))

;; Display-only startup tuning — guarded so batch (which -l's this file) is a
;; no-op here.  Every form is a pure variable write needing no frame, so
;; daemon and TTY sessions still take it (the old early-init.el set these
;; unconditionally).  Menu/tool/scrollbar stripping goes through
;; default-frame-alist BEFORE the first frame — cheaper than -1'ing the modes.
;; frame-inhibit-implied-resize stays t rather than NEWS:488's 'force for
;; tiling WMs — parity (design §4.1).  redisplay-skip-fontification-on-input
;; is the corrected spelling of design.md:305 ("fontication" — that variable
;; does not exist in 31.1; old early-init.el:32 had the right name).  Dropped
;; from the old file per design §6: the mouse-color frame parameter.  (The
;; LSP_USE_PLISTS setenv is no longer dropped -- lsp-mode replaced eglot and
;; the setenv is restored in the package-startup section; design-lsp §9.)
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
