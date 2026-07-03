```org
* Core Emacs
** Lexical Binding
#+begin_src emacs-lisp
;;; init.el --- Main Configuration File -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
#+end_src

** Package Management
#+begin_src emacs-lisp
(setq load-prefer-newer t)

(setq package-user-dir (expand-file-name "var/elpa/" user-emacs-directory))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'no-littering)
  (package-install 'no-littering))

(require 'no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(no-littering-theme-backups)

;; Local personal libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-verbose nil
      use-package-expand-minimally t
      use-package-always-ensure t
      use-package-compute-statistics nil
      use-package-enable-imenu-support t)
#+end_src

** Compile Angel
#+begin_src emacs-lisp
;; Exclude anything you don't want silently compiled (adjust to taste)
;; (setq compile-angel-excluded-files-regexps '("/\\.dir-config\\.el$"))

(use-package compile-angel
  :demand t
  :config
  (setq compile-angel-verbose t)
  (push "/init.el" compile-angel-excluded-path-suffixes)
  (push "/early-init.el" compile-angel-excluded-path-suffixes)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(setq compile-angel-enable-byte-compile t)
(setq compile-angel-enable-native-compile t)

(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

(with-eval-after-load "savehist"
  (push (concat "/" (file-name-nondirectory savehist-file))
        compile-angel-excluded-path-suffixes))
(with-eval-after-load "recentf"
  (push (concat "/" (file-name-nondirectory recentf-save-file))
        compile-angel-excluded-path-suffixes))
(with-eval-after-load "cus-edit"
  (when (stringp custom-file)
    (push (concat "/" (file-name-nondirectory custom-file))
          compile-angel-excluded-path-suffixes)))

;; Only now, with exclusions registered, turn on the interception itself.
(compile-angel-on-load-mode)
#+end_src

** Custom Functions
#+begin_src emacs-lisp
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (and (not noninteractive)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (childframe-workable-p))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(defun reload-init-file ()
  "Tangle and reload the Emacs configuration from config.org.
This function is designed for use with package.el.
IMPORTANT: This does NOT fully restart Emacs. Some changes (especially
removed keybindings, hooks, or modes) will persist until you restart Emacs."
  (interactive)
  (let* ((config-org (expand-file-name "config.org" user-emacs-directory))
         (init-el (expand-file-name "init.el" user-emacs-directory))
         (start-time (current-time)))
    ;; Check if config.org exists
    (unless (file-exists-p config-org)
      (user-error "Configuration file %s not found!" config-org))
    ;; Confirm before reloading
    (when (yes-or-no-p "Reload configuration? (This won't reset removed keybindings/hooks) ")
      (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
      (message "Tangling configuration from config.org...")
      ;; Ensure Org is loaded before tangling
      (require 'org)
      ;; Tangle the org file
      (condition-case err
          (org-babel-tangle-file config-org init-el "emacs-lisp")
        (error
         (user-error "Failed to tangle config.org: %s" (error-message-string err))))
      (message "✓ Tangling complete")
      (message "Loading init.el...")
      ;; Load the tangled init file
      (condition-case err
          (load-file init-el)
        (error
         (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
         (user-error "Failed to load init.el: %s" (error-message-string err))))
      ;; Calculate load time
      (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
        (message "✓ Configuration loaded successfully")
        (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
        (message "Reload completed in %.2f seconds" elapsed-time)
        (message "")
        (message "NOTE: Some changes require a full Emacs restart:")
        (message "  • Removed keybindings, hooks, or modes")
        (message "  • Package installations/removals")
        (message "  • Major structural changes")
        (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))))

(defun my/auto-tangle-config-org ()
  "Automatically tangle config.org when saved."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "config.org" user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/auto-tangle-config-org nil t)))

(defun selected-region-or-symbol-at-point ()
  "Return the selected region, otherwise return the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))
#+end_src

** Constants
#+begin_src emacs-lisp
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/rootp
  (zerop (user-uid))
  "Are you using ROOT user?")

(defconst emacs/>=29p
  (version<= "29" emacs-version)
  "Emacs is 29 or above.")

(defconst emacs/>=29.2p
  (version<= "29.2" emacs-version)
  "Emacs is 29.2 or above.")

(defconst emacs/>=30p
  (version<= "30" emacs-version)
  "Emacs is 30 or above.")

(defconst emacs/>=31p
  (version<= "31" emacs-version)
  "Emacs is 31 or above.")
#+end_src

** Performance Tuning
#+begin_src emacs-lisp
;; Bidi optimizations (Safe for Emacs 27+)
;; Note: `bidi-display-reordering` is omitted as it is an obsolete Emacs 24 hack
;; that causes rendering glitches in modern Emacs.
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Mitigate Input Lag by disabling GTK input methods (PGTK builds only).
(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (pgtk-use-im-context nil)))))

;; Apply bidi optimizations to common modes
(defun my/bidi-optimizations ()
  "Apply bidi optimizations for current buffer."
  (setq-local bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t))

(add-hook 'org-mode-hook #'my/bidi-optimizations)
(add-hook 'text-mode-hook #'my/bidi-optimizations)
(add-hook 'prog-mode-hook #'my/bidi-optimizations)

;; Emacs 29+ Long Line Optimizations
;; Prevents Emacs from freezing when opening minified JSON or large log files.
(setq-default long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

;; Undo limits
(setq undo-limit 800000
      undo-strong-limit 1200000
      undo-outer-limit 120000000)

;; ==========================================
;; SCROLLING & REDISPLAY
;; ==========================================
;; Note: `fast-but-imprecise-scrolling`, `redisplay-skip-fontification-on-input`,
;; `inhibit-compacting-font-caches`, and `frame-resize-pixelwise` are already
;; set in early-init.el. We do not duplicate them here.
(setq auto-window-vscroll nil            ;; Prevents scrolling jitter
      highlight-nonselected-windows nil
      window-resize-pixelwise nil
      byte-compile-warnings '(not obsolete)
      native-comp-async-report-warnings-errors 'silent)

;; Disable blink-cursor-mode properly (setq doesn't turn off the minor mode)
(blink-cursor-mode -1)

;; ==========================================
;; DOOM-INSPIRED OPTIMIZATIONS
;; (Crucial for lsp-mode + flycheck)
;; ==========================================

;; 1. Clipboard Spam Prevention (CRITICAL for Wayland/X11)
;; Prevents Emacs from making OS-level DBus/X11 API calls every time
;; you highlight text with the mouse/keyboard, eliminating micro-stutters.
(setq select-active-regions nil)

;; 2. Faster File Opening
;; Skips expensive case-insensitive regex matching when checking
;; `auto-mode-alist` to determine a file's major mode.
(setq auto-mode-case-fold nil)

;; 3. Typing Fluidity (JIT Lock Deferral)
;; Defers syntax highlighting by 50ms. This prevents font-lock and
;; lsp-mode semantic tokens from blocking the main thread during rapid typing.
(setq jit-lock-defer-time 0.05)

;; 4. Prevent GTK Tooltip Hangs (PGTK/GTK builds)
;; Forces Emacs to use its own internal tooltips instead of calling out
;; to the GTK DBus tooltip API, which can hang the UI on hover.
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))
#+end_src

** Small Configs
#+begin_src emacs-lisp
;; Explicitly disable bzr, svn, hg to prevent TRAMP errors
(setq vc-handled-backends '(Git))

;; Automatically follow symlinks to version-controlled files (suppress prompt)
(setq vc-follow-symlinks t)

;; Confirmation settings
(setq confirm-kill-processes nil)  ;; Auto-kill processes on exit

;; Faster echo of keystrokes
(setq echo-keystrokes 0.1)

;; Don't create lockfiles
(setq-default create-lockfiles nil)

;; Compilation settings
(setq-default compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t)

;; Suppress redefinition warnings
(setq ad-redefinition-action 'accept)

;; Require final newline
(setq require-final-newline t)

;; Enable commands
(put 'erase-buffer 'disabled nil)

(delete-selection-mode 1)

(setq window-sides-vertical t)

;; File associations
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

(setq use-dialog-box nil
      use-file-dialog nil)

(setq save-interprogram-paste-before-kill t)
#+end_src

** Auto Save
#+begin_src emacs-lisp
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)
#+end_src

** Garbage Collector
#+begin_src emacs-lisp
(use-package gcmh
  :unless (fboundp 'igc-info)
  :hook (after-init . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024)))

(add-hook 'focus-out-hook #'garbage-collect)
#+end_src

** UTF-8 Coding System
#+begin_src emacs-lisp
(set-language-environment "UTF-8")

;; Explicitly set fallback and default boundaries to UTF-8.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(prefer-coding-system 'utf-8)
#+end_src

** Session Management
#+begin_src emacs-lisp
;; Save place
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))

;; History with symlink resolution
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)

  ;; Keep recentf from listing no-littering's own housekeeping files.
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))

  (add-to-list 'recentf-exclude #'file-remote-p)

  (setq recentf-filename-handlers '(abbreviate-file-name))

  (defvar ar/recentf-org-agenda-cache nil
    "Cached list of resolved org-agenda file paths.")

  (defun ar/update-recentf-org-agenda-cache ()
    "Update the recentf exclusion cache for org-agenda files."
    (when (bound-and-true-p org-agenda-files)
      (setq ar/recentf-org-agenda-cache
            (mapcar #'file-truename
                    (seq-filter #'file-exists-p (org-agenda-files t))))))

  (defun ar/exclude-org-agenda-files-p (file)
    "Return t if FILE is an org-agenda file, using the cached list."
    (and (not (file-remote-p file))
         (string-suffix-p ".org" file t)
         (when ar/recentf-org-agenda-cache
           (member (file-truename file) ar/recentf-org-agenda-cache))))

  (add-to-list 'recentf-exclude #'ar/exclude-org-agenda-files-p)

  ;; Update cache lazily when org files are opened or on idle timer
  (add-hook 'org-mode-hook #'ar/update-recentf-org-agenda-cache)
  (run-with-idle-timer 30 t #'ar/update-recentf-org-agenda-cache))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables '(global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))
#+end_src

** Misc
#+begin_src emacs-lisp
(use-package simple
  :ensure nil
  :hook ((text-mode . visual-line-mode))
  :init
  ;; FIX: These are minor modes, not variables. `setq` does not enable them.
  (column-number-mode 1)
  (line-number-mode 1)

  (setq kill-whole-line t
        line-move-visual nil
        track-eol t
        set-mark-command-repeat-pop t)

  (setq-default show-trailing-whitespace nil)

  ;; FIX: Removed `delete-trailing-whitespace` from `before-save-hook`.
  ;; You already have `stripspace` configured later in your config!
  ;; `stripspace` is vastly superior because it only cleans modified lines,
  ;; preventing cursor-jumping and preserving Git blame history.

  ;; Prettify the process list
  (with-no-warnings
    (defun my-list-processes--prettify ()
      (when-let* ((entries tabulated-list-entries))
        (setq tabulated-list-entries nil)
        (dolist (p (process-list))
          (when-let* ((val (cadr (assoc p entries)))
                      (name (aref val 0))
                      (pid (aref val 1))
                      (status (aref val 2))
                      (status (list status 'face
                                    (if (memq status '(stop exit closed failed)) 'error 'success)))
                      (buf-label (aref val 3))
                      (tty (list (aref val 4) 'face 'font-lock-doc-face))
                      (thread (list (aref val 5) 'face 'font-lock-doc-face))
                      (cmd (list (aref val 6) 'face 'completions-annotations)))
            (push (list p (vector name pid status buf-label tty thread cmd))
                  tabulated-list-entries)))))
    (advice-add #'list-processes--refresh :after #'my-list-processes--prettify)))

;; Misc Settings
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

;; Silence the Bell
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; Suppress "End/Beginning of buffer" Messages
(setq command-error-function
      (lambda (data context caller)
        (when (not (memq (car data) '(beginning-of-buffer end-of-buffer)))
          (command-error-default-function data context caller))))

;; Removes <dir> style
(setq uniquify-buffer-name-style 'forward)

;; Other Misc
;; Inside your ** Misc block, update the "Other Misc" section:
(setq delete-by-moving-to-trash t
      make-backup-files nil
      auto-save-default nil
      adaptive-fill-regexp "[ \t]*\\(>[ \t]*\\|[-–—]\\|[•‣*]\\|[0-9]+[.)]\\)[ \t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)
#+end_src

** Setup User
#+begin_src emacs-lisp
(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")
#+end_src

** Title
#+begin_src emacs-lisp
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)
#+end_src


```
