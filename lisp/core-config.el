;;; core-config.el --- Core behavior, performance tuning and shared helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; The core module: editor defaults, gcmh GC management, and the shared
;; predicates/macros (`ar/icons-displayable-p', `with-eval-after-load-all',
;; `too-long-file-p', ...) that every later module builds on.  Also owns
;; `reload-init-file', which re-evaluates all module files after edits.

;;; Code:

(declare-function windmove-find-other-window "windmove")
(declare-function pgtk-use-im-context "pgtk")
(declare-function org-in-src-block-p "org")

(defvar global-hl-line-overlay)
(defvar evil-visual-state-entry-hook)
(defvar evil-visual-state-exit-hook)

;; Shared predicates and macros.  Keep this block first: every later module
;; (and every later block here) may call these helpers.

;; Customization group for the `ar/' namespace; later modules add their own
;; defcustoms to it.
(defgroup ar nil
  "Customization group for this configuration's `ar/' options."
  :group 'convenience)

(defun ar/icons-displayable-p ()
  "Return non-nil if the `nerd-icons' icon font is displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

;; Size thresholds that suspend expensive minor modes in huge buffers.
(defun too-long-file-p ()
  "Return non-nil if the current buffer exceeds size thresholds.
Backing guard for `ar/define-large-file-guard': keeps fontification,
folding and line numbers from freezing the main thread on generated
files, minified JSON and logs."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(defmacro ar/define-large-file-guard (mode)
  "Advise minor-mode command MODE to refuse activation in huge buffers.
A negative or zero prefix argument bypasses the guard so the mode can
still be toggled deliberately."
  `(define-advice ,mode (:before-while (&optional arg) guard-large-files)
     (or (and arg (< (prefix-numeric-value arg) 1))
         (not (too-long-file-p)))))

(defmacro with-eval-after-load-all (features &rest body)
  "Run BODY once every feature in FEATURES has loaded.
FEATURES is a single feature or an unquoted list of features.  BODY
runs exactly once, the moment the LAST of FEATURES to load actually
finishes loading.  BODY is automatically wrapped in `with-no-warnings'.

\(fn FEATURES BODY...)"
  (declare (indent 1) (debug (form body)))
  (when (eq (car-safe features) 'quote)
    (setq features (cadr features)))
  (unless (listp features)
    (setq features (list features)))
  (setq features (delete-dups (copy-sequence features)))
  (unless features
    (error "FEATURES must not be empty for `with-eval-after-load-all'"))
  (dolist (f features)
    (unless (or (symbolp f) (stringp f))
      (error "Invalid feature %S for `with-eval-after-load-all' (must be a symbol or string)" f)))
  (let ((form `(with-no-warnings ,(macroexp-progn body))))
    (dolist (f (reverse features))
      (setq form `(with-eval-after-load ,(if (stringp f) f `',f) ,form)))
    form))

;; Unified escape: clear search highlights, then abort the minibuffer or the
;; pending command.  Bound to `<escape>' in normal/visual/motion states and
;; all minibuffer maps by the keybinds module.
(defun ar/escape-key ()
  "Clear search highlights, abort recursive edits, or quit pending commands."
  (interactive)
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))
  (if (minibufferp)
      (abort-recursive-edit)
    (keyboard-quit)))

;; Swap a horizontal split to vertical or vice versa.
(defun ar/window-toggle-split-direction ()
  "Switch window split from horizontal to vertical, or vice versa."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbor-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbor1 (windmove-find-other-window neighbor-dir win))
               (neighbor2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbor-dir next-win)))))
          (setq done (and next-win
                          (window-live-p next-win)
                          (not (window-minibuffer-p next-win))
                          (eq neighbor1 neighbor2)
                          (not (window-minibuffer-p neighbor2))))
          (when done
            (let* ((other-buf (window-buffer next-win)))
              (delete-window next-win)
              (if (eq nextdir 'right)
                  (split-window-below)
                (split-window-right))
              (set-window-buffer (next-window) other-buf))))))))

;; Reload entry point.  init.el and the lisp/ module files are the whole
;; source of truth; stale .elc artifacts are removed before reloading so
;; `load-prefer-newer' cannot resurrect them.
(defvar ar-modules) ;; bound by init.el with the ordered module feature list
(defvar ar/config-dir) ;; the real configuration root, set by early-init.el

(defun reload-init-file ()
  "Re-evaluate every module file in `ar-modules'.
early-init.el and init.el are deliberately excluded: the bootstrap they
own (package bootstrap, module require order) must run exactly once per
session.  Loads the lisp/ sources directly -- never the var/bytecode
cache, whose stale artifacts are exactly what a reload must bypass.
Some changes (removed hooks, packages) still require a full restart."
  (interactive)
  (let ((start-time (current-time))
        (lisp-dir (expand-file-name "lisp" ar/config-dir)))
    (dolist (mod ar-modules)
      (let ((src (expand-file-name (concat (symbol-name mod) ".el") lisp-dir)))
        (unless (file-exists-p src)
          (user-error "Reload: %s not found in %s" mod lisp-dir))
        (message "Reloading %s..." mod)
        (load src nil 'nomessage)))
    (message "Reload finished in %.2fs -- restart Emacs for package changes."
             (float-time (time-subtract (current-time) start-time)))))

;; Performance and garbage collection.  Startup-time GC deferral is owned
;; by early-init.el; this block hands steady-state control to gcmh (Doom
;; core parity: 64 MB high threshold, auto idle delay x10).
(defvar gcmh-high-cons-threshold)
(setq gcmh-high-cons-threshold (* 64 1024 1024))

(use-package gcmh
  :defer t
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10))

(defun ar/gcmh-activate-after-startup ()
  "Hand steady-state GC to gcmh once startup completes."
  (unless (bound-and-true-p gcmh-mode)
    (when (require 'gcmh nil t)
      (gcmh-mode 1))))

;; Activate gcmh right after startup completes (batch runs skip it).
(unless noninteractive
  (add-hook 'emacs-startup-hook #'ar/gcmh-activate-after-startup 102))

;; Long-line optimizations (Emacs 29+): keep redisplay linear on minified
;; JSON, logs and generated code.
(setq-default long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

;; Predictable LTR redisplay: no per-paragraph bidi search on this
;; Latin/CJK workload.
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Expanded undo limits for large refactoring operations.
(setopt undo-limit 800000
        undo-strong-limit 1200000
        undo-outer-limit 120000000)

;; Scrolling and redisplay: viewport stays put while typing, font-lock defers
;; 50 ms so input wins, and case folding is skipped while picking major modes.
(setopt highlight-nonselected-windows nil
        window-resize-pixelwise nil
        jit-lock-defer-time 0.05
        auto-mode-case-fold nil)
(setq auto-window-vscroll nil)

;; Platform layer: PGTK/Wayland workarounds, daemon plumbing, UTF-8 on every
;; I/O boundary, and identity.  All of it is batch-safe.

;; Disable the GTK input-method context per frame: it adds input latency
;; under Wayland (fcitx5/ibus).
(defun ar/pgtk-disable-im-context (frame)
  "Turn off the GTK input-method context on FRAME."
  (with-selected-frame frame
    (pgtk-use-im-context nil)))
(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions #'ar/pgtk-disable-im-context)
  ;; The hook never fires for the initial frame of a non-daemon session.
  (when (eq window-system 'pgtk)
    (pgtk-use-im-context nil)))

;; Internal Emacs tooltips: the GTK DBus tooltip path hangs on hover with
;; this PGTK/Wayland stack.
(setopt x-gtk-use-system-tooltips nil)

;; No DBus/X11 selection chatter while text is highlighted.
(setopt select-active-regions nil)

;; Native dialog boxes freeze the daemon when triggered from emacsclient
;; sessions; everything is prompted through the minibuffer instead.
(setopt use-dialog-box nil
        use-file-dialog nil)

;; GPG passphrases are prompted in the minibuffer (`loopback') instead of a
;; GTK pinentry dialog.
(setopt epg-pinentry-mode 'loopback)

;; Native compilation: defaults stay (native-comp-speed 2 + the 30.1+
;; important-warnings filter).  Only the *display* of async-comp warnings
;; is quieted; this idempotent push covers reload paths.
(unless (member '(comp) warning-suppress-types)
  (push '(comp) warning-suppress-types))

;; Daemon plumbing: load server.el eagerly in daemon sessions so
;; `server-after-make-frame-hook' is a live list from the first client
;; frame.  The server itself is started by the systemd user unit /
;; emacsclient, never from the config.
(when (daemonp)
  (require 'server))

;; UTF-8 on every I/O boundary.  The Windows selection guard is kept so the
;; config stays portable.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; Identity: used by Git commits, Org exports and capture templates.
(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

;; Frame and icon titles show the buffer name.
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

;; Small quality-of-life defaults: VCS scope, process safety, filesystem
;; hygiene, buffer-list conventions and file associations.

;; Only Git is handled: probing bzr/svn/hg on remote files causes TRAMP I/O
;; stutter and SSH timeouts.
(setopt vc-handled-backends '(Git)
        vc-follow-symlinks t)

;; Process and compilation safeguards.
(setopt confirm-kill-processes nil
        compilation-ask-about-save nil)
(setq compilation-always-kill t
        compilation-scroll-output t)

;; Silence advice-redefinition noise; echo keystrokes responsively.
(setopt ad-redefinition-action 'accept
        echo-keystrokes 0.1)

;; Filesystem hygiene: no lockfiles (they fight file watchers), no backup
;; clutter, no auto-save files (super-save owns the save strategy), and a
;; final newline always ensured.
(setopt create-lockfiles nil
        make-backup-files nil
        auto-save-default nil
        delete-by-moving-to-trash t)
(setq-default require-final-newline t)

;; Re-enable dangerous-by-default commands that are useful in a power setup.
(put 'erase-buffer 'disabled nil)

;; Core minor modes and window layout.
(delete-selection-mode 1)
(setopt window-sides-vertical t)

;; Keep the system clipboard: external clipboard content is saved to the
;; kill-ring before any kill operation overwrites it.
(setopt save-interprogram-paste-before-kill t)

;; `q' buries read-only list buffers to match Vim muscle memory.
(use-package tabulated-list
  :defer t
  :config
  (keymap-set tabulated-list-mode-map "q" #'quit-window))

;; Fallback major modes for extensions without native support.
(dolist (pair '(("\\.in\\'" . text-mode)
                ("\\.out\\'" . text-mode)
                ("\\.args\\'" . text-mode)
                ("\\.bb\\'" . shell-script-mode)
                ("\\.bbclass\\'" . shell-script-mode)
                ("\\.Rmd\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist pair))

;; Editing defaults from `simple' (preloaded).

;; Trailing whitespace is a per-mode decision: code and config buffers only.
(defun ar/show-trailing-whitespace ()
  "Show trailing whitespace in code and config buffers."
  (setq-local show-trailing-whitespace t))

(use-package simple
  :hook (text-mode . visual-line-mode)
  :init
  (column-number-mode 1)
  (setopt kill-whole-line t
          line-move-visual nil
          track-eol t
          set-mark-command-repeat-pop t)
  (add-hook 'prog-mode-hook #'ar/show-trailing-whitespace)
  (add-hook 'conf-mode-hook #'ar/show-trailing-whitespace)
  ;; Single-character y/n instead of yes/no (Emacs 28+).
  (setopt use-short-answers t)
  ;; Default buffer state: text mode, 80-column fill, 4-space indentation.
  (setq-default major-mode 'text-mode
                fill-column 80
                tab-width 4
                indent-tabs-mode nil)
  ;; Silence audible and visual bells.
  (setopt visible-bell nil
          ring-bell-function #'ignore)
  ;; Suppress echo-area chatter for buffer boundaries.
  (setq command-error-function
        (lambda (data context caller)
          (unless (memq (car data) '(beginning-of-buffer end-of-buffer))
            (command-error-default-function data context caller))))
  ;; Duplicate buffer names are disambiguated by directory (uniquify).
  (setopt uniquify-buffer-name-style 'forward)
  :config
  ;; Auto-fill keeps blockquotes, bullets and numbered lists flowing.
  (setopt adaptive-fill-regexp
          "[ \t]*\\(>[ \t]*\\|[-–—]\\|[•‣*]\\|[0-9]+[.)]\\)[ \t]*"
          adaptive-fill-first-line-regexp "^* *$"
          sentence-end
          "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
          sentence-end-double-space nil
          word-wrap-by-category t)
  ;; Drop the continuation fringe arrows; visual wrapping is enough.
  (setq-default fringe-indicator-alist
                (delq (assq 'continuation fringe-indicator-alist)
                      fringe-indicator-alist)))

;; Session persistence: cursor positions, recent files and minibuffer
;; history, routed into var/ and etc/ by early-init.el before any of this
;; runs.

;; recentf helpers, defined at top level so they exist before recentf-mode
;; can ever consult the exclusion list.
(defvar ar/recentf-flush-timer nil
  "Periodic `recentf-save-list' timer (daemon hygiene).")

(use-package saveplace
  :custom
  ;; Forget positions in offline/unmounted volumes so startup never hangs.
  (save-place-forget-unreadable-files t)
  :config
  (save-place-mode 1))

(use-package recentf
  :init
  (setopt recentf-max-saved-items 300
          recentf-exclude
          '("/\\.?cache/" "/\\.cask/" "/url/" "COMMIT_EDITMSG\\'" "/bookmarks\\'"
            "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
            "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
            "^/tmp/" "^/var/folders/.+$" "/persp-confs/"))
  :config
  (recentf-mode 1)
  ;; Never record our own state files or remote paths: flushing TRAMP
  ;; entries risks SSH timeouts, and truename resolution hangs on dead
  ;; symlink targets.
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name recentf-save-file))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name ar/var-dir))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name ar/etc-dir))
  (add-to-list 'recentf-exclude #'file-remote-p)
  ;; Abbreviate instead of resolving truenames when recording entries.
  (setq recentf-filename-handlers '(abbreviate-file-name))
  ;; Daemon-safe periodic flush: state survives SIGKILL'd sessions.
  (unless (or noninteractive (timerp ar/recentf-flush-timer))
    (setq ar/recentf-flush-timer
          (run-at-time 300 300 #'recentf-save-list))))

(use-package savehist
  :init
  (setopt enable-recursive-minibuffers t
          history-length 1000
          savehist-additional-variables '(global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 300)
  :config
  (savehist-mode 1))

;; Keep buffers synchronized with disk changes via native file notifications.
(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  ;; Never fall back to interval polling: native notifications only.
  (auto-revert-avoid-polling t)
  :config
  (global-auto-revert-mode 1))

;; camelCase-aware word boundaries, globally.
(use-package subword
  :config
  (global-subword-mode 1))

;; Viewport stability and native pixel-precision scrolling, tuned for
;; PGTK/Wayland.  `scroll-margin' MUST stay 0: non-zero values snap the
;; viewport and destroy pixel-smooth animation on this compositor.
(setopt hscroll-step 1
        hscroll-margin 2
        scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position t
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil)

(use-package pixel-scroll
  :custom
  ;; No synthetic momentum: prevents Wayland/PGTK compositor feedback loops.
  (pixel-scroll-precision-use-momentum nil)
  ;; Animate PageUp/PageDown and `scroll-up-command'/`scroll-down-command'.
  (pixel-scroll-precision-interpolate-page t)
  :config
  (pixel-scroll-precision-mode 1))

;; Pair machinery: electric insertion and O(1) multi-character emphasis
;; pairs in prose.  (Decision: electric-pair, NOT smartparens.)

;; O(1) multi-character emphasis injection: typing *, =, _ or ~ twice in a
;; row in prose produces the closing pair immediately.  Strictly inhibited
;; inside Org src blocks and Markdown code spans.
(defun ar/elec-pair-multi-char-prose ()
  "Insert a matching emphasis pair after doubled punctuation in prose."
  (when (and (derived-mode-p 'text-mode 'org-mode 'markdown-mode)
             (not (and (derived-mode-p 'org-mode) (org-in-src-block-p)))
             (not (and (derived-mode-p 'markdown-mode)
                       (or (and (fboundp 'markdown-code-block-at-point-p)
                                (markdown-code-block-at-point-p))
                           (and (fboundp 'markdown-inline-code-at-point-p)
                                (markdown-inline-code-at-point-p))))))
    (let* ((c1 (char-before))
           (c2 (char-before (1- (point))))
           (before-pair (char-before (- (point) 2)))
           (after-pair (char-after)))
      (when (and c1 c2 (eq c1 c2)
                 (memq c1 '(?* ?= ?_ ?~))
                 (not (eq before-pair c1))
                 (not (eq after-pair c1))
                 (or (null before-pair)
                     (not (memq (char-syntax before-pair) '(?w ?_)))))
        (insert (string c1 c1))
        (backward-char 2)))))

(use-package elec-pair
  :config
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
        electric-pair-skip-whitespace t
        electric-pair-delete-adjacent-pairs t)
  (electric-pair-mode 1)
  (add-hook 'post-self-insert-hook #'ar/elec-pair-multi-char-prose))

;; Electric indentation plus an "electric words" engine: keywords like
;; `else' re-indent the line as soon as they are completed at end of line.
(defcustom ar/electric-indent-words
  '("else" "elif" "elseif" "catch" "finally" "except")
  "Words that trigger immediate re-indentation when typed at EOL."
  :type '(repeat string)
  :group 'ar)

(defun ar/electric-indent-words-fn (_char)
  "Force re-indentation when an electric word is completed at EOL.
Confined to `prog-mode' derivatives to avoid prose collisions."
  (when (and (derived-mode-p 'prog-mode)
             (eolp)
             ar/electric-indent-words)
    (save-excursion
      (backward-word)
      (looking-at-p (concat "\\<" (regexp-opt ar/electric-indent-words)
                            "\\>")))))

(add-hook 'electric-indent-functions #'ar/electric-indent-words-fn)

(use-package electric
  :init
  ;; Newline and backspace trigger electric indentation; ?\^? gives smart
  ;; backspace-dedent in leading whitespace for Python/C-style modes.
  (setq-default electric-indent-chars (append '(?\n ?\^?)
                                              electric-indent-chars)))

;; Matching-delimiter highlighting and event-driven blink feedback.
(use-package paren
  :custom
  (show-paren-delay 0.05)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-style 'parenthesis)
  ;; Off-screen matches render as a window overlay instead of clobbering the
  ;; echo area (protects ElDoc and which-key).
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode 1)
  ;; Emacs 31: no phantom highlighting inside strings and comments.
  (setopt show-paren-not-in-comments-or-strings 'all))

;; Blink feedback on closing-delimiter insertion.
(setopt blink-matching-paren t
        blink-matching-paren-distance nil
        blink-matching-delay 0.1
        blink-matching-paren-highlight-offscreen t)

;; Whitespace fontification restricted to mixed-indentation detection.
(use-package whitespace
  :defer t
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(space-before-tab space-after-tab indentation)))

(ar/define-large-file-guard whitespace-mode)

;; Native line numbers: absolute numbering, with a pre-sized margin to
;; prevent redisplay jitter.
(use-package display-line-numbers
  :defer t
  :hook ((prog-mode conf-mode yaml-mode LaTeX-mode tex-mode)
         . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'absolute)
  :custom-face
  ;; Recessive margin typography, matching the doom-tokyo-night palette.
  (line-number ((t (:inherit shadow :foreground "#565f89"))))
  (line-number-current-line
   ((t (:inherit default :foreground "#c0caf5" :weight bold)))))

(ar/define-large-file-guard display-line-numbers-mode)

;; Window dividers: right-only placement with a 1px baseline.
(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  :custom-face
  (window-divider ((t (:foreground "#292e42"))))
  :config
  (window-divider-mode 1))

;; Active-line highlight: selected window only, behind syntax highlighting,
;; suppressed in terminal/UI modes and during Evil visual selection.
(use-package hl-line
  :custom
  (hl-line-sticky-flag nil)
  (hl-line-overlay-priority -1)
  :config
  (define-advice global-hl-line-highlight (:around (fn) guard-excluded-modes)
    (if (derived-mode-p 'dired-mode
                        'magit-mode
                        'compilation-mode
                        'special-mode
                        'eshell-mode
                        'shell-mode
                        'term-mode
                        'ghostel-mode
                        'ghostel-compile-view-mode
                        'treemacs-mode)
        (when global-hl-line-overlay
          (delete-overlay global-hl-line-overlay))
      (funcall fn)))
  (add-hook 'evil-visual-state-entry-hook #'global-hl-line-unhighlight)
  (add-hook 'evil-visual-state-exit-hook #'global-hl-line-highlight)
  (global-hl-line-mode 1))

;; Long-daemon hygiene: automatically kill long-inactive, buried buffers.
(use-package buffer-terminator
  :defer 60
  :custom
  (buffer-terminator-verbose nil)
  ;; Terminate buffers inactive for 30 minutes, checked every 10 minutes.
  (buffer-terminator-inactivity-timeout (* 30 60))
  (buffer-terminator-interval (* 10 60))
  :config
  (buffer-terminator-mode 1))

;; Module boundary: core is loaded by init.el before every other module.

(provide 'core-config)
;;; core-config.el ends here
