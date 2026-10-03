;;; core-config.el --- Core behavior, performance tuning and shared helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; The core module: editor defaults, performance and GC tuning, and the
;; shared predicates/macros (`icons-displayable-p', `with-eval-after-load-all',
;; `too-long-file-p', ...) that every later module builds on.  Also owns
;; `reload-init-file', which re-evaluates all module files after edits.

;;; Code:

;; Shared predicates and macros.  Keep this block first: every later module
;; (and every later block here) may call these helpers.

;; Customization group for the `ar/' namespace; later modules add their own
;; defcustoms to it.
(defgroup ar nil
  "Customization group for this configuration's `ar/' options."
  :group 'convenience)

;; Childframes (posframe/corfu popups) need a graphical display or a TTY
;; build with child-frame support, plus a dedicated minibuffer frame.
(defun childframe-workable-p ()
  "Return non-nil if childframes are supported in this session.
Used by UI packages such as `corfu' and `posframe'."
  (and (not noninteractive)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun childframe-completion-workable-p ()
  "Return non-nil if childframe completion UIs may be used."
  (childframe-workable-p))

(defun icons-displayable-p ()
  "Return non-nil if the `nerd-icons' icon font is displayable.
Guards icon rendering in environments without nerd-icon support."
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
still be toggled deliberately.  Every module defines its large-file
guard through this macro so the thresholds stay consistent config-wide."
  `(define-advice ,mode (:before-while (&optional arg) guard-large-files)
     (or (and arg (< (prefix-numeric-value arg) 1))
         (not (too-long-file-p)))))

;; Region-or-symbol fallback shared by search, navigation and transient menus.
(defun selected-region-or-symbol-at-point ()
  "Return the active region, or the symbol at point if no region is active."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defmacro with-eval-after-load-all (features &rest body)
  "Run BODY once every feature in FEATURES has loaded.

FEATURES is a single feature or an unquoted list of features, where
each feature is a symbol or (per `with-eval-after-load') a file-name
string:

  (with-eval-after-load-all eglot ...)                 ; one feature
  (with-eval-after-load-all (eglot flymake) ...)       ; several
  (with-eval-after-load-all \\='(eglot flymake) ...)      ; quoted list

BODY runs exactly once, the moment the LAST of FEATURES to load
actually finishes loading -- whichever one that happens to be.  Any
FEATURES already loaded when this macro is evaluated are satisfied
immediately; only the remaining ones are waited on.  The order in
which FEATURES are listed has no effect on when or whether BODY runs.

BODY is automatically wrapped in `with-no-warnings', since it will
typically call functions or reference variables from packages that
are not yet loaded (and therefore not yet defined) at byte-compile
time, which would otherwise produce spurious compiler warnings.

\(fn FEATURES BODY...)"
  (declare (indent 1) (debug (form body)))
  ;; Normalize FEATURES to a flat, unquoted list, accepting any of:
  ;; SYMBOL, 'SYMBOL, (SYMBOL...), '(SYMBOL...).
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
  ;; Build one `with-eval-after-load' per feature, nested innermost-out,
  ;; with BODY (wrapped in `with-no-warnings') at the core.  Nesting order
  ;; does not matter for *when* BODY runs: `with-eval-after-load' fires
  ;; immediately for a feature that is already loaded, so no matter which
  ;; order we nest in, BODY only actually fires once the last outstanding
  ;; feature loads.
  (let ((form `(with-no-warnings ,(macroexp-progn body))))
    (dolist (f (reverse features))
      (setq form `(with-eval-after-load ,(if (stringp f) f `',f) ,form)))
    form))

;; Unified escape: clear search highlights, then abort the minibuffer or the
;; pending command.  Bound to `<escape>' in normal/visual/motion states and
;; all minibuffer maps by the keybinds module; defined here so those bindings
;; always resolve.
(defun ar/escape-key ()
  "Clear search highlights, abort recursive edits, or quit pending commands."
  (interactive)
  ;; Clear Evil ex-search highlights.
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))
  ;; Abort the minibuffer if active, otherwise cancel macros/popups.
  (if (minibufferp)
      (abort-recursive-edit)
    (keyboard-quit)))

;; Swap a horizontal split to vertical or vice versa.  Bound to `SPC w t' by
;; the keybinds module (the old config bound a symbol that is not an Emacs
;; command; this is the real command).
(defun window-toggle-split-direction ()
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
          (setq done (and (eq neighbor1 neighbor2)
                          (not (minibufferp neighbor2))))
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

(defun reload-init-file ()
  "Re-evaluate every module file in `ar-modules'.
early-init.el and init.el are deliberately excluded: the bootstrap they
own (package bootstrap, module require order) must run exactly
once per session.  Some changes (removed hooks, packages) still require
a full restart."
  (interactive)
  (let ((start-time (current-time)))
    (dolist (mod ar-modules)
      (let ((src (file-name-sans-extension
                  (or (locate-library (symbol-name mod))
                      (user-error "Reload: %s not found on `load-path'" mod)))))
        (message "Reloading %s..." mod)
        ;; Re-evaluate the SOURCE file: byte-compiled artifacts are ignored
        ;; for this one load, so recent edits always take effect.
        (load (concat src ".el") nil 'nomessage)))
    (message "Reload finished in %.2fs -- restart Emacs for package changes."
             (float-time (time-subtract (current-time) start-time)))))

;; Performance and garbage collection.  Startup-time GC deferral is owned by
;; early-init.el (design section 4.1); this block replaces `gcmh' at runtime
;; with the measured steady-state values from the same section.

(defconst ar/gc-steady-cons-threshold 100000000
  "Steady-state `gc-cons-threshold' (100 MB, lsp-mode's documented value).
Lower it if freezing appears under heavy LSP work, raise it for stutter.")

(defun ar/gc-restore-steady-state ()
  "Restore steady-state GC thresholds after a heavy phase."
  (setq gc-cons-threshold ar/gc-steady-cons-threshold
        gc-cons-percentage 0.1))

;; Batch runs keep whatever early-init.el set (the 256 MB batch cap, design
;; section 4.1) -- after-init-time is already non-nil for `-l' loads, so it
;; is not a reliable reload marker there.  Interactive: restore on
;; `emacs-startup-hook' during normal startup; early-init restores the same
;; values on the same hook, so this is belt and braces.  Live reloads
;; (reload-init-file re-evaluates core-config.el) apply the steady state at once.
(unless noninteractive
  (if after-init-time
      (ar/gc-restore-steady-state)
    (add-hook 'emacs-startup-hook #'ar/gc-restore-steady-state 101)))

;; Raise the GC threshold while the minibuffer is active so completion input
;; never triggers a collection mid-keystroke; collect and restore on exit.
(defun ar/gc-minibuffer-setup ()
  "Defer garbage collection while the minibuffer is in use."
  (setq gc-cons-threshold (* 2 ar/gc-steady-cons-threshold)))

(defun ar/gc-minibuffer-exit ()
  "Collect garbage and restore steady-state thresholds after completion."
  (garbage-collect)
  (ar/gc-restore-steady-state))

(add-hook 'minibuffer-setup-hook #'ar/gc-minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'ar/gc-minibuffer-exit)

;; Reclaim memory when the frame loses focus.  Proven habit from the old
;; config's gcmh block; `after-focus-change-function' is the replacement for
;; the obsolete `focus-out-hook'.
(defun ar/gc-when-unfocused ()
  "Collect garbage when no frame has input focus."
  (unless (frame-focus-state)
    (garbage-collect)))
(add-function :after after-focus-change-function #'ar/gc-when-unfocused)

;; Long-line optimizations (Emacs 29+): keep redisplay linear on minified
;; JSON, logs and generated code.
(setq-default long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

;; Predictable LTR redisplay: no per-paragraph bidi search and no Parenthesis
;; Algorithm inhibition overhead on this Latin/CJK workload.
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Expanded undo limits for large refactoring operations.
(setopt undo-limit 800000
        undo-strong-limit 1200000
        undo-outer-limit 120000000)

;; Scrolling and redisplay: viewport stays put while typing, font-lock defers
;; 50 ms so input wins (proven feel under eglot + tree-sitter), and case
;; folding is skipped while picking major modes.
(setopt highlight-nonselected-windows nil
        window-resize-pixelwise nil
        jit-lock-defer-time 0.05
        auto-mode-case-fold nil)
(setq auto-window-vscroll nil)

;; Platform layer: PGTK/Wayland workarounds, daemon plumbing, UTF-8 on every
;; I/O boundary, and identity.  All of it is batch-safe.

;; Disable the GTK input-method context per frame: it adds noticeable input
;; latency under Wayland (fcitx5/ibus), and Emacs' own input handling is what
;; we want.  Frame-local, so it re-applies to every new frame, including
;; daemon client frames.
(defun ar/pgtk-disable-im-context (frame)
  "Turn off the GTK input-method context on FRAME."
  (with-selected-frame frame
    (pgtk-use-im-context nil)))
(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions #'ar/pgtk-disable-im-context))

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
;; GTK pinentry dialog, preserving daemon stability on PGTK/Wayland.
(setopt epg-pinentry-mode 'loopback)

;; Native compilation: defaults stay (design section 4.1 keeps
;; `native-comp-speed' 2 and relies on the 30.1+ important-warnings filter).
;; Only the *display* of async-comp warnings is quieted -- the old config's
;; parity behavior.  early-init owns the same suppression; this idempotent
;; push covers reload paths and is additive, never clobbering other entries.
(unless (member '(comp) warning-suppress-types)
  (push '(comp) warning-suppress-types))

;; Daemon plumbing: load server.el eagerly in daemon sessions so
;; `server-after-make-frame-hook' is a live list from the first client frame;
;; the ui module hangs font setup on it.  Interactive sessions are covered by
;; `after-make-frame-functions' above.  The server itself is started by the
;; systemd user unit / emacsclient, never from the config.
(when (daemonp)
  (require 'server))

;; UTF-8 on every I/O boundary.  The Windows selection guard is kept so the
;; config stays portable: forcing utf-8 there breaks the utf-16-le clipboard.
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

;; Process and compilation safeguards: never prompt about processes or save
;; modified buffers mid-compile, and always scroll the compilation window.
(setopt confirm-kill-processes nil
        compilation-ask-about-save nil)
(setq compilation-always-kill t
      compilation-scroll-output t)

;; Silence advice-redefinition noise; echo keystrokes responsively.
(setopt ad-redefinition-action 'accept
        echo-keystrokes 0.1)

;; Filesystem hygiene: no lockfiles (they fight file watchers), no backup
;; clutter, no auto-save files (super-save in the editor module owns the
;; save strategy), and a final newline always ensured.
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

;; `q' buries read-only list buffers (*Packages*, *Buffer List*, ...) to match
;; Vim/Emacs muscle memory.  Deferred until tabulated-list first loads.
(use-package tabulated-list
  :defer t
  :config
  (keymap-set tabulated-list-mode-map "q" #'quit-window))

;; Fallback major modes for extensions without native support.
;; `shell-script-mode' is the autoload alias of `sh-mode' (verified on 31.1).
(dolist (pair '(("\\.in\\'" . text-mode)
                ("\\.out\\'" . text-mode)
                ("\\.args\\'" . text-mode)
                ("\\.bb\\'" . shell-script-mode)
                ("\\.bbclass\\'" . shell-script-mode)
                ("\\.Rmd\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist pair))

;; Editing defaults from `simple' (preloaded): sensible buffer state, quiet
;; bells, single-character prompts and per-mode trailing-whitespace display.

;; Trailing whitespace is a per-mode decision: code and config buffers only,
;; so minibuffer, echo-area and UI buffers are never affected (Doom-style
;; per-mode hooks instead of a global mode).  Top-level defun so every hook
;; always resolves to a defined command.
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
(defvar ar/recentf-org-agenda-cache nil
  "Cached list of resolved `org-agenda' file paths (recentf exclusion).")
(defvar ar/recentf-flush-timer nil
  "Periodic `recentf-save-list' timer (daemon hygiene).")
(defvar ar/recentf-org-agenda-timer nil
  "Idle timer refreshing the `org-agenda' recentf exclusion cache.")

(defun ar/update-recentf-org-agenda-cache ()
  "Refresh the recentf exclusion cache for `org-agenda' files."
  (when (bound-and-true-p org-agenda-files)
    (setq ar/recentf-org-agenda-cache
          (mapcar #'file-truename
                  (seq-filter #'file-exists-p (org-agenda-files t))))))

(defun ar/exclude-org-agenda-files-p (file)
  "Return non-nil if FILE is an `org-agenda' file, using the cached list."
  (and (not (file-remote-p file))
       (string-suffix-p ".org" file t)
       ar/recentf-org-agenda-cache
       (member (file-truename file) ar/recentf-org-agenda-cache)))

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
          '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
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
  (add-to-list 'recentf-exclude #'ar/exclude-org-agenda-files-p)
  ;; Abbreviate instead of resolving truenames when recording entries.
  (setq recentf-filename-handlers '(abbreviate-file-name))
  ;; Daemon-safe periodic flush: state survives SIGKILL'd sessions.
  (unless (timerp ar/recentf-flush-timer)
    (setq ar/recentf-flush-timer
          (run-at-time 300 300 #'recentf-save-list)))
  ;; Defer org-agenda cache updates until org is actually loaded.
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'ar/update-recentf-org-agenda-cache)
    (unless (timerp ar/recentf-org-agenda-timer)
      (setq ar/recentf-org-agenda-timer
            (run-with-idle-timer 30 t #'ar/update-recentf-org-agenda-cache)))))

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
;; PGTK/Wayland.

;; `scroll-margin' MUST stay 0: non-zero values snap the viewport and destroy
;; pixel-smooth animation on this compositor.
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
;; pairs in prose.  (`delete-pair' tuning and the `ar/delete-pair-dwim' DWIM
;; live in the editor module -- the single copy of each definition.)

;; O(1) multi-character emphasis injection: typing *, =, _ or ~ twice in a
;; row in prose produces the closing pair immediately.  Strictly inhibited
;; inside Org src blocks and Markdown code spans.  Top-level defun so the
;; post-self-insert hook always resolves to a defined command.
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
                 ;; Do not fire when appending to identical punctuation (***).
                 (not (eq before-pair c1))
                 (not (eq after-pair c1))
                 ;; Only in empty space or before words, never after words.
                 (or (null before-pair)
                     (not (memq (char-syntax before-pair) '(?w ?_)))))
        (insert (string c1 c1))
        (backward-char 2)))))

(use-package elec-pair
  :config
  ;; Conservative inhibition avoids pairing in unbalanced buffers and inside
  ;; complex strings.
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
  ;; Emacs 31: no phantom highlighting inside strings and comments.  The
  ;; released 31.1 defcustom takes `all'/`on-mismatch'/nil (the old config's
  ;; boolean t predates the release type); `all' is the strictest guard.
  (setopt show-paren-not-in-comments-or-strings 'all))

;; Blink feedback on closing-delimiter insertion.  The unbounded search
;; distance guarantees accuracy in deeply nested AST blocks.
(setopt blink-matching-paren t
        blink-matching-paren-distance nil
        blink-matching-delay 0.1
        blink-matching-paren-highlight-offscreen t)

;; Whitespace fontification restricted to mixed-indentation detection;
;; trailing spaces are handled by `show-trailing-whitespace' (simple block).

(use-package whitespace
  :defer t
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(space-before-tab space-after-tab indentation))
  :config
  ;; Abort activation in huge buffers (negative/zero prefix bypasses).
  (ar/define-large-file-guard whitespace-mode))

;; Native line numbers: absolute current line with relative surroundings for
;; Vim motions, with a pre-sized margin to prevent redisplay jitter.

(use-package display-line-numbers
  :defer t
  :hook ((prog-mode conf-mode yaml-mode LaTeX-mode tex-mode)
         . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'absolute)
  :custom-face
  ;; Recessive margin typography, matching the doom-tokyo-night palette the
  ;; ui module loads.  Note: a loaded theme re-asserts its own faces; these
  ;; values exist to match it byte-for-byte if it does not.
  (line-number ((t (:inherit shadow :foreground "#565f89"))))
  (line-number-current-line
   ((t (:inherit default :foreground "#c0caf5" :weight bold))))
  :config
  ;; Abort activation in huge buffers (negative/zero prefix bypasses).
  (ar/define-large-file-guard display-line-numbers-mode))

;; Window dividers: right-only placement with a 1px baseline, so horizontal
;; window boundaries carry no redundant chrome.

(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  :custom-face
  ;; Harmonizes with the doom-tokyo-night border color.
  (window-divider ((t (:foreground "#292e42"))))
  :config
  ;; Synchronous activation guarantees registration before the first paint.
  (window-divider-mode 1))

;; Active-line highlight: selected window only, behind syntax highlighting,
;; suppressed in terminal/UI modes and during Evil visual selection.

(use-package hl-line
  :custom
  ;; Confine the highlight strictly to the active window.
  (hl-line-sticky-flag nil)
  ;; Render the overlay behind syntax highlighting and diagnostics.
  (hl-line-overlay-priority -1)
  :config
  ;; Delete the overlay instead of highlighting in excluded modes.  Pruned to
  ;; modes that exist in this config (ghostel lives in the vc-projects
  ;; module; treemacs likewise).
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
  ;; Drop the highlight while an Evil visual selection is active, redraw it
  ;; on exit.  The hooks are inert until the editor module loads evil.
  (add-hook 'evil-visual-state-entry-hook #'global-hl-line-unhighlight)
  (add-hook 'evil-visual-state-exit-hook #'global-hl-line-highlight)
  ;; Synchronous activation guarantees overlay registration.
  (global-hl-line-mode 1))

;; Long-daemon hygiene: automatically kill long-inactive, buried buffers.

(use-package buffer-terminator
  :defer 60
  :custom
  ;; Keep the minibuffer free of echo-area logging.
  (buffer-terminator-verbose nil)
  ;; Terminate buffers inactive for 30 minutes, checked every 10 minutes.
  (buffer-terminator-inactivity-timeout (* 30 60))
  (buffer-terminator-interval (* 10 60))
  :config
  ;; Janitor semantics: the package only needs its background timer running,
  ;; so 60 idle seconds after startup is exactly as good as module load time
  ;; (use-package `:defer N' arms an idle timer; batch runs never fire idle
  ;; timers, so the gates see the package unloaded).  The eager load bought
  ;; nothing but startup cost, and the package is outside the design's
  ;; eager-load budget.
  (buffer-terminator-mode 1))

;; Module boundary: core is loaded by init.el before every other module.

(provide 'core-config)
;;; core-config.el ends here
