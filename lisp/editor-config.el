;;; editor-config.el --- Evil stack, general.el leader definers, core editing aids -*- lexical-binding: t; -*-

;;; Commentary:

;; The editing module.  Defines `ar/global-leader' and
;; `ar/local-leader' (used by every module loaded after this one) and
;; configures the evil stack and the core editing aids.  Package
;; installation is handled exclusively by init.el's `ar/packages'
;; manifest, so these declarations install nothing.  evil-ts-obj is a
;; git recipe in that manifest; everything else resolves through
;; straight's default recipe repositories.

;;; Code:

;; Evil core engine.  Loaded eagerly: the evil stack is part of the
;; startup eager-load budget (design §4.2).
(use-package evil
  :demand t
  :init
  ;; Core settings required before package load.
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-mode-line-format nil
        evil-kbd-macro-suppress-motion-error t
        evil-symbol-word-search t
        evil-undo-system 'undo-fu
        evil-kill-on-visual-paste nil)
  ;; Extended keybindings and behavioral toggles.
  ;; (The old config also set `evil-want-C-h-delete', which exists in evil
  ;; 1.15.0 (evil-vars.el:580, default nil); it is deliberately dropped so
  ;; C-h keeps its default insert-state behavior.)
  (setq evil-want-C-g-bindings t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-respect-visual-line-mode nil)
  ;; State-dependent cursor shapes (variable state only; batch-safe).
  (setq evil-normal-state-cursor '(box)
        evil-insert-state-cursor '(bar)
        evil-visual-state-cursor '(hollow)
        evil-replace-state-cursor '(underline))
  :custom
  (evil-ex-visual-char-range t)
  (evil-ex-search-vim-style-regexp t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-echo-state nil)
  (evil-move-cursor-back nil)
  ;; (The old `evil-v$-excludes-newline t' entry is dropped: evil 1.15.0
  ;; defines it (evil-vars.el:1138, default nil) but we keep the default
  ;; `$' behavior.)
  (evil-want-fine-undo t)
  ;; Prevents cursor from moving past EOL.
  (evil-move-beyond-eol nil)
  (evil-search-wrap t)
  (select-enable-clipboard t)
  (evil-visual-update-x-selection-p nil)
  ;; Search behavior and highlighting.
  (evil-ex-search-case 'smart)
  (evil-ex-search-incremental t)
  (evil-ex-search-highlight-all t)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq-default evil-shift-width tab-width)
  ;; Ex and search history cycling via arrow keys and Emacs bindings.
  (dolist (keymap (list evil-ex-completion-map
                        evil-ex-search-keymap))
    (define-key keymap (kbd "<up>") #'previous-history-element)
    (define-key keymap (kbd "<down>") #'next-history-element)
    (define-key keymap (kbd "C-p") #'previous-history-element)
    (define-key keymap (kbd "C-n") #'next-history-element))
  ;; Forces normal state in read-only UI buffers.  (dashboard-mode is not
  ;; installed in the new stack; dropped.)
  (evil-set-initial-state 'messages-buffer-mode 'normal))

;; Evil Collection: standardized Vim bindings for non-text buffers, with
;; Unimpaired bracket navigation.  Minibuffer integration stays off to
;; protect Vertico.
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-want-unimpaired-p t)
  ;; Find-usages bindings are enabled by default in evil-collection 3.x.
  ;; (The old config set `evil-collection-want-find-usages-bindings-p', a
  ;; variable that never existed; the obsolete knob is
  ;; `evil-collection-want-find-usages-bindings' and already defaults to t.)
  :config
  (evil-collection-init))

;; Linear undo/redo backend consumed by `evil-undo-system'.
(use-package undo-fu
  :defer t)

;; Persistent undo across sessions.  Excluded modes are pruned to those that
;; actually exist in the new package set (dashboard/pdf/vterm/eat dropped).
;; Eagerly loaded: with ":defer t" (the old shape) the :config body never
;; runs, so the compression choice, custom hasher, and
;; `undo-fu-session-global-mode' never activate.
(use-package undo-fu-session
  :demand t
  :custom
  ;; Route persistent undo history to the var directory.
  (undo-fu-session-directory (ar/var-file "undo-fu-session/"))
  ;; Align disk footprint with undo-fu's linear paradigm; ignore branching history.
  (undo-fu-session-linear t)
  ;; Cull oldest session files to prevent infinite filesystem bloat and I/O stutter.
  (undo-fu-session-file-limit 500)
  ;; Exclude transient Git files to prevent metadata leakage and I/O errors.
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'"
     "/git-rebase-todo\\'"))
  (undo-fu-session-incompatible-major-modes
   '(magit-status-mode
     magit-log-mode
     magit-diff-mode
     magit-revision-mode
     git-commit-mode
     dired-mode
     dirvish-mode
     treemacs-mode
     eshell-mode
     term-mode
     ghostel-mode
     compilation-mode
     grep-mode
     occur-mode
     org-agenda-mode
     help-mode
     helpful-mode
     Info-mode
     special-mode))
  :config
  ;; Conditional compression: prefer zstd for near-instantaneous I/O, fallback to gzip.
  (setq undo-fu-session-compression
        (cond ((executable-find "zstd") 'zst)
              ((executable-find "gzip") 'gz)
              (t nil)))
  ;; Bulletproof path hashing: stable `secure-hash' primitive bypasses OS
  ;; 255-char limits while appending the truncated base filename for readability.
  (defun ar/undo-fu-session-hash-path (file ext)
    "Safely hash FILE to bypass OS 255-char limits, appending EXT."
    (let* ((base (file-name-nondirectory file))
           (hash (substring (secure-hash 'sha1 file) 0 16)))
      (expand-file-name (concat hash "-" base ext) undo-fu-session-directory)))
  (setq undo-fu-session-make-file-name-function #'ar/undo-fu-session-hash-path)
  (undo-fu-session-global-mode 1))

;; Changelist navigation orthogonal to the jump list; powers evil's
;; COUNT-aware g; and g, temporal motions.
(use-package goto-chg
  :defer t
  :commands (goto-last-change goto-last-change-reverse)
  :custom
  ;; Spatial threshold to group micro-edits into logical navigation blocks.
  (glc-default-span 16))

;; gc / gcc comment operators.
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

;; Argument text objects bound to `a'.
(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; Numeric increment/decrement, autoloaded on first use.  No key bindings
;; here on purpose: the old config never bound it either, and the natural
;; C-a/C-x bindings would shadow the global C-x prefix in normal state.
(use-package evil-numbers
  :defer t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

;; gx motion to swap two text regions (gX cancels).
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;; Visual pulse feedback on evil edit operations; inherits the theme's
;; highlight face.
(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.1)
  :custom-face
  (evil-goggles-default-face ((t (:inherit highlight))))
  :config
  (evil-goggles-mode 1))

;; gl / gL alignment operators (global minor mode).
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode 1))

;; general.el defines the leader machinery shared by every later module.
;; Loaded eagerly right after evil so that `ar/global-leader' and
;; `ar/local-leader' are available below and in modules 5-10.
(use-package general
  :demand t
  :after evil
  :config
  ;; Global leader prefix: SPC (also C-SPC), across evil states.
  (general-create-definer ar/global-leader
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Local leader prefix: SPC m (also C-SPC m).
  (general-create-definer ar/local-leader
    :states '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m"))

;; Top-level leader entries (ported from the old general block).
(ar/global-leader
  "SPC" '(execute-extended-command :wk "M-x")
  ":"   '(eval-expression :wk "Eval")
  "u"   '(universal-argument :wk "Universal arg")
  "!"   '(shell-command :wk "Shell cmd")
  "X"   '(org-capture :wk "Org capture"))

;; The unified <escape> wrapper binding (`ar/escape-key' is defined in core)
;; moved to the keybinds module, which owns all escape/pop routing.

;; Intercepts evil's o/O operators to continue comment lines natively, using
;; bounded O(1) line searches for comment presence and precise cursor
;; placement.  Carried from the old config and now actually bound (design
;; §6.3: the old config defined these commands but never bound o/O).
(defcustom ar/evil-want-o/O-to-continue-comments t
  "If non-nil, o/O keys will continue comment lines."
  :type 'boolean
  :group 'evil)

(defun ar/evil-open-below-smart ()
  "Open line below, continuing comments if applicable."
  (interactive)
  (if (and ar/evil-want-o/O-to-continue-comments
           comment-start-skip
           (save-excursion
             (beginning-of-line)
             (re-search-forward comment-start-skip (line-end-position) t)))
      (progn
        (end-of-line)
        (comment-indent-new-line)
        (evil-insert-state))
    (evil-open-below (prefix-numeric-value current-prefix-arg))))

(defun ar/evil-open-above-smart ()
  "Open line above, continuing comments if applicable."
  (interactive)
  (if (and ar/evil-want-o/O-to-continue-comments
           comment-start-skip
           (save-excursion
             (beginning-of-line)
             (re-search-forward comment-start-skip (line-end-position) t)))
      (progn
        (beginning-of-line)
        (re-search-forward comment-start-skip (line-end-position) t)
        (comment-indent-new-line)
        (forward-line -1)
        (indent-according-to-mode)
        ;; Push cursor past the delimiter on the newly created line.
        (re-search-forward comment-start-skip (line-end-position) t)
        (evil-insert-state))
    (evil-open-above (prefix-numeric-value current-prefix-arg))))

(general-define-key
 :states 'normal
 "o" #'ar/evil-open-below-smart
 "O" #'ar/evil-open-above-smart)

;; Auto-save on idle and at natural navigation boundaries.  Activated from
;; `after-init-hook': interactively that runs before the first user input,
;; so the auto-save safety net is live from the first keystroke, while
;; batch -l runs (which never fire the hook) stay free of the package.
;; core-config.el nils `auto-save-default', so nothing else owns saving before
;; this fires -- an idle-timer start would leave a window with no net.
(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 10)
  (super-save-remote-files nil)
  ;; Event-driven triggers at window/buffer switches.
  (super-save-triggers
   '(other-window
     windmove-up
     windmove-down
     windmove-left
     windmove-right
     ace-window)))

;; Retro-active Vim-style wrapping (ys/cs/ds) plus evil-embrace's regex-aware
;; structural pairs for prose and LaTeX environments.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :config
  ;; Merge the regex-aware structural engine into evil-surround operators.
  (evil-embrace-enable-evil-surround-integration)
  ;; Native Org emphasis pairs (*, /, _, =, ~) with regex boundaries.
  (add-hook 'org-mode-hook #'embrace-org-mode-hook)
  ;; Native LaTeX pairs ($, \verb).
  (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
  ;; Markdown/GFM pair injection (embrace.el lacks a native markdown hook).
  (defun ar/embrace-markdown-setup ()
    "Inject markdown emphasis and code pairs into embrace."
    (embrace-add-pair ?* "*" "*")
    (embrace-add-pair ?_ "_" "_")
    (embrace-add-pair ?` "`" "`"))
  (add-hook 'markdown-mode-hook #'ar/embrace-markdown-setup)
  (add-hook 'gfm-mode-hook #'ar/embrace-markdown-setup))

;; Native delete-pair tuning plus an O(1) DWIM wrapper for adjacent, string,
;; and nested pair deletion, strictly confined to programming modes.
;; `delete-pair-push-mark' is a verified Emacs 31.1 variable (research-emacs31 §3).
;; This is the single definition of each: a byte-identical earlier copy of
;; the DWIM in core-config.el (and core's setopt of the two variables) was removed
;; so edits here can never be silently shadowed by the later-loaded module.
(setq delete-pair-blink-delay 0.1)
(setq delete-pair-push-mark t)

(defun ar/delete-pair-dwim ()
  "Intelligently delete the pair surrounding or preceding the cursor.
Strictly confined to `prog-mode' to prevent structural destruction in prose."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (user-error "Delete Pair DWIM restricted to prog-mode; use `ds' in prose"))
  (let ((prev (char-before))
        (next (char-after))
        (ppss (syntax-ppss)))
    (cond
     ;; Delete preceding closing delimiter or quote.
     ((and prev (memq (char-syntax prev) '(?\) ?\" ?\|)))
      (delete-pair -1))
     ;; Delete following opening delimiter or quote.
     ((and next (memq (char-syntax next) '(?\( ?\" ?\|)))
      (delete-pair 1))
     ;; Inside a string: jump to start, handle prefixes, and delete.
     ((nth 3 ppss)
      (let ((start (nth 8 ppss)))
        (goto-char start)
        ;; Handle string prefixes (e.g., f"..." in Python).
        (unless (memq (char-after) '(?\" ?\|))
          (forward-char 1))
        (delete-pair 1)))
     ;; Fallback: jump out of the current list and delete the enclosing pair.
     (t
      (condition-case nil
          (progn
            (backward-up-list)
            (delete-pair 1))
        (scan-error (user-error "Not inside a deletable pair")))))))

;; Extends the % motion across semantic blocks (LaTeX, HTML, code).
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; AST text objects for evil's operator-pending maps plus Unimpaired-style
;; goto motions.  The old config used ":defer t :after evil", which never
;; loads the package (use-package 2.4.5 expansion verified: nothing triggers
;; the require), so the text objects were silently dead; :demand t :after
;; evil registers them right after evil loads, as the old config intended.
(use-package evil-textobj-tree-sitter
  :demand t
  :after evil
  :config
  ;; Map AST nodes directly to evil's operator-pending text-object keymaps.
  ;; `evil-textobj-tree-sitter-get-textobj' is a macro: arguments are passed
  ;; literally (never quote the group lists).
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("parameter.inner" "call.inner")))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer")))

  ;; Dynamically registers named interactive commands for Unimpaired-style
  ;; bracket navigation.
  (defun ar/ts-register-goto (key node &optional previous)
    "Bind KEY to a goto command for tree-sitter NODE; PREVIOUS inverts it."
    (let* ((suffix (if previous "prev" "next"))
           (cmd-name (intern (format "ar/ts-goto-%s-%s"
                                     (string-replace "." "-" node)
                                     suffix)))
           (doc (format "Jump to %s %s node." (if previous "previous" "next") node)))
      (defalias cmd-name
        `(lambda ()
           (interactive)
           (evil-textobj-tree-sitter-goto-textobj ,node ,previous))
        doc)
      (general-define-key
       :states '(normal visual motion)
       key cmd-name)))

  (ar/ts-register-goto "]f" "function.outer")
  (ar/ts-register-goto "[f" "function.outer" t)
  (ar/ts-register-goto "]F" "call.outer")
  (ar/ts-register-goto "[F" "call.outer" t)
  (ar/ts-register-goto "]C" "class.outer")
  (ar/ts-register-goto "[C" "class.outer" t)
  (ar/ts-register-goto "]c" "comment.outer")
  (ar/ts-register-goto "[c" "comment.outer" t)
  (ar/ts-register-goto "]v" "conditional.outer")
  (ar/ts-register-goto "[v" "conditional.outer" t)
  (ar/ts-register-goto "]l" "loop.outer")
  (ar/ts-register-goto "[l" "loop.outer" t)
  (ar/ts-register-goto "]a" "parameter.outer")
  (ar/ts-register-goto "[a" "parameter.outer" t)

  ;; Strip verbose package prefixes and .inner/.outer suffixes from which-key.
  (with-eval-after-load 'which-key
    (add-to-list 'which-key-replacement-alist
                 '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:\\.inner\\|\\.outer\\)") . (nil . "\\1")))))

;; AST-aware structural manipulation (drag/swap/extract/raise) built on the
;; Emacs 30 `treesit-thing' API.  Installed by init.el's manifest as a git
;; recipe (git-only: dvzubarev/evil-ts-obj), hence a plain declarative form
;; here.
(use-package evil-ts-obj
  :defer t
  :after evil
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (evil-ts-obj-mode))))
         ((LaTeX-mode markdown-mode markdown-ts-mode yaml-mode yaml-ts-mode) . evil-ts-obj-mode))
  :config
  ;; Aborts activation in massive buffers; bypasses guard to allow explicit
  ;; deactivation.  `too-long-file-p' is defined in core.
  (define-advice evil-ts-obj-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; The AST refactor transient is defined as soon as transient loads, not
;; inside evil-ts-obj's :config, so that the "c s" leader entry resolves
;; before the first tree-sitter buffer is opened.  The evil-ts-obj-*
;; operators it lists resolve when evil-ts-obj-mode activates.
(with-eval-after-load 'transient
  (defun ar/ast-refactor-title ()
    "Return the titled heading for the AST refactor transient."
    (concat
     (if (fboundp 'nerd-icons-codicon)
         (nerd-icons-codicon "nf-cod-symbol_structure" :face 'transient-heading :v-adjust 0.02)
       "")
     (propertize " AST Refactor" 'face 'transient-heading)))

  (transient-define-prefix ar/ast-refactor-transient ()
    "AST Structural Refactoring"
    [:description ar/ast-refactor-title
     ["Drag & Swap"
      ("j" "Drag Down" evil-ts-obj-drag-down :transient t)
      ("k" "Drag Up" evil-ts-obj-drag-up :transient t)
      ("J" "Swap Down" evil-ts-obj-swap-dwim-down :transient t)
      ("K" "Swap Up" evil-ts-obj-swap-dwim-up :transient t)]
     ["Extract & Inject"
      ("l" "Extract Out" evil-ts-obj-extract-down-dwim :transient t)
      ("h" "Extract In" evil-ts-obj-extract-up-dwim :transient t)
      ("s" "Inject Down" evil-ts-obj-inject-down-dwim :transient t)
      ("S" "Inject Up" evil-ts-obj-inject-up-dwim :transient t)]
     ["Mutate"
      ("r" "Raise" evil-ts-obj-raise-dwim :transient t)
      ("c" "Clone After" evil-ts-obj-clone-after-dwim :transient t)
      ("C" "Clone Before" evil-ts-obj-clone-before-dwim :transient t)
      (">" "Slurp" evil-ts-obj-slurp :transient t)
      ("<" "Barf" evil-ts-obj-barf :transient t)
      ("<escape>" "" transient-quit-one :format " ")]]))

;; Bound here because the command lives in this module; the remaining SPC c
;; entries belong to the dev module.
(ar/global-leader
  "c s" '(ar/ast-refactor-transient :wk "Structure refactor"))

;; Exposes the Emacs 30 `treesit-thing' API as Unimpaired-style bracket
;; motions with C-level sibling traversal (no-op outside ts modes).
(general-define-key
 :states '(normal visual motion)
 "]s" (lambda () (interactive) (ignore-errors (treesit-thing-next (point) 'sentence)))
 "[s" (lambda () (interactive) (ignore-errors (treesit-thing-prev (point) 'sentence)))
 "]x" (lambda () (interactive) (ignore-errors (treesit-thing-next (point) 'sexp)))
 "[x" (lambda () (interactive) (ignore-errors (treesit-thing-prev (point) 'sexp))))

;; O(1) AST-aware visual selection expansion via the native treesit engine,
;; integrated with repeat-mode for modifier-less tapping.
(use-package expreg
  :if (treesit-available-p)
  :defer t
  :commands (expreg-expand expreg-contract)
  :config
  ;; Restores the exact cursor origin on C-g to prevent spatial drift.
  (setq expreg-restore-point-on-quit t)
  (defvar-keymap expreg-repeat-map
    :doc "Keymap for repeating expreg commands."
    "+" #'expreg-expand
    "=" #'expreg-expand
    "-" #'expreg-contract
    "_" #'expreg-contract)
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map))

(general-define-key
 :states 'visual
 "v" #'expreg-expand)

(general-define-key
 :states '(normal visual motion)
 "M-=" #'expreg-expand
 "C-M-+" #'expreg-expand
 "C-M--" #'expreg-contract)

;; Trims trailing whitespace only on edited lines, keeping VCS diffs clean.
(use-package ws-butler
  :defer t
  :hook (prog-mode . ws-butler-mode))

;; O(1) line/region dragging with automatic re-indentation of displaced text.
(use-package move-text
  :defer t
  :commands (move-text-up move-text-down)
  :config
  (defun ar/move-text-indent-region-advice (&rest _ignored)
    "Re-indent the current line or active region after moving text."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add #'move-text-up :after #'ar/move-text-indent-region-advice)
  (advice-add #'move-text-down :after #'ar/move-text-indent-region-advice))

;; O(1) radix-tree jumping engine: at-full overlay style, home-row keys,
;; proximity sorting, and the isearch M-j touch (the editor module's isearch
;; configuration, design §2).
(use-package avy
  :defer t
  :commands (avy-goto-char-2
             avy-goto-line
             avy-goto-word-1
             avy-goto-word-0
             avy-isearch
             avy-move-line
             avy-copy-line
             avy-kill-region
             avy-transpose-lines-in-region
             avy-move-region
             avy-copy-region
             avy-kill-ring-save-region)
  :bind (:map isearch-mode-map
         ("M-j" . avy-isearch))
  :custom
  ;; Path generation: standard shortest-path algorithm restricted to home-row.
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Visual rendering: `at-full' preserves peripheral word context.
  (avy-style 'at-full)
  ;; Buffer dimming: focus strictly on the opaque spotlights.
  (avy-background t)
  ;; Scope control: default to the active window; C-u expands to all windows.
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  ;; Render an overlay even for a single target, preventing accidental jumps.
  (avy-single-candidate-jump nil)
  ;; Proximity engine: sort candidates by physical distance to the cursor.
  (avy-orders-alist '((avy-goto-char . avy-order-closest)
                      (avy-goto-word-0 . avy-order-closest)
                      (avy-goto-word-1 . avy-order-closest)))
  (avy-timeout-seconds 0.3)
  :custom-face
  ;; Opaque spotlight faces: 100% opaque backgrounds prevent ghosting.
  (avy-lead-face-0 ((t (:background "#f7768e" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face-1 ((t (:background "#e0af68" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face-2 ((t (:background "#9ece6a" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face ((t (:background "#7dcfff" :foreground "#1a1b26" :weight bold)))))

;; Wraps avy into evil's operator-pending grammar under a unified gs prefix,
;; registering native avy commands as evil motions.  Loads after avy (which
;; itself loads on first avy command via its autoloads).
(use-package evil-easymotion
  :after (evil avy)
  :config
  (evil-define-motion ar/evil-avy-goto-char-2 ()
    "Jump to 2-char target."
    :type exclusive :jump t
    (avy-goto-char-2))
  (evil-define-motion ar/evil-avy-goto-line ()
    "Jump to visible line."
    :type line :jump t
    (avy-goto-line))
  (evil-define-motion ar/evil-avy-goto-char-timer ()
    "Jump to string target via timer."
    :type exclusive :jump t
    (avy-goto-char-timer))
  ;; Populate `evilem-map' under the "gs" prefix.
  (evilem-default-keybindings "gs")
  (define-key evilem-map "s" #'ar/evil-avy-goto-char-2)
  (define-key evilem-map "l" #'ar/evil-avy-goto-line)
  (define-key evilem-map "SPC" #'ar/evil-avy-goto-char-timer))

;; Incremental, regex-aware s/S jumps.  Disabled-mode list is pruned to modes
;; that exist in the new package set (pdf/vterm/eat/deadgrep/rg/ag/flycheck/
;; lsp-ui/docker/dashboard dropped).
(use-package evil-snipe
  :after evil
  :custom
  (evil-snipe-enable-highlight t)
  (evil-snipe-enable-incremental-highlight t)
  (evil-snipe-auto-disable-substitute t)
  ;; Full-screen spatial awareness for incremental highlighting.
  (evil-snipe-scope 'whole-visible)
  ;; Vim parity: lowercase is case-insensitive, uppercase is strict.
  (evil-snipe-smart-case t)
  ;; Keeps the echo area reserved for ElDoc and which-key.
  (evil-snipe-show-prompt nil)
  (evil-snipe-auto-scroll t)
  ;; Expands search scope to the whole buffer if a localized snipe fails.
  (evil-snipe-spillover-scope 'whole-buffer)
  ;; TAB dynamically increases the snipe buffer size.
  (evil-snipe-tab-increment t)
  :custom-face
  (evil-snipe-first-match-face ((t (:foreground "#7dcfff" :underline t))))
  (evil-snipe-matches-face ((t (:foreground "#7dcfff" :underline t))))
  :config
  ;; Populate exclusions and aliases BEFORE activating the global modes;
  ;; modifying defcustom lists here prevents upstream overwrites.
  (dolist (mode '(dirvish-directory-view-mode
                  dirvish-mode
                  git-timemachine-mode
                  eshell-mode
                  ghostel-mode
                  term-mode
                  compilation-mode
                  grep-mode
                  occur-mode
                  flymake-diagnostics-buffer-mode
                  xref--xref-buffer-mode
                  embark-collect-mode
                  help-mode
                  helpful-mode
                  Info-mode
                  woman-mode
                  eww-mode
                  Custom-mode
                  ediff-mode
                  profiler-report-mode
                  messages-buffer-mode
                  backtrace-mode))
    (add-to-list 'evil-snipe-disabled-modes mode))
  ;; Map brackets and quotes to regex patterns for boundary jumping.
  (push '(?\[ "[[{(<]") evil-snipe-aliases)
  (push '(?\] "[]})>]") evil-snipe-aliases)
  (push '(?\" "[\"'`´]") evil-snipe-aliases)
  ;; Activate global modes AFTER the exclusion list is fully populated.
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  ;; Visual-state z/Z: start a snipe and capture the traversed region.
  (evil-define-key 'visual evil-snipe-local-mode-map "z" #'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" #'evil-snipe-S)
  ;; Inject `evilem-map' into the snipe parent transient map.
  (with-eval-after-load 'evil-easymotion
    (define-key evil-snipe-parent-transient-map "g" evilem-map)))

;; Avy-backed navigation to and mutation of visible URLs, file paths, and
;; buttons, with an embark fallback when no recognized link is at point.
(use-package link-hint
  :commands (link-hint-open-link
             link-hint-copy-link
             link-hint-open-multiple-links
             link-hint-copy-all-links
             link-hint-open-link-at-point
             link-hint-copy-link-at-point)
  :custom
  (link-hint-action-fallback-commands
   (list :open (lambda ()
                 (condition-case _
                     (progn (embark-dwim) t)
                   (error nil))))))

;; Match index/total in the modeline during searches, remapped over the
;; query-replace family, with evil integration via evil-anzu.
(use-package anzu
  :demand t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " → ")
  ;; Disable native mode-line injection (doom-modeline renders it).
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode 1))

(use-package evil-anzu
  :after (evil anzu))

;; Occurrence editing engine backing evil-multiedit.  Deferred; evil-multiedit
;; itself requires iedit when it loads.
(use-package iedit
  :commands (iedit-mode
             iedit-rectangle-mode)
  :custom
  ;; Prevents the default C-; global keymap injection.
  (iedit-toggle-key-default nil)
  (iedit-auto-narrow nil)
  (iedit-occurrence-context-lines 1))

;; Multi-cursor editing over iedit.  The old config used ":after (evil iedit)"
;; with :commands, which defers the autoload registration until iedit loads
;; (never on its own), leaving M-d bound to an unresolvable symbol; the
;; verified use-package 2.4.5 expansion for this shape registers autoloads at
;; evil load instead and loads the package on first invocation.
(use-package evil-multiedit
  :after evil
  :defer t
  :commands (evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-match-all
             evil-multiedit-abort
             evil-multiedit-toggle-or-restrict-region)
  :custom-face
  (evil-multiedit-match-face ((t (:background "#bb9af7" :foreground "#1a1b26"))))
  :config
  (evil-multiedit-default-keybinds))

(general-define-key
 :states '(normal visual)
 "M-d" #'evil-multiedit-match-symbol-and-next
 "M-D" #'evil-multiedit-match-all)

(general-define-key
 :states 'visual
 "C-S-<down>" #'evil-multiedit-match-symbol-and-next
 "C-S-<up>"   #'evil-multiedit-match-symbol-and-prev)

(ar/global-leader
  "c v"   '(:ignore t :wk "multi-cursor")
  "c v v" '(evil-multiedit-match-symbol-and-next :wk "Make cursor here")
  "c v a" '(evil-multiedit-match-all :wk "Make all cursors")
  "c v q" '(evil-multiedit-abort :wk "Undo all cursors")
  "c v r" '(evil-multiedit-toggle-or-restrict-region :wk "Restrict region")
  "c v I" '(evil-multiedit-match-symbol-and-prev :wk "Cursors in selection (beg)")
  "c v A" '(evil-multiedit-match-symbol-and-next :wk "Cursors in selection (end)"))

;; Centralized dashboard for avy's spatial jumping and remote text mutation,
;; with a DRY dispatcher honoring --above/--below transient scope args.
(defun ar/avy-dispatch (cmd)
  "Execute avy CMD with an `-above'/`-below' suffix based on transient args."
  (let* ((t-args (transient-args transient-current-command))
         (above (member "--above" t-args))
         (below (member "--below" t-args))
         (suffix (cond ((and above below) "")
                       (above "-above")
                       (below "-below")
                       (t "")))
         (target (intern (concat (symbol-name cmd) suffix))))
    (call-interactively (if (fboundp target) target cmd))))

;; Explicit wrappers to satisfy transient's command resolution.
(defun ar/avy-goto-char-2 () "Goto char via `ar/avy-dispatch'." (interactive) (ar/avy-dispatch 'avy-goto-char-2))
(defun ar/avy-goto-word-1 () "Goto word via `ar/avy-dispatch'." (interactive) (ar/avy-dispatch 'avy-goto-word-1))
(defun ar/avy-goto-symbol-1 () "Goto symbol via `ar/avy-dispatch'." (interactive) (ar/avy-dispatch 'avy-goto-symbol-1))
(defun ar/avy-goto-line () "Goto line via `ar/avy-dispatch'." (interactive) (ar/avy-dispatch 'avy-goto-line))

;; avy 0.5.0 defines these two commands without autoload cookies (verified
;; against avy.el:2170/2233 and its generated avy-autoloads.el, which lists
;; neither); register them here so the first transient press loads the
;; library instead of signaling void-function.
(autoload #'avy-pop-mark "avy" nil t)
(autoload #'avy-org-goto-heading-timer "avy" nil t)

(with-eval-after-load 'transient
  (defun ar/avy-title ()
    "Return the titled heading for the avy transient."
    (concat
     (if (fboundp 'nerd-icons-faicon)
         (nerd-icons-faicon "nf-fa-crosshairs" :face 'transient-heading :v-adjust 0.02)
       "")
     (propertize " Avy Spatial Jumps & Mutations" 'face 'transient-heading)))

  (transient-define-prefix ar/avy-transient ()
    "Avy Spatial Jumps and Mutations Dashboard"
    [:description ar/avy-title
     ["Scope (applies to ⬍)"
      :description (lambda ()
                     (let ((args (transient-args transient-current-command)))
                       (format "Scope (applies to (%s))"
                               (cond ((and (member "--above" args) (member "--below" args)) "#")
                                     ((member "--above" args) "↑")
                                     ((member "--below" args) "↓")
                                     (t "#")))))
      :class transient-row
      ("a" "Above" "--above")
      ("b" "Below" "--below")]

     ["Goto Thing"
      ("c" "Character" avy-goto-char-timer)
      ("2" "2 Characters ⬍" ar/avy-goto-char-2)
      ("w" "Word ⬍" ar/avy-goto-word-1)
      ("s" "Symbol ⬍" ar/avy-goto-symbol-1)
      ;; (The old "W" Whitespace-end entry is dropped as a preference: avy
      ;; 0.5.0 defines `avy-goto-whitespace-end' (avy.el:1409, autoloaded).)
      ("p" "Pop mark" avy-pop-mark)]

     ["Goto Line"
      :pad-keys t
      ("l" "Line ⬍" ar/avy-goto-line)
      ("e" "End of line" avy-goto-end-of-line)
      ("o" "Org heading" avy-org-goto-heading-timer :if (lambda () (derived-mode-p 'org-mode)))
      ("n" "Line number" goto-line :if (lambda () (bound-and-true-p display-line-numbers-mode)))]

     ["Edit Other Line"
      ("C" "Copy" avy-kill-ring-save-whole-line)
      ("k" "Kill" avy-kill-whole-line :if (lambda () (not buffer-read-only)))
      ("m" "Move" avy-move-line :if (lambda () (not buffer-read-only)))
      ("d" "Duplicate" avy-copy-line :if (lambda () (not buffer-read-only)))]

     ["Edit Other Region"
      ("r" "Copy" avy-kill-ring-save-region)
      ("K" "Kill" avy-kill-region :if (lambda () (not buffer-read-only)))
      ("M" "Move" avy-move-region :if (lambda () (not buffer-read-only)))
      ("D" "Duplicate" avy-copy-region :if (lambda () (not buffer-read-only)))
      ("t" "Transpose" avy-transpose-lines-in-region :if (lambda () (and (not buffer-read-only) (region-active-p))))]

     ["Index & Errors"
      ("g" "Org Goto…" org-goto :if (lambda () (derived-mode-p 'org-mode)))
      ("i" "Index…" imenu :if (lambda () (bound-and-true-p imenu--index-alist)))
      ("M-p" "Previous" previous-error)
      ("M-n" "Next" next-error)]

     ["Exit"
      ("<escape>" "" transient-quit-one :format " ")
      ("RET" "Exit Avy" transient-quit-all)]]))

(ar/global-leader
  "y"   '(:ignore t :wk "avy/jump")
  "y y" '(ar/avy-transient :wk "Avy dashboard")
  "y c" '(avy-goto-char-timer :wk "Char timer")
  "y l" '(avy-goto-line :wk "Line")
  "y w" '(avy-goto-word-1 :wk "Word")
  "y s" '(avy-goto-symbol-1 :wk "Symbol"))

;; Enchant-based JIT spell checking with per-mode exclusions, an inline
;; personal vocabulary, and a large-file guard.  Disabled-mode list pruned to
;; modes that exist in the new package set (neotree/dashboard/vterm/pdf
;; dropped).
;;
;; Deferred via `text-mode-hook' instead of a global mode: the disabled-mode
;; list below already excluded prog-mode and conf-mode (and every other
;; listed mode derives from one of them or is a UI buffer), so a global
;; jinx-mode could never highlight there -- hooking the prose mode keeps the
;; identical visible behavior while keeping jinx and its native module out
;; of startup entirely.  org/markdown/gfm buffers run `text-mode-hook'
;; through derived-mode inheritance, so one hook covers the whole prose
;; family.
(use-package jinx
  :defer t
  :init
  (defvar ar/jinx-disabled-modes
    '(prog-mode
      conf-mode
      emacs-lisp-mode
      dired-mode
      ibuffer-mode
      treemacs-mode
      magit-status-mode
      magit-log-mode
      magit-diff-mode
      magit-branch-mode
      org-agenda-mode
      org-src-mode
      which-key-mode
      help-mode
      Info-mode
      embark-collect-mode
      ghostel-mode
      term-mode)
    "Modes in which Jinx should not activate.")
  ;; Inline personal vocabulary (lowercase entries for broadest coverage).
  (defvar ar/jinx-personal-words
    "emacs elisp use-package org-mode prog-mode conf-mode text-mode
eglot eldoc flymake minibuffer minimap config flycheck env corfu vertico orderless consult embark denote treemacs dirvish magit evil avy anzu jinx
tokyo night jetbrainsmono nerd font pgtk wayland ui
tramp ssh json yaml toml lsp dap ast"
    "Personal vocabulary injected as `jinx-local-words' in prose buffers.")
  ;; Per-buffer activation in prose buffers.  `add-hook' prepends, so the
  ;; vocabulary injection below lands ahead of `jinx-mode' in
  ;; `text-mode-hook' and the words are buffer-local before the mode starts
  ;; scanning.
  (add-hook 'text-mode-hook #'jinx-mode)
  ;; Injects the personal vocabulary into prose and markup buffers.  Done in
  ;; :init (it needs no jinx binding), so the hooks are in place even for
  ;; the very first prose buffer that loads the package.
  (dolist (mode '(org-mode
                  LaTeX-mode
                  markdown-mode
                  gfm-mode
                  text-mode))
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (setq-local jinx-local-words ar/jinx-personal-words))))
  :custom
  ;; Deterministic English regardless of system locale.
  (jinx-languages "en")
  :config
  (setq jinx-exclude-regexps
        `((emacs-lisp-mode "Package-Requires:.*$")
          (org-mode ,(rx "\\" (+ alpha)))
          (LaTeX-mode ,(rx "\\" (+ alpha)))
          (t "[A-Z]+\\>"
             "-+\\>"
             "\\w*?[0-9]\\w*\\>"
             "[a-z]+://\\S-+"
             "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
             "\\(?:Local Variables\\|End\\):\\s-*$"
             "jinx-\\(?:languages\\|local-words\\):\\s-+.*$"
             "[/~][a-zA-Z0-9._/-]+")))

  ;; jinx's default `jinx-exclude-faces' for org-mode misses heading text and
  ;; document metadata faces; inject them.
  (setf (alist-get 'org-mode jinx-exclude-faces)
        (append (alist-get 'org-mode jinx-exclude-faces)
                '(org-level-1 org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8
                  org-document-title org-document-info
                  org-outline org-priority)))

  ;; Aborts activation in massive buffers; bypasses guard for explicit toggles.
  (define-advice jinx-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p))))
  ;; Enforces the custom disabled-mode list (`jinx-disabled-modes' does not
  ;; exist in jinx itself -- checked through 2.11).
  (define-advice jinx--on (:before-while () guard-disabled-modes)
    (not (apply #'derived-mode-p ar/jinx-disabled-modes))))

;; Rich help buffers with source, callers, and keybindings, remapped over the
;; describe-* family.
(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function
             helpful-symbol)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  ;; Limits open helpful buffers; kills the least-recently-used beyond this.
  (helpful-max-buffers 7)
  ;; Apropos searches all symbols, not only user-facing ones.
  (apropos-do-all t))

;; TODO keyword highlighting and navigation with Tokyo-Night keyword faces.
;; FIX: the old config's ":hook (after-init . global-hl-line-mode)" enabled
;; the wrong mode (and after-init-hook never fires in batch), so TODO
;; highlighting was never active; this loads hl-todo eagerly and enables the
;; real global mode.
(use-package hl-todo
  :demand t
  :bind (:map hl-todo-mode-map
              ("C-c n" . hl-todo-next)
              ("C-c p" . hl-todo-previous)
              ("C-c i" . hl-todo-insert))
  :custom-face
  (hl-todo ((t (:inherit font-lock-keyword-face :weight bold))))
  :custom
  (hl-todo-require-punctuation nil)
  (hl-todo-highlight-punctuation ":!")
  (hl-todo-keyword-faces
   '(("TODO"       . "#e0af68")
     ("COMMENT"    . "#e0af68")
     ("FIXME"      . "#f7768e")
     ("BUG"        . "#f7768e")
     ("ISSUE"      . "#f7768e")
     ("PROG"       . "#7aa2f7")
     ("TRICK"      . "#ff9e64")
     ("WORKAROUND" . "#ff9e64")
     ("BENCHMARK"  . "#9ece6a")
     ("REVIEW"     . "#7dcfff")
     ("NOTE"       . "#bb9af7")
     ("DEPRECATED" . "#73daca")
     ("HACK"       . "#db4b4b")
     ("DEBUG"      . "#e0af68")
     ("STUB"       . "#e0af68")))
  :config
  (global-hl-todo-mode 1)
  ;; Safe registration of consult preview hooks (consult lives in the
  ;; completion module; the variable exists there).
  (with-eval-after-load 'consult
    (when (boundp 'consult-preview-allowed-hooks)
      (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode)
      (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode)))
  (defun ar/hl-todo-rg-project ()
    "Search for TODO keywords across the current project using ripgrep."
    (interactive)
    (let ((regexp (concat "\\b(" (string-join (mapcar #'car hl-todo-keyword-faces) "|") ")\\b")))
      (cond
       ((fboundp 'consult-ripgrep)
        (consult-ripgrep nil regexp))
       ((fboundp 'hl-todo-rgrep)
        (hl-todo-rgrep regexp))
       (t
        (user-error "Neither consult-ripgrep nor hl-todo-rgrep is available"))))))

;; Global leader for buffer and project TODO searches.  consult-todo /
;; consult-todo-project are completion-module packages (autoloaded), bound
;; here to keep the old SPC s t/T/C/P quartet intact.
(ar/global-leader
  "s t" '(hl-todo-occur :wk "Todo list (buffer)")
  "s T" '(ar/hl-todo-rg-project :wk "Todo in project (rg)")
  "s C" '(consult-todo :wk "Todo list (consult)")
  "s P" '(consult-todo-project :wk "Todo in project (consult)"))

;; Local leader (SPC m) for hl-todo navigation in hl-todo buffers.
(ar/local-leader
  :keymaps 'hl-todo-mode-map
  "n" '(hl-todo-next :wk "Next todo")
  "p" '(hl-todo-previous :wk "Previous todo")
  "i" '(hl-todo-insert :wk "Insert todo"))

;; Reopen the current file via TRAMP sudo.  The SPC f E entry is bound here
;; because the command lives in this module; the rest of the SPC f tree is
;; bound by the keybinds module's leader-trees block.
(use-package sudo-edit
  :defer t
  :commands (sudo-edit))

(ar/global-leader
  "f E" '(sudo-edit :wk "Sudo edit"))

(provide 'editor-config)
;;; editor-config.el ends here
