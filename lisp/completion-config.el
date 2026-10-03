;;; completion-config.el --- Minibuffer and in-buffer completion stacks -*- lexical-binding: t; -*-

;;; Commentary:

;; The vertico/consult/marginalia/orderless/embark minibuffer stack and
;; the corfu/cape in-buffer completion stack.

;;; Code:

;; Orderless -- multi-component matching for minibuffer and in-buffer
;; completion.  Deferred on purpose: `orderless' registers its completion
;; style from its autoloads (the `add-to-list' on `completion-styles-alist'
;; ships with an autoload cookie), and the `:custom' forms below run
;; immediately at startup, so the style is active from the first minibuffer
;; use while the library itself loads lazily.
(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-pcm-leading-wildcard t)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  ;; Prioritize fast literal matching before falling back to regex compilation.
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  :config
  ;; Explicit affix dictionary for predictable Vim-like dispatch routing.
  (setq orderless-affix-dispatch-alist
        `((?! . orderless-without-literal)
          (?= . orderless-literal)
          (?% . orderless-initialism)
          (?~ . orderless-flex)
          (?, . orderless-literal-prefix)
          (?& . ,(if (fboundp 'orderless-annotation)
                     'orderless-annotation
                   'orderless-literal))))

  (defun ar/orderless-disambiguation-dispatch (word _index _total)
    "Handle disambiguation suffixes for Consult and file extensions."
    (let ((tofu-re (if (boundp 'consult--tofu-regexp)
                       (concat consult--tofu-regexp "*\\'")
                     "\\'")))
      (cond
       ;; Exact end-of-string match for Consult commands.
       ((string-suffix-p "$" word)
        `(orderless-regexp . ,(concat (substring word 0 -1) tofu-re)))
       ;; File extension matching in minibuffer and shell modes.
       ((and (or minibuffer-completing-file-name
                 (derived-mode-p 'eshell-mode))
             (string-prefix-p "." word))
        `(orderless-regexp . ,(concat "\\." (substring word 1) tofu-re))))))

  ;; Chains the native affix dispatcher with the custom disambiguation dispatcher.
  (setq orderless-style-dispatchers
        '(orderless-affix-dispatch
          ar/orderless-disambiguation-dispatch)))

;; Vertico -- vertical minibuffer completion UI.  `vertico-mode' is enabled
;; in :init, which loads vertico eagerly at startup (part of the design's
;; sanctioned eager-load budget); the customizations below are applied to
;; the loaded package.
(use-package vertico
  :defer t
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (minibuffer-depth-indicate-mode 1)
  (vertico-mode 1)
  :custom
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle nil)
  :bind (:map vertico-map
              ;; Group navigation via arrow keys.
              ("C-<down>" . vertico-next-group)
              ("C-<up>"   . vertico-previous-group)))

;; Vertico bundled extensions.  These ship inside the vertico package; they
;; are configured here, never installed as separate packages.

;; vertico-directory: RET/DEL act on the path component in the minibuffer.
(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; vertico-quick: Avy-style rapid candidate selection.
(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
              ("M-q" . vertico-quick-jump)
              ("M-Q" . vertico-quick-exit)
              ("M-i" . vertico-quick-insert)))

;; vertico-repeat: save and replay minibuffer completion sessions.
(use-package vertico-repeat
  :after vertico
  :init
  ;; Registers the save hook via autoloads to capture all sessions.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; Replay the last completion session from any evil state.
(general-define-key
 :states '(normal insert visual motion)
 "C-x M-r" #'vertico-repeat-select)

;; vertico-multiform: per-command/category layouts plus candidate
;; transformers (directory highlighting, enabled-mode highlighting).
(use-package vertico-multiform
  :after vertico
  :config
  ;; Registers the `dired-directory' face (defined in dired.el) without
  ;; dragging dired into startup: a bare `(require 'dired)' here used to
  ;; fire while vertico's eager :init ran, costing ~8 ms on every start
  ;; (plus evil-collection-dired loading on top of it).  The first idle
  ;; moment is early enough for the directory highlighter below; batch runs
  ;; never fire idle timers, so the gates correctly see dired unloaded.
  (run-with-idle-timer 2 nil (lambda () (require 'dired)))

  ;; Intercepts candidate formatting to apply per-category text transformations.
  (defvar ar/vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start)
    (when ar/vertico-transform-functions
      (dolist (fun (ensure-list ar/vertico-transform-functions))
        (setq cand (funcall fun cand))))
    (cl-call-next-method cand prefix suffix index start))

  ;; Highlights directories with a trailing slash using Dired faces.
  (defun ar/vertico-highlight-directory-fn (file)
    (when (string-suffix-p "/" file)
      (setq file (copy-sequence file))
      (add-face-text-property 0 (length file) 'dired-directory 'append file))
    file)

  ;; Highlights already-enabled modes in M-x using the originating buffer.
  (defun ar/vertico-highlight-enabled-mode-fn (cmd)
    (let ((sym (intern-soft cmd)))
      (when sym
        (let ((origin-buf (window-buffer (minibuffer-selected-window))))
          (when (and origin-buf (buffer-live-p origin-buf))
            (with-current-buffer origin-buf
              (when (or (eq sym major-mode)
                        (and (memq sym minor-mode-list)
                             (boundp sym)
                             (symbol-value sym)))
                (setq cmd (copy-sequence cmd))
                (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))))))
    cmd)

  ;; Configures per-command and per-category display rules.
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-outline buffer)
          (consult-line-multi buffer)
          (consult-yasnippet grid)
          (execute-extended-command
           (ar/vertico-transform-functions . ar/vertico-highlight-enabled-mode-fn))))

  (setq vertico-multiform-categories
        '((file
           (ar/vertico-transform-functions . ar/vertico-highlight-directory-fn))
          (embark-keybinding grid)))

  ;; Activates the multiform engine post-load.
  (vertico-multiform-mode 1))

;; Consult -- async search and navigation backbone.  (The C-s
;; `consult-history' minibuffer binding is owned by the keybinds module's
;; `ar/minibuffer-escape-maps' loop: that variable is defined in keybinds-config.el,
;; which loads after completion, so a boundp-guarded copy here was dead code
;; that could never fire.)

(use-package consult
  :bind (;; Buffer switching
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ;; Navigation
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ([remap bookmark-jump] . consult-bookmark))
  :init
  ;; Routes register previews through Consult's formatting engine.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Routes xref locations through Consult's preview engine.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Starts consult-line from the top of the buffer.
  (setq consult-line-start-from-top t)
  ;; Configures hidden files and .git exclusion for ripgrep and find.
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden --glob '!.git'"
        consult-find-args "find . -not -path '*/.git/*'")
  ;; Configures preview debouncing per command category.
  (consult-customize
   consult-line consult-line-multi consult-imenu consult-imenu-multi
   :preview-key 'any
   consult-theme
   :preview-key '(:debounce 0.2 any)
   ;; Disabled previews to eliminate I/O overhead on massive buffer/recentf lists.
   consult-buffer consult-recent-file
   :preview-key nil
   consult-ripgrep consult-git-grep consult-grep consult-find consult-fd
   :preview-key '(:debounce 0.4 any)
   consult-goto-line consult-mark consult-global-mark
   consult-outline consult-bookmark
   :preview-key '(:debounce 0.4 any))
  ;; Configures narrowing key and help binding.
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;; consult-dir: directory jumping for the minibuffer.  (The old
;; `consult-dir-jump-file-command' custom is redundant, not absent:
;; consult-dir 0.2 defines it with default #'consult-find
;; (consult-dir.el:172), which is exactly what `consult-dir-jump-file' uses
;; out of the box.  The f d / f j / s d bindings that reach this package
;; live in the keybinds module's leader-trees block.)
(use-package consult-dir
  :after consult
  :commands (consult-dir
             consult-dir-jump-file))

;; consult-todo: hl-todo keywords through the consult minibuffer.
(use-package consult-todo
  :defer t
  :after (consult hl-todo)
  :custom
  ;; Map narrowing characters to the 15 custom hl-todo keywords.
  (consult-todo-narrow '((?t . "TODO")
                         (?c . "COMMENT")
                         (?f . "FIXME")
                         (?b . "BUG")
                         (?i . "ISSUE")
                         (?p . "PROG")
                         (?r . "TRICK")
                         (?w . "WORKAROUND")
                         (?B . "BENCHMARK")
                         (?R . "REVIEW")
                         (?n . "NOTE")
                         (?d . "DEPRECATED")
                         (?h . "HACK")
                         (?D . "DEBUG")
                         (?s . "STUB")))
  ;; Route uncategorized keywords under the 'o' character.
  (consult-todo-other '(?o . "OTHER"))
  ;; Disable comment-only restriction to include Org headings and Markdown prose.
  (consult-todo-only-comment nil)
  ;; Enable debounced live buffer previews for project searches.
  (consult-todo-dir-preview-key '(:debounce 0.2 any))
  ;; Cache threshold for large project greps in seconds.
  (consult-todo-cache-threshold 5.0))

;; Marginalia -- rich candidate annotations, enabled eagerly (part of the
;; sanctioned eager-load budget); nerd-icons-completion layers icons on top.
(use-package marginalia
  :init
  (marginalia-mode 1)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package nerd-icons-completion
  :after marginalia
  :init
  ;; Activates the icon injection engine immediately via autoloads.
  (nerd-icons-completion-mode 1)
  :config
  ;; Synchronizes icon state if Marginalia is toggled manually later.
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Embark -- context-aware action menus for minibuffer candidates and
;; buffer targets, with the consult exporters and org targets.
(use-package embark
  :bind
  ;; Global and minibuffer bindings for Embark actions.
  (("C-;" . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-;" . embark-act)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect))
  :init
  ;; Replaces which-key prefix discovery with Embark's target discovery.
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hides the mode-line in Embark Collect buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Configures minimal and highlight indicators.
  (setq embark-indicators
        '(embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

;; embark-consult: live collect/export glue between Embark and Consult.
(use-package embark-consult
  :after (embark consult))

;; embark-org: Org-heading targets for Embark (ships inside embark).
(use-package embark-org
  :after (embark org))

;; Native completion behavior: complete-at-point on TAB, mode-filtered M-x
;; candidates, and no synchronous ispell word completion in text modes.
(setopt tab-always-indent 'complete
        text-mode-ispell-word-completion nil
        read-extended-command-predicate #'command-completion-default-include-p)

;; Corfu -- in-buffer completion popup, global outside ephemeral buffers.
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-count 5)
  (corfu-max-width 80)
  ;; Preselects the prompt to prevent accidental candidate commits.
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-auto-delay 0.24)
  (corfu-auto-prefix 2)
  ;; Never pop up in chat, terminal, help or debugger buffers.
  (global-corfu-modes '((not erc-mode
                             eshell-mode
                             circe-mode
                             help-mode
                             gud-mode
                             vterm-mode
                             ghostel-mode
                             ghostel-compile-view-mode)
                        t))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
              ("TAB"       . corfu-next)
              ("<tab>"     . corfu-next)
              ("S-TAB"     . corfu-previous)
              ("<backtab>" . corfu-previous))
  :config
  ;; Free the RET key: commit happens on further input, not by accident.
  (keymap-unset corfu-map "RET")

  ;; Guards the minibuffer against Corfu popups and routes CAPF to Vertico.
  (defun ar/corfu-minibuffer-guard ()
    "Disable Corfu in the minibuffer and route completion-in-region to Vertico."
    (setq-local corfu-auto nil)
    (corfu-mode -1)
    ;; Route minibuffer CAPF (like cape-history) to Vertico via Consult.
    (when (fboundp 'consult-completion-in-region)
      (setq-local completion-in-region-function #'consult-completion-in-region)))
  (add-hook 'minibuffer-setup-hook #'ar/corfu-minibuffer-guard)
  ;; Closes the popup on insert state exit or buffer save.
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  (advice-add #'save-buffer :before (lambda (&rest _) (corfu-quit))))

;; nerd-icons-corfu: file/mode glyphs in the Corfu margin.  Registered the
;; moment corfu loads; the formatter itself is autoloaded.
(use-package nerd-icons-corfu
  :defer t
  :init
  (with-eval-after-load 'corfu
    ;; Registers the formatter with Corfu's margin API.
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; Cape -- completion-at-point wrappers for the global and local pipelines.
;; Loads with corfu (whose :hook fires global-corfu-mode at after-init):
;; cape is not in the design's eager-load budget, and the capf construction
;; below must call cape's own `cape-capf-*' builders -- an :init body would
;; have loaded the package during module load.  The advices are safe even
;; though lsp-completion/comint are not loaded yet: advice-add registers on
;; the symbol and activates when the function gets defined.

;; The lsp-completion capf ships inside the lsp-mode package; declaring it
;; here only satisfies the byte-compiler (the advice below must not require
;; lsp-completion).
(declare-function lsp-completion-at-point "lsp-completion")

(use-package cape
  :after corfu
  :config
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions (cape-capf-inside-string #'cape-file))
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))

  (add-hook 'comint-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-history nil t)))
  (add-hook 'minibuffer-setup-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-history nil t)))
  ;; Enables unwrapped file completion locally for shell scripts.
  (add-hook 'sh-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
  (add-hook 'bash-ts-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
  ;; lsp-completion's capf is exclusive by default; make it coexist with cape
  ;; and yasnippet-capf.  (The old config wrapped lsp-mode's capf the same
  ;; way; its noninterruptible/buster wrappers were lsp-mode-specific and
  ;; have no proven counterpart here.)
  ;; lsp-completion-at-point returns its plist WITHOUT an `:exclusive'
  ;; property (lsp-completion.el:655-678), so it is exclusive by capf
  ;; convention: when the server returns nothing, cape-file / cape-dabbrev /
  ;; yasnippet-capf would never be tried.  Same rationale the eglot advice
  ;; carried.  advice-add on the not-yet-defined symbol activates when
  ;; lsp-completion.el loads.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

;; Dabbrev -- dynamic abbreviation expansion from open buffers.

(defcustom ar/dabbrev-buffer-scanning-size-limit (* 1 1024 1024)
  "Size limit in characters for a buffer to be scanned by dabbrev."
  :type 'integer
  :group 'dabbrev)

(defun ar/dabbrev-friend-buffer-p (other-buffer)
  "Return non-nil if OTHER-BUFFER is safe and under the size limit."
  (and (buffer-live-p other-buffer)
       (< (buffer-size other-buffer) ar/dabbrev-buffer-scanning-size-limit)))

(use-package dabbrev
  :bind (;; Swaps M-/ and C-M-/ for in-buffer completion routing.
         ("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-friend-buffer-function #'ar/dabbrev-friend-buffer-p)
  (dabbrev-upcase-means-case-search t)
  ;; Enables cross-buffer scanning for global word discovery.
  (dabbrev-check-other-buffers nil)
  :config
  ;; O(1) exact string matching for known ephemeral and internal buffers.
  (setq dabbrev-ignored-buffer-names '("*Messages*"
                                       "*scratch*"
                                       "*Backtrace*"
                                       "*Completions*"
                                       "*Warnings*"))
  ;; Ignores hidden and internal Emacs buffers.
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Ignores TAGS and generated index files.
  (add-to-list 'dabbrev-ignored-buffer-regexps
               "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
  ;; Excludes binary, terminal, and machine-generated output buffers.
  (dolist (mode '(doc-view-mode
                  pdf-view-mode
                  dirvish-directory-view-mode
                  vterm-mode
                  term-mode
                  eshell-mode
                  ghostel-mode
                  ghostel-compile-view-mode
                  tags-table-mode
                  authinfo-mode
                  compilation-mode
                  magit-status-mode
                  magit-log-mode
                  special-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))

;; Grep -- the search plumbing beneath consult-ripgrep and xref: ripgrep
;; templates for the *grep* family, xref backed by ripgrep, and evil states
;; for the result and edit buffers.
(use-package grep
  :commands (grep
             lgrep
             rgrep
             zgrep
             zrgrep)
  :custom
  ;; Prevents automatic buffer saving before search execution.
  (grep-save-buffers nil)
  ;; Groups search results by filename with visual separators.
  (grep-use-headings t)
  :config
  ;; `grep-apply-setting' persists templates across `grep-compute-defaults' cycles.
  (when-let* ((rg (executable-find "rg")))
    (grep-apply-setting 'grep-command (format "%s -nH --null --no-heading --color=never -e " rg))
    ;; `<X>` and `<C>` are dropped; `rg` lacks `--exclude` and ANSI colors corrupt `compilation-mode`.
    (grep-apply-setting 'grep-template
                        (format "%s -nH --null --no-heading --color=never -e <R> <F>" rg))
    (grep-apply-setting 'grep-find-command (format "%s -nH --null --no-heading --color=never -e " rg))
    ;; Routes `<X>` to `find` to bypass `rg`'s missing `--exclude` flag, then `-exec`s into `rg`.
    (grep-apply-setting 'grep-find-template
                        (format "find <D> <X> -type f <F> -exec %s -nH --null --no-heading --color=never -e <R> {} +" rg))
    ;; Routes `xref-find-references` natively to ripgrep's regex engine.
    (setq xref-search-program 'ripgrep))
  ;; Forces Evil into normal state for read-only navigation.
  (evil-set-initial-state 'grep-mode 'normal)
  ;; Forces Evil into normal state for the writable edit mode.
  (evil-set-initial-state 'grep-edit-mode 'normal)
  ;; Injects Vim "write and quit" parity for grep mutations.  Emacs 31.1's
  ;; grep-edit-mode only provides `grep-edit-save-changes'; the old config
  ;; bound the nonexistent `grep-edit-save' and `grep-edit-abort' (ZZ/ZQ),
  ;; so only ZZ is bound -- C-g aborts an edit session natively.
  (evil-define-key 'normal grep-edit-mode-map
    "ZZ" #'grep-edit-save-changes))

;; Module feature marker; init.el requires this after core-config/editor-config/ui-config.
(provide 'completion-config)
;;; completion-config.el ends here
