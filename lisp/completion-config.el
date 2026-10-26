;;; completion-config.el --- Minibuffer and in-buffer completion stacks -*- lexical-binding: t; -*-

;;; Commentary:

;; The vertico/consult/marginalia/orderless/embark minibuffer stack and
;; the corfu/cape in-buffer completion stack.

;;; Code:

(declare-function consult-narrow-help "consult")
(declare-function corfu-quit "corfu")
(declare-function grep-apply-setting "grep")
(declare-function ar/corfu-minibuffer-guard "completion-config")

;; Orderless -- multi-component matching for minibuffer and in-buffer
;; completion.  Registers its completion style from autoloads while the
;; library itself loads lazily.
(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-pcm-leading-wildcard t)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  :config
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
       ((string-suffix-p "$" word)
        `(orderless-regexp . ,(concat (substring word 0 -1) tofu-re)))
       ((and (or minibuffer-completing-file-name
                 (derived-mode-p 'eshell-mode))
             (string-prefix-p "." word))
        `(orderless-regexp . ,(concat "\\." (substring word 1) tofu-re))))))

  (setq orderless-style-dispatchers
        '(orderless-affix-dispatch
          ar/orderless-disambiguation-dispatch)))

;; Vertico -- vertical minibuffer completion UI.  Part of the sanctioned
;; eager-load budget.
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
              ("C-<down>" . vertico-next-group)
              ("C-<up>"   . vertico-previous-group)))

;; vertico-directory: RET/DEL act on the path component.
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
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(general-define-key
 :states '(normal insert visual motion)
 "C-x M-r" #'vertico-repeat-select)

;; vertico-multiform: per-command/category layouts plus candidate
;; transformers (directory highlighting, enabled-mode highlighting).
(use-package vertico-multiform
  :after vertico
  :config
  (defun ar/dired-require-idle-h ()
    "Load `dired' in the background so directory candidates pick up its faces."
    (require 'dired))
  (run-with-idle-timer 2 nil #'ar/dired-require-idle-h)

  (defvar ar/vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start)
    (when ar/vertico-transform-functions
      (dolist (fun (ensure-list ar/vertico-transform-functions))
        (setq cand (funcall fun cand))))
    (cl-call-next-method cand prefix suffix index start))

  (defun ar/vertico-highlight-directory-fn (file)
    "Highlight directory candidates with a trailing slash using Dired faces."
    (when (string-suffix-p "/" file)
      (setq file (copy-sequence file))
      (add-face-text-property 0 (length file) 'dired-directory 'append file))
    file)

  (defun ar/vertico-highlight-enabled-mode-fn (cmd)
    "Highlight already-enabled modes in M-x using the originating buffer."
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
                (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd))))))
      cmd))

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

  (vertico-multiform-mode 1))

;; Consult -- async search and navigation backbone.
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
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-line-start-from-top t)
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden --glob '!.git'"
        consult-find-args "find . -not -path '*/.git/*'")
  (consult-customize
   consult-line consult-line-multi consult-imenu consult-imenu-multi
   :preview-key 'any
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-buffer consult-recent-file
   :preview-key nil
   consult-ripgrep consult-git-grep consult-grep consult-find consult-fd
   :preview-key '(:debounce 0.4 any)
   consult-goto-line consult-mark consult-global-mark
   consult-outline consult-bookmark
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;; wgrep -- editable grep result buffers: apply search-and-replace to files.
(use-package wgrep
  :defer t
  :hook (grep-mode . wgrep-setup)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file nil)
  :config
  ;; Apply evil's ZZ write-and-quit parity to grep edits.
  (with-eval-after-load 'grep
    (evil-set-initial-state 'grep-mode 'normal)
    (evil-set-initial-state 'grep-edit-mode 'normal)))

;; consult-dir: directory jumping for the minibuffer.
(use-package consult-dir
  :after consult
  :commands (consult-dir
             consult-dir-jump-file))

;; Marginalia -- rich candidate annotations, enabled eagerly.
(use-package marginalia
  :init
  (marginalia-mode 1)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package nerd-icons-completion
  :after marginalia
  :init
  (nerd-icons-completion-mode 1)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Embark -- context-aware action menus for minibuffer candidates and
;; buffer targets, with the consult exporters and org targets.
(use-package embark
  :bind
  (("C-;" . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-;" . embark-act)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect))
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq embark-indicators
        '(embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult))

(use-package embark-org
  :after (embark org))

;; Native completion behavior.
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
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-auto-delay 0.24)
  (corfu-auto-prefix 2)
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
  (keymap-unset corfu-map "RET")

  (defun ar/corfu-minibuffer-guard ()
    "Disable Corfu in the minibuffer and route CAPF to Vertico."
    (setq-local corfu-auto nil)
    (corfu-mode -1)
    (when (fboundp 'consult-completion-in-region)
      (setq-local completion-in-region-function #'consult-completion-in-region)))
  (add-hook 'minibuffer-setup-hook #'ar/corfu-minibuffer-guard)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  (advice-add #'save-buffer :before (lambda (&rest _) (corfu-quit))))

;; nerd-icons-corfu: file/mode glyphs in the Corfu margin.
(use-package nerd-icons-corfu
  :defer t
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; Cape -- completion-at-point wrappers.
(declare-function lsp-completion-at-point "lsp-completion")

;; lsp-completion's capf is exclusive by default; make it coexist with
;; cape and yasnippet-capf.
(use-package cape
  :after corfu
  :config
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions (cape-capf-inside-string #'cape-file))
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))

  (defun ar/cape-history-local-h ()
    "Complete from buffer-local history in comint/minibuffer buffers."
    (add-hook 'completion-at-point-functions #'cape-history nil t))
  (defun ar/cape-file-local-h ()
    "Complete file names locally in shell buffers."
    (add-hook 'completion-at-point-functions #'cape-file nil t))
  (add-hook 'comint-mode-hook #'ar/cape-history-local-h)
  (add-hook 'minibuffer-setup-hook #'ar/cape-history-local-h)
  (add-hook 'sh-mode-hook #'ar/cape-file-local-h)
  (add-hook 'bash-ts-mode-hook #'ar/cape-file-local-h)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package yasnippet-capf
  :after (yasnippet corfu)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package consult-yasnippet
  :defer t
  :after (consult yasnippet)
  :commands (consult-yasnippet))

;; Dabbrev -- dynamic abbreviation expansion from open buffers.

(defcustom ar/dabbrev-buffer-scanning-size-limit (* 1 1024 1024)
  "Size limit in characters for a buffer to be scanned by dabbrev."
  :type 'integer
  :group 'ar)

(defun ar/dabbrev-friend-buffer-p (other-buffer)
  "Return non-nil if OTHER-BUFFER is safe and under the size limit."
  (and (buffer-live-p other-buffer)
       (< (buffer-size other-buffer) ar/dabbrev-buffer-scanning-size-limit)))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-friend-buffer-function #'ar/dabbrev-friend-buffer-p)
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-other-buffers nil)
  :config
  (setq dabbrev-ignored-buffer-names '("*Messages*"
                                       "*scratch*"
                                       "*Backtrace*"
                                       "*Completions*"
                                       "*Warnings*"))
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-regexps
               "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
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

;; Grep -- ripgrep templates for the *grep* family, xref backed by
;; ripgrep, and evil states for the result and edit buffers.
(use-package grep
  :commands (grep
             lgrep
             rgrep
             zgrep
             zrgrep)
  :custom
  (grep-save-buffers nil)
  (grep-use-headings t)
  :config
  (when-let* ((rg (executable-find "rg")))
    (grep-apply-setting 'grep-command (format "%s -nH --null --no-heading --color=never -e " rg))
    (grep-apply-setting 'grep-template
                        (format "%s -nH --null --no-heading --color=never -e <R> <F>" rg))
    (grep-apply-setting 'grep-find-command (format "%s -nH --null --no-heading --color=never -e " rg))
    (grep-apply-setting 'grep-find-template
                        (format "find <D> <X> -type f <F> -exec %s -nH --null --no-heading --color=never -e <R> {} +" rg))
    (setq xref-search-program 'ripgrep)))

;; Module feature marker.
(provide 'completion-config)
;;; completion-config.el ends here
