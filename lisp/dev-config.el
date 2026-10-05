;;; dev-config.el --- IDE tooling: lsp-mode + lsp-ui, flymake, dape, apheleia -*- lexical-binding: t; -*-

;;; Commentary:

;; The IDE module: tree-sitter grammar management, lsp-mode with lsp-ui,
;; flymake with flymake-popon, dape debugging, apheleia formatting, and
;; envrc.  The optional emacs-lsp-booster binary integration is included.

;;; Code:

(declare-function ar/lsp-sync-workspaces "dev-config")
(declare-function ar/var-file "early-init")
(defvar lsp-ui-mode-map)

;; Tree-sitter grammar management.  Stays eager: the remap into
;; `major-mode-remap-alist' is what routes source files to *-ts-mode in
;; the first place.
(use-package treesit
  :if (treesit-available-p)
  :preface
  (defvar ar/treesit-grammar-dir
    (ar/var-file "tree-sitter/")
    "Target directory for compiled tree-sitter grammars.")
  :init
  (unless (file-directory-p ar/treesit-grammar-dir)
    (make-directory ar/treesit-grammar-dir t))
  :custom
  (treesit-extra-load-path (list ar/treesit-grammar-dir))
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes t)
  :config
  (setq treesit-font-lock-level 4)
  ;; Registers non-built-in grammar sources (Emacs 31 ships typescript,
  ;; toml, yaml, dockerfile, rust).  No :commit pins on purpose.
  (dolist (source '((python "https://github.com/tree-sitter/tree-sitter-python")
                    (bash "https://github.com/tree-sitter/tree-sitter-bash")
                    (c "https://github.com/tree-sitter/tree-sitter-c")
                    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                    (css "https://github.com/tree-sitter/tree-sitter-css")
                    (html "https://github.com/tree-sitter/tree-sitter-html")
                    (json "https://github.com/tree-sitter/tree-sitter-json")
                    (latex "https://github.com/latex-lsp/tree-sitter-latex")
                    (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
                    (nix "https://github.com/nix-community/tree-sitter-nix")
                    (java "https://github.com/tree-sitter/tree-sitter-java")
                    (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
                    (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
                    (awk "https://github.com/BeagleFoot/tree-sitter-awk")
                    (org "https://github.com/milisims/tree-sitter-org")))
    (setf (alist-get (car source) treesit-language-source-alist) (cdr source))))

;; Language servers: lsp-mode.  `ar/lsp-ensure-maybe' starts lsp from
;; `prog-mode-hook' for every mode except the excluded list; interactive
;; sessions defer through `lsp-deferred', batch starts `lsp' directly.
(defcustom ar/lsp-excluded-modes
  '(emacs-lisp-mode lisp-interaction-mode lisp-mode
    makefile-mode makefile-ts-mode snippet-mode ron-mode)
  "Major modes in which lsp auto-start is never attempted."
  :type '(repeat symbol)
  :group 'lsp-mode)

(declare-function lsp-workspace-folders-add "lsp-mode")
(declare-function lsp-session "lsp-mode")
(declare-function lsp--session-workspaces "lsp-mode")

(defun ar/lsp-ensure-maybe ()
  "Start lsp for the current buffer unless its mode is excluded.
Interactive sessions defer through `lsp-deferred'.  Batch runs import
the buffer directory as a workspace folder and call `lsp' directly."
  (unless (apply #'derived-mode-p ar/lsp-excluded-modes)
    (if noninteractive
        (progn
          (require 'lsp-mode)
          (lsp-workspace-folders-add default-directory)
          (lsp))
      (lsp-deferred))))

(defvar lsp-use-plists)

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred
             lsp-execute-code-action
             lsp-organize-imports
             lsp-rename
             lsp-find-declaration
             lsp-find-type-definition
             lsp-find-implementation
             lsp-inlay-hints-mode
             lsp-semantic-tokens-mode)
  :hook ((prog-mode . ar/lsp-ensure-maybe)
         (lsp-completion-mode . ar/lsp-completion-orderless))
  :preface
  (defun ar/lsp-completion-orderless ()
    "Use orderless for the `lsp-capf' completion category."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  ;; --- performance set ---
  (lsp-log-io nil)
  (lsp-idle-delay 0.500)
  (lsp-file-watch-threshold 2000)
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flymake)
  ;; --- UI features ---
  (lsp-semantic-tokens-enable t)
  (lsp-eldoc-render-all t)
  :config
  ;; Plist-consistency tripwire: lsp-mode records the compile-time value of
  ;; `lsp-use-plists'; a mismatch means the package was built without
  ;; LSP_USE_PLISTS and must be rebuilt.
  (with-eval-after-load 'lsp-completion
    (when (and (boundp 'lsp-completion-plist-value-when-compiled)
               (not (equal lsp-use-plists
                           lsp-completion-plist-value-when-compiled)))
      (warn "LSP_USE_PLISTS mismatch: lsp-mode compiled with lsp-use-plists=%s but runs with %s; rebuild lsp-mode after changing LSP_USE_PLISTS"
            lsp-completion-plist-value-when-compiled lsp-use-plists)))

  ;; Workspace observation bridge: live mirror of the lsp session's real
  ;; workspaces for batch observers.
  (defvar lsp--workspaces nil
    "Live mirror of the lsp session's real workspaces.")
  (defun ar/lsp-sync-workspaces (&rest _)
    "Refresh the `lsp--workspaces' mirror from the live lsp session."
    (setq lsp--workspaces (lsp--session-workspaces (lsp-session))))
  (add-hook 'lsp-after-initialize-hook #'ar/lsp-sync-workspaces)
  (add-hook 'lsp-after-open-hook #'ar/lsp-sync-workspaces)
  (add-hook 'lsp-after-uninitialized-functions #'ar/lsp-sync-workspaces))

;; lsp-ui: sideline diagnostics, hover doc popup, peek definitions.
(use-package lsp-ui
  :after lsp-mode
  :commands (lsp-ui-doc-show lsp-ui-doc-glance lsp-ui-sideline-mode lsp-ui-peek-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-imenu-buffer-position 'right)
  :bind (:map lsp-ui-mode-map
         ("C-c l d" . lsp-ui-doc-glance)
         ("C-c l p" . lsp-ui-peek-find-references)))

;; emacs-lsp-booster integration.  Fully optional: every effect below is
;; gated on the `emacs-lsp-booster' binary.
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json.
OLD-FN is the original JSON parser; fall back to it with ARGS when
the buffer does not start with bytecode."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD.
OLD-FN is the wrapped `lsp-resolve-final-command'."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(when (executable-find "emacs-lsp-booster")
  (advice-add (if (progn (require 'json)
                          (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; Workspace symbol search through consult.
(defvar lsp-mode-map)
(declare-function consult-lsp-symbols "consult-lsp")
(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Treemacs views over LSP data: errors list (problems panel) and symbols.
(use-package lsp-treemacs
  :defer t
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols))

;; Diagnostics: built-in flymake replaces flycheck, with flymake-popon
;; for at-point diagnostic popups.
(use-package flymake
  :defer t
  :commands (flymake-goto-next-error
             flymake-goto-prev-error
             flymake-show-buffer-diagnostics
             flymake-show-project-diagnostics
             flymake-start)
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy))

(use-package flymake-popon
  :after flymake
  :commands (flymake-popon-mode)
  :hook (flymake-mode . flymake-popon-mode))

;; Python: interpreter and indentation.
(use-package python
  :defer t
  :custom
  (python-shell-completion-native-enable nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

;; Formatting: apheleia runs formatters asynchronously on save (+onsave).
(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :custom
  (apheleia-hide-log-buffers t)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; Debugger: dape (DAP: breakpoints, stepping, REPL, watch).
(use-package dape
  :defer t
  :commands (dape dape-breakpoint-toggle dape-breakpoint-log dape-breakpoint-expression
             dape-continue dape-next dape-step-in dape-step-out dape-restart dape-kill
             dape-pause dape-disconnect dape-evaluate-expression dape-repl dape-info
             dape-quit)
  :custom
  (dape-buffer-window-arrangement 'gdb)
  ;; Project root (falling back to default-directory) is the default;
  ;; pinned explicitly so upstream default changes can't surprise us.
  (dape-cwd-function #'dape--default-cwd))

;; Direnv: per-directory environment isolation.
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Code leader tree (SPC c).
(ar/global-leader
  "c"   '(:ignore t :wk "code")
  "c a" '(lsp-execute-code-action :wk "Code actions (lightbulb)")
  "c i" '(lsp-organize-imports :wk "Organize imports")
  "c r" '(lsp-rename :wk "Rename symbol")
  "c f" '(apheleia-format-buffer :wk "Format buffer (async)")
  "c e" '(consult-flymake :wk "Diagnostics (consult)")
  "c E" '(flymake-show-project-diagnostics :wk "Project diagnostics")
  "c B" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
  "c !" '(flymake-start :wk "Force diagnostics run")
  "c H" '(lsp-inlay-hints-mode :wk "Toggle inlay hints")
  "c T" '(lsp-semantic-tokens-mode :wk "Toggle semantic tokens")
  "c D" '(xref-find-references :wk "Find references")
  "c A" '(consult-lsp-symbols :wk "Workspace symbols")
  "c x" '(lsp-treemacs-errors-list :wk "Problems panel (treemacs)")
  "c y" '(lsp-treemacs-symbols :wk "Symbols view (treemacs)")
  "c d"   '(:ignore t :wk "debug")
  "c d d" '(dape :wk "Start debugger (dape)")
  "c d b" '(dape-breakpoint-toggle :wk "Toggle breakpoint")
  "c d c" '(dape-continue :wk "Continue")
  "c d n" '(dape-next :wk "Next step")
  "c d s" '(dape-step-in :wk "Step in")
  "c d o" '(dape-step-out :wk "Step out")
  "c d r" '(dape-restart :wk "Restart")
  "c d q" '(dape-quit :wk "Quit / Kill")
  "c d i" '(dape-info :wk "Info / UI")
  "c d R" '(dape-repl :wk "REPL")
  "c g" '(:ignore t :wk "goto")
  "c g d" '(xref-find-definitions :wk "Definition")
  "c g D" '(lsp-find-declaration :wk "Declaration")
  "c g t" '(lsp-find-type-definition :wk "Type definition")
  "c g i" '(lsp-find-implementation :wk "Implementation"))

;; Diagnostics navigation.
(general-define-key
 :states 'motion
 "] e" #'flymake-goto-next-error
 "[ e" #'flymake-goto-prev-error)

;; Hover docs and rename.
(general-define-key
 :states 'normal
 "K" #'eldoc
 "F2" #'lsp-rename)

(provide 'dev-config)
;;; dev-config.el ends here
