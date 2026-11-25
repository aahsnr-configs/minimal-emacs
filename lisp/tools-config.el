;;; tools-config.el --- Jupyter, evaluation, lookup, LLM, pdf tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; The tools module (:tools parity): ein (notebooks) and emacs-jupyter
;; (kernel REPLs and Org Babel), eval (quickrun + eros), lookup (dumb-jump
;; + online providers), and pdf (pdf-tools). LLM configuration belongs to
;; llm-config.

;;; Code:

(declare-function pdf-view-next-line-or-next-page "pdf-view")
(declare-function pdf-view-previous-line-or-previous-page "pdf-view")
(declare-function pdf-view-next-page "pdf-view")
(declare-function pdf-view-previous-page "pdf-view")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view")
(declare-function jupyter-run-repl "jupyter-repl"
                  (kernel-name &optional repl-name associate-buffer client-class display))
(declare-function jupyter-connect-repl "jupyter-repl"
                  (file &optional repl-name associate-buffer client-class display))
(declare-function jupyter-server-list-kernels "jupyter-server" (server))
(declare-function jupyter-repl-associate-buffer "jupyter-repl" (client))
(defvar savehist-additional-variables)

;; ein: tame Jupyter notebooks with emacs.
(use-package ein
  :defer t
  :commands (ein:run ein:login ein:run-remote ein:stop
             ein:notebooklist-open)
  :custom
  (ein:output-area-inlined-images t))

;; emacs-jupyter: kernel REPLs, evaluation from source buffers, and the
;; Org Babel backend.  It needs the `jupyter' executable and `ipykernel' in
;; the active environment (pyvenv activates the project's .venv, so a
;; per-project kernel comes from `uv add --dev ipykernel'); the
;; zmq module compiles itself on first use and needs a C compiler and libzmq.
(use-package jupyter
  :defer t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-server-list-kernels
             jupyter-repl-associate-buffer
             jupyter-repl-pop-to-buffer
             jupyter-repl-scratch-buffer
             jupyter-repl-restart-kernel
             jupyter-repl-interrupt-kernel
             jupyter-eval-line-or-region
             jupyter-eval-string-command
             jupyter-eval-defun
             jupyter-eval-buffer
             jupyter-eval-remove-overlays
             jupyter-inspect-at-point)
  :custom
  ;; Short results show as an end-of-line overlay (like eros for Elisp).
  (jupyter-eval-use-overlays t)
  (jupyter-repl-history-maximum-length 1000))

;; These Org commands live in files separate from jupyter.el.
(autoload #'org-babel-jupyter-scratch-buffer "ob-jupyter" nil t)
(autoload #'jupyter-org-toggle-request-queuing "jupyter-org-client" nil t)

;; Persist named Jupyter kernels with the other savehist state in var/.
(with-eval-after-load 'jupyter-server
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables
                 'jupyter-server-kernel-names)))

;; eval: run code, run (also, repls) -- quickrun recipes + eros overlays.
(use-package quickrun
  :defer t
  :commands (quickrun quickrun-region quickrun-replace-region
                      quickrun-compile-only quickrun-shell)
  :custom
  (quickrun-focus-p nil))

;; eros: inline overlay display of evaluation results (primarily Elisp).
(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "⟹ "))

(ar/global-leader
  "o r" '(quickrun :wk "Run code (quickrun)")
  "o R" '(quickrun-region :wk "Run region"))

;; lookup: jump-to-definition fallback and online documentation providers.
;; The backend hook lives in :init: it is what loads dumb-jump, so putting it
;; in :config would never run.  consult-xref (completion-config) stays the
;; definitions UI.
(use-package dumb-jump
  :defer t
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg))

(use-package request
  :defer t)

(ar/global-leader
  "c g x" '(dumb-jump-go-prefer-external :wk "Definition (external)"))

;; gptel-quick: quick explanations at point.
(use-package gptel-quick
  :after gptel
  :commands (gptel-quick)
  :custom
  (gptel-quick-word-count 60))

;; gptel-magit: AI-assisted commit/tag messages inside magit.
(use-package gptel-magit
  :after (gptel magit)
  :commands (gptel-magit-commit-generate
             gptel-magit-diff-explain
             gptel-magit-tag-generate)
  :config
  (gptel-magit-install))

;; ob-gptel: LLM org-babel blocks.
(use-package ob-gptel
  :after (gptel org)
  :commands (org-babel-execute:gptel))

;; pdf: Emacs as a PDF reader via pdf-tools.
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-annot-activate-created-annotations t)
  :config
  ;; Wire the build-free loader only when the epdfinfo server already
  ;; exists; otherwise `M-x pdf-tools-install' builds it on demand.
  ;; Loading must never block on a compilation (startup or batch).
  (when (and (boundp 'pdf-info-epdfinfo-program)
             (file-exists-p pdf-info-epdfinfo-program))
    (pdf-loader-install 'no-query))
  ;; Evil bindings for pdf-view.
  (evil-define-key 'normal pdf-view-mode-map
    "j" #'pdf-view-next-line-or-next-page
    "k" #'pdf-view-previous-line-or-previous-page
    "l" #'pdf-view-next-page
    "h" #'pdf-view-previous-page
    "/" #'isearch-forward
    "d" #'pdf-view-scroll-up-or-next-page
    "u" #'pdf-view-scroll-down-or-previous-page))

;; Restore scroll position per file.
(use-package saveplace-pdf-view
  :after pdf-view)

;; Org links to PDFs with page precision (org-pdftools installs its own
;; org-link handler on load).
(use-package org-pdftools
  :after org
  :commands (org-pdftools-get-link org-pdftools-open))

(provide 'tools-config)
;;; tools-config.el ends here
