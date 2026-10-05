;;; tools-config.el --- Jupyter, evaluation, lookup, LLM, pdf tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; The tools module (:tools parity): ein (Jupyter notebooks), eval
;; (quickrun + eros), lookup (dumb-jump + online providers), llm (gptel
;; suite), and pdf (pdf-tools).

;;; Code:

(declare-function pdf-view-next-line-or-next-page "pdf-view")
(declare-function pdf-view-previous-line-or-previous-page "pdf-view")
(declare-function pdf-view-next-page "pdf-view")
(declare-function pdf-view-previous-page "pdf-view")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view")

;; ein: tame Jupyter notebooks with emacs.
(use-package ein
  :defer t
  :commands (ein:run ein:login ein:run-remote ein:notebooklist-open)
  :custom
  (ein:output-area-inlined-images t))

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
(use-package dumb-jump
  :defer t
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package request
  :defer t)

(ar/global-leader
  "c g x" '(dumb-jump-go-prefer-external :wk "Definition (external)"))

;; llm: LLM integration powered by gptel (ChatGPT default backend).
(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-add gptel-add-file gptel-menu)
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'gpt-4o)
  (gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming . "You are an experienced software engineer. Reply with code only, no explanations unless asked.")
     (writing . "You are a professional writing assistant. Improve clarity, flow and correctness."))))

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

(ar/global-leader
  "o l" '(gptel :wk "Open gptel chat") ; rebinding: org-store-link moves below
  "o L" '(org-store-link :wk "Store org link")
  "o g" '(gptel-menu :wk "gptel menu")
  "o q" '(gptel-quick :wk "Quick explain at point"))

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
  (with-eval-after-load 'evil-collection-pdf
    nil)
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
