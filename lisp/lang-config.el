;;; lang-config.el --- Language stacks: data, elisp, json, latex, markdown, nix, python, qt, sh, web, yaml -*- lexical-binding: t; -*-

;;; Commentary:

;; The language module (:lang parity): per-language majors, tree-sitter
;; routing (owned by dev-config's treesit block), LSP hooks (owned by
;; lsp-mode's prog-mode auto-start), and per-language tooling.

;;; Code:

(defvar json-mode-map (make-sparse-keymap)
  "Declared before use-package :bind references it; the real map
arrives with json-mode.")
(declare-function LaTeX-math-mode "latex")
(declare-function bash-completion-capf-nonexclusive "bash-completion")

;; data: config/data formats.
(use-package csv-mode
  :defer t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :custom
  (csv-separators '("," ";" "\t")))

;; emacs-lisp: a parsel-tongue for the oldest serpent.
(use-package elisp-mode
  :ensure nil
  :defer t
  :custom
  (lisp-indent-offset nil)
  :config
  ;; highlight-quoted: highlight quoted symbols.
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package highlight-quoted
  :defer t)

;; macrostep: interactive macro-expansion.
(use-package macrostep
  :defer t
  :commands (macrostep-expand macrostep-mode)
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)))

;; overseer: evaluate ERT tests from point.
(use-package overseer
  :defer t
  :commands (overseer-test
             overseer-test-run-test
             overseer-test-this-buffer
             overseer-test-file
             overseer-help))

;; elisp-def: jump to elisp definitions.
(use-package elisp-def
  :defer t
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

;; let-completion: completion for `let'-bound variables.
(use-package let-completion
  :defer t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . let-completion-mode))

;; package-lint-flymake: lint package metadata via flymake (+flymake).
(use-package package-lint-flymake
  :defer t
  :after flymake
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;; buttercup: BDD test framework.
(use-package buttercup
  :defer t
  :commands (buttercup-run-discover buttercup-run-at-point))

;; json (+lsp +tree-sitter): json-mode/json-ts-mode via treesit remap.
(use-package json-mode
  :defer t
  :mode ("\\.json\\'" . json-mode)
  :custom
  (js-indent-level 2))

(use-package json-snatcher
  :defer t
  :commands (jsons-print-path)
  :bind (:map json-mode-map
         ("C-c C-p" . jsons-print-path)))

;; latex (+cdlatex +fold +lsp).
(use-package tex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server nil)
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (reftex-plug-into-AUCTeX t)
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  ;; +fold: folded sections via TeX-fold.
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

(use-package auctex-cont-latexmk
  :after tex
  :commands (auctex-cont-latexmk-mode)
  :hook (LaTeX-mode . auctex-cont-latexmk-mode))

;; +cdlatex: TAB-completion and environment/abbrev insertion.
(use-package cdlatex
  :defer t
  :commands (cdlatex-mode turn-on-cdlatex)
  :hook (LaTeX-mode . turn-on-cdlatex)
  :custom
  (cdlatex-tab-indent-limit 8))

;; evil-tex: LaTeX text objects/operators (+everywhere).
(use-package evil-tex
  :after (evil tex)
  :commands (evil-tex-mode)
  :hook (LaTeX-mode . evil-tex-mode))

;; markdown (+lsp +tree-sitter).
(use-package markdown-mode
  :defer t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-enable-math t))

(use-package markdown-toc
  :defer t
  :commands (markdown-toc-generate-toc markdown-toc-generate-or-refresh-toc))

;; markdown-ts-mode is built into Emacs 31: `treesit-enabled-modes' prefers
;; it over markdown-mode once the markdown grammar is available (dev-config
;; registers its source).  The GNU ELPA package of the same name refuses to
;; load on 31.x -- it must stay out of the manifest.

;; edit-indirect: edit code blocks in dedicated buffers.
(use-package edit-indirect
  :defer t
  :after markdown-mode)

;; evil-markdown (+everywhere): text objects and motions.
(use-package evil-markdown
  :after (evil markdown-mode)
  :commands (evil-markdown-mode)
  :hook (markdown-mode . evil-markdown-mode))

;; nix (+lsp +tree-sitter).
(use-package nix-mode
  :defer t
  :mode ("\\.nix\\'" . nix-mode)
  :custom
  (nix-nixfmt-bin "nixfmt"))

(use-package nix-update
  :defer t
  :commands (nix-update-fetch))

(use-package nixos-options
  :defer t
  :commands (nixos-options-get-documentation-for-option
             nixos-options-get-option-by-name))

(use-package nix-ts-mode
  :if (treesit-available-p)
  :defer t
  :mode ("\\.nix\\'" . nix-ts-mode))

;; python (+lsp +tree-sitter +conda).
(use-package pip-requirements
  :defer t
  :mode ("^requirements\\(\\.\\(in\\|txt\\)\\)?\\'" . pip-requirements-mode))

(use-package pipenv
  :defer t
  :commands (pipenv-activate pipenv-deactivate pipenv-shell)
  :custom
  (pipenv-executable "pipenv"))

(declare-function pyvenv-activate "pyvenv")

(defun ar/pyvenv-activate-project-venv ()
  "Activate a project-local .venv when present."
  (when-let* ((root (when buffer-file-name (locate-dominating-file buffer-file-name ".venv")))
              (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      (pyvenv-activate venv))))

(use-package pyvenv
  :defer t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :hook (python-mode . ar/pyvenv-activate-project-venv))

(use-package conda
  :defer t
  :commands (conda-env-activate conda-env-deactivate conda-env-list)
  :custom
  (conda-anaconda-home (expand-file-name "~/miniconda3")))

(autoload #'nosetests-one "nose" nil t)
(autoload #'nosetests-all "nose" nil t)

(use-package python-pytest
  :defer t
  :commands (python-pytest python-pytest-file python-pytest-function)
  :custom
  (python-pytest-executable "pytest"))

;; qt (+lsp +tree-sitter): qml modes.
(use-package qml-mode
  :defer t
  :mode ("\\.qml\\'" . qml-mode))

(use-package qt-pro-mode
  :defer t
  :mode ("\\.pro\\'" . qt-pro-mode)
  :mode ("\\.pri\\'" . qt-pro-mode))

(use-package qml-ts-mode
  :if (treesit-available-p)
  :defer t
  :mode ("\\.qml\\'" . qml-ts-mode))

;; sh: sh-mode + native bash completion via CAPF (no +lsp).
(defun ar/sh-bash-completion-setup ()
  "Add the bash completion CAPF buffer-locally in shell buffers."
  (add-hook 'completion-at-point-functions
            #'bash-completion-capf-nonexclusive nil t))

(use-package bash-completion
  :defer t
  :after sh-script
  :config
  (add-hook 'bash-ts-mode-hook #'ar/sh-bash-completion-setup)
  (add-hook 'sh-mode-hook #'ar/sh-bash-completion-setup))

;; web (+lsp +tree-sitter).
(use-package web-mode
  :defer t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.handlebars\\'" "\\.hbs\\'"
         "\\.vue\\'" "\\.html?\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-current-element-highlight t))

(use-package emmet-mode
  :defer t
  :hook ((web-mode css-mode html-mode) . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t))

(dolist (pair '(("\\.haml\\'" . haml-mode)
                ("\\.pug\\'" . pug-mode)
                ("\\.jade\\'" . pug-mode)
                ("\\.slim\\'" . slim-mode)
                ("\\.sass\\'" . sass-mode)
                ("\\.scss\\'" . scss-mode)
                ("\\.styl\\'" . stylus-mode)))
  (add-to-list 'auto-mode-alist pair))

(use-package haml-mode
  :defer t)

(use-package pug-mode
  :defer t)

(use-package slim-mode
  :defer t)

(use-package sass-mode
  :defer t)

(use-package stylus-mode
  :defer t)

(use-package sws-mode
  :defer t)

;; rainbow-mode: colorize color literals in css/web buffers.
(use-package rainbow-mode
  :defer t
  :hook ((css-mode scss-mode less-css-mode css-ts-mode html-mode web-mode)
         . rainbow-mode))

;; yaml (+lsp +tree-sitter): yaml-mode + built-in yaml-ts-mode.
(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(provide 'lang-config)
;;; lang-config.el ends here
