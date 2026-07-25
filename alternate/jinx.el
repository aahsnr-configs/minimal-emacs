;;; jinx.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ahsanur Rahman
;;
;; Author: Ahsanur Rahman <ahsanur041@proton.me>
;; Maintainer: Ahsanur Rahman <ahsanur041@proton.me>
;; Created: July 25, 2026
;; Modified: July 25, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/ahsan/jinx
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package jinx
  :init
  (defvar ar/jinx-disabled-modes
    '(prog-mode
      conf-mode
      emacs-lisp-mode
      dired-mode
      ibuffer-mode
      neotree-mode
      treemacs-mode
      magit-status-mode
      magit-log-mode
      magit-diff-mode
      magit-branch-mode
      org-agenda-mode
      org-src-mode
      dashboard-mode
      which-key-mode
      help-mode
      Info-mode
      embark-collect-mode
      ghostel-mode
      vterm-mode
      term-mode
      pdf-view-mode)
    "Modes in which Jinx should not activate.")
  ;; Inline exclusions replacing the dict.txt file.
  (defvar ar/jinx-personal-words
    "emacs elisp use-package org-mode prog-mode conf-mode text-mode
eglot eldoc flymake corfu vertico orderless consult embark
denote treemacs dirvish magit evil avy anzu jinx
tokyo night jetbrainsmono nerd font pgtk wayland
tramp ssh json yaml toml lsp dap ast"
    "Personal vocabulary for Jinx session words. Write entries in lowercase for broadest coverage.")
  :custom
  ;; Deterministic English regardless of system locale.
  (jinx-languages "en")
  :config
  ;; Dynamically construct exclude regexps to hide dangerous backslashes
  ;; and `#+` sequences from Org-mode's LaTeX and keyword parsers.
  (setq jinx-exclude-regexps
        `((emacs-lisp-mode "Package-Requires:.*$")
          (org-mode ,(rx bol "#+" (+ word) (* any) eol)
                    ,(rx "\\" (+ alpha)))
          (LaTeX-mode ,(rx "\\" (+ alpha)))
          (t "[A-Z]+\\>"
             "-+\\>"
             "\\w*?[0-9]\\w*\\>"
             "[a-z]+://\\S-+"
             "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
             "\\(?:Local Variables\\|End\\):\\s-*$"
             "jinx-\\(?:languages\\|local-words\\):\\s-+.*$"
             "[/~][a-zA-Z0-9._/-]+")))
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice jinx-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p))))
  ;; Enforces custom disabled modes (jinx-disabled-modes removed in v2.8).
  (define-advice jinx--on (:before-while () guard-disabled-modes)
    (not (apply #'derived-mode-p ar/jinx-disabled-modes)))
  ;; Injects personal vocabulary into prose and markup buffers.
  (dolist (mode '(org-mode
                  LaTeX-mode
                  markdown-mode
                  gfm-mode
                  text-mode))
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (setq-local jinx-local-words ar/jinx-personal-words))))
  ;; Activates global spell-checking directly per Elpaca load-order physics.
  (global-jinx-mode 1))


(provide 'jinx)
;;; jinx.el ends here
