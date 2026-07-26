;;; dape.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ahsanur Rahman
;;
;; Author: Ahsanur Rahman <ahsanur041@proton.me>
;; Maintainer: Ahsanur Rahman <ahsanur041@proton.me>
;; Created: July 26, 2026
;; Modified: July 26, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/ahsan/dape
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(use-package dape
  :defer t
  :commands (dape
             dape-quit
             dape-restart
             dape-next
             dape-step-in
             dape-step-out
             dape-continue
             dape-pause
             dape-breakpoint-toggle
             dape-breakpoint-remove-all
             dape-breakpoint-log
             dape-breakpoint-expression
             dape-breakpoint-hits
             dape-evaluate-expression
             dape-watch-dwim
             dape-info
             dape-repl
             dape-memory
             dape-disassemble
             dape-select-thread
             dape-select-stack
             dape-until
             dape-restart-frame)
  :custom
  ;; Unbind the default C-x C-a prefix; we use general.el leader bindings.
  (dape-key-prefix nil)
  ;; IDE-style side panels (matches Treemacs-left, info-right paradigm).
  (dape-buffer-window-arrangement 'right)
  ;; Full VS Code-like multi-panel layout on session start.
  (dape-many-windows t)
  ;; Keep info buffers chrome-free (matches ar/hide-modeline-mode aesthetic).
  (dape-info-hide-mode-line t)
  ;; Wire cwd to native project.el (NOT projectile).
  ;; Emacs 31: `when-let` is obsolete; strictly use `when-let*`.
  (dape-cwd-function (lambda ()
                       (or (when-let* ((proj (project-current)))
                             (project-root proj))
                           default-directory)))
  ;; Auto-expand variable trees: 1 level for locals (buffer 0), 0 for globals.
  (dape-variable-auto-expand-alist '((0 . 1)))
  ;; Tabular alignment in the scopes buffer.
  (dape-info-variable-table-aligned t)
  ;; Line-level stepping granularity.
  (dape-stepping-granularity 'line)
  ;; Adapter response timeout in seconds.
  (dape-request-timeout 5)
  ;; Configuration hints in minibuffer.
  (dape-minibuffer-hint t)
  ;; Echo runInTerminal output to REPL.
  (dape-repl-echo-shell-output t)
  ;; Dash-form config parsing.
  (dape-history-add 'shell-like)
  ;; Route breakpoint persistence to no-littering var directory.
  (dape-default-breakpoints-file (no-littering-expand-var-file-name "dape/breakpoints.eld"))
  ;; Render inlay hints for 10 lines surrounding the stopped stack frame.
  (dape-inlay-hints 10)
  :custom-face
  ;; Themes inline exception overlays to Tokyo Night red.
  (dape-exception-description-face ((t (:foreground "#f7768e" :inherit error))))
  :config
  ;; Breakpoint persistence: save on quit, load on startup.
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'elpaca-after-init-hook #'dape-breakpoint-load)

  ;; Enable mouse-based breakpoint manipulation (click fringe to toggle).
  (dape-breakpoint-global-mode 1)

  ;; Visual pulse on stop & dynamic inlay-hints guard.
  ;; Buffer-locally overrides `dape-inlay-hints` to prevent main-thread blocking
  ;; when the debugger halts in massive minified JSON or log files.
  (add-hook 'dape-display-source-hook
            (lambda ()
              (if (too-long-file-p)
                  (setq-local dape-inlay-hints nil)
                (setq-local dape-inlay-hints 10))
              (unless (too-long-file-p)
                (pulse-momentary-highlight-one-line))))

  ;; Save buffers before debug session (useful for interpreted languages).
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on success.
  (add-hook 'dape-compile-hook #'kill-buffer)

  ;; Hide modeline in REPL buffer.
  (add-hook 'dape-repl-mode-hook #'ar/enable-hide-modeline)

  ;; Spatial Routing (Ghostel/Eshell drawer paradigm)
  (add-to-list 'display-buffer-alist
               '((derived-mode . dape-repl-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.30)
                 (window-parameters (no-delete-other-windows . t))))
  (add-to-list 'display-buffer-alist
               '((derived-mode . dape-memory-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.40)
                 (window-parameters (no-delete-other-windows . t))))
  (add-to-list 'display-buffer-alist
               '("\\*dape-disassemble.*\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.35)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Route standard compilation buffers (spawned by dape-compile-function) to bottom drawer.
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t))))

  ;; Transient Dashboard (Replacing dap-hydra)
  (with-eval-after-load 'transient
    (defun ar/dape-title ()
      (concat
       (if (fboundp 'nerd-icons-codicon)
           (nerd-icons-codicon "nf-cod-debug" :face 'transient-heading :v-adjust 0.02)
         "")
       (propertize " Debug" 'face 'transient-heading)))

    (transient-define-prefix ar/toggles-dape ()
      "Debugger"
      [:description ar/dape-title
		    ["Session"
		     ("S" "start" dape :transient t)
		     ("P" "project" projection-dape :transient t)
		     ("q" "quit" dape-quit)
		     ("k" "kill" dape-kill)
		     ("r" "restart" dape-restart :transient t)
		     ("R" "restart frame" dape-restart-frame :transient t)]
		    ["Navigation"
		     ("n" "next" dape-next :transient t)
		     ("s" "step in" dape-step-in :transient t)
		     ("o" "step out" dape-step-out :transient t)
		     ("c" "continue" dape-continue :transient t)
		     ("p" "pause" dape-pause :transient t)
		     ("u" "until" dape-until :transient t)]
		    ["Breakpoints"
		     ("b" "toggle" dape-breakpoint-toggle :transient t)
		     ("B" "clear all" dape-breakpoint-remove-all :transient t)
		     ("l" "log" dape-breakpoint-log :transient t)
		     ("h" "hits" dape-breakpoint-hits :transient t)
		     ("C" "condition" dape-breakpoint-expression :transient t)]
		    ["Evaluation"
		     ("e" "eval" dape-evaluate-expression :transient t)
		     ("w" "watch" dape-watch-dwim :transient t)]
		    ["Info Panels"
		     ("i" "info" dape-info :transient t)
		     ("t" "threads" dape-select-thread :transient t)
		     ("F" "stack" dape-select-stack :transient t)
		     ("m" "repl" dape-repl :transient t)
		     ("M" "memory" dape-memory :transient t)
		     ("A" "disasm" dape-disassemble :transient t)]
		    ["Exit"
		     ("<escape>" "" transient-quit-one :format " ")]])))


(provide 'dape)
;;; dape.el ends here
