(use-package dape
  :defer t
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :custom
  (dape-breakpoint-global-mode +1)
  (dape-buffer-window-arrangement 'right)

  :config

  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  (add-hook 'dape-repl-mode-hook #'hide-mode-line-mode)
  (add-hook 'dape-compile-hook #'kill-buffer)

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
                    ["Stepping"
                     ("n" "next" dape-next :transient t)
                     ("s" "step in" dape-step-in :transient t)
                     ("o" "step out" dape-step-out :transient t)
                     ("c" "continue" dape-continue :transient t)
                     ("p" "pause" dape-pause :transient t)
                     ("r" "restart" dape-restart :transient t)]

                    ["Switch"
                     ("m" "memory" dape-memory :transient t)
                     ("t" "thread" dape-select-thread :transient t)
                     ("w" "watch" dape-watch-dwim :transient t)
                     ("s" "stack" dape-select-stack :transient t)
                     ("i" "info" dape-info :transient t)
                     ("r" "repl" dape-repl :transient t)]

                    ["Breakpoints"
                     ("b" "toggle" dape-breakpoint-toggle :transient t)
                     ("l" "log" dape-breakpoint-log :transient t)
                     ("e" "expression" dape-breakpoint-expression :transient t)
                     ("B" "clear" dape-breakpoint-remove-all :transient t)]

                    ["Debug"
                     ("d" "dape" dape :transient t)
                     ("D" "disconnect" dape-disconnect-quit)
                     ("k" "kill" dape-kill)
                     ("Q" "quit" dape-quit)
                     ("<escape>" "" transient-quit-one :format " ")]]))
  )
