```org
** DONE Terminal
Native terminal emulation via =ghostel= and the =libghostty-vt= engine. Spawns as a persistent 30% bottom drawer via =display-buffer-alist= with strict Evil, project-awareness, and upstream shell integration.
#+begin_src emacs-lisp
(use-package ghostel
  :defer t
  :commands (ghostel ghostel-project)
  :bind (:map ghostel-semi-char-mode-map
         ;; Eshell muscle-memory parity: simulate M-p/M-n history navigation
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         ("M-<backspace>" . ghostel-backward-kill-word))
  :config
  (require 'seq)
  ;; Force Ghostel into a dedicated bottom side-window occupying 30% of the frame.
  (add-to-list 'display-buffer-alist
               '((derived-mode . ghostel-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Shell integration: allow calling magit from the terminal via OSC 52;e.
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  ;; TODO: Upgrade `project-switch-commands` and `ghostel-project` buffer scoping
  ;; to integrate natively with `projectile` roots and `persp-mode` workspaces
  ;; once the Project Management and Workspaces sections are finalized.
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  ;; Custom C-k behavior: kill to end of line in Emacs and send C-k to terminal.
  (defun ar/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel and save to kill-ring."
    (interactive)
    (copy-region-as-kill (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  (define-key ghostel-semi-char-mode-map (kbd "C-k") #'ar/ghostel-send-C-k-and-kill)
  ;; Strip DIRENV_ variables to force local .envrc evaluation in child shells.
  (add-hook 'ghostel-pre-spawn-hook
            (lambda ()
              (setq process-environment
                    (seq-remove (lambda (env) (string-prefix-p "DIRENV_" env))
                                process-environment))))
  ;; TRAMP Support: Automatically inject shell integration scripts (OSC 7/133)
  ;; into remote sessions via temporary SCP transfers.
  (setq ghostel-tramp-shell-integration t)
  ;; TODO: Add the following to your OS-level `~/.zshrc` for IDE synergy:
  ;; if [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]]; then
  ;;   alias e='ghostel_cmd find-file-other-window'
  ;;   alias dow='ghostel_cmd dired-other-window'
  ;;   alias gst='ghostel_cmd magit-status-setup-buffer'
  ;; fi
  ;; TODO: Add the following to your OS-level `~/.tmux.conf` for true-color passthrough:
  ;; set -g default-terminal "tmux-256color"
  ;; set -ga terminal-overrides ",xterm-ghostty:Tc"
  )

(defun ar/ghostel-toggle ()
  "Toggle the Ghostel terminal drawer at the bottom of the frame."
  (interactive)
  ;; TODO: Upgrade `seq-find` buffer querying to strictly respect `persp-mode`
  ;; workspace boundaries and `projectile` project roots once those systems
  ;; are finalized, preventing cross-workspace terminal bleeding.
  (let* ((buf (seq-find (lambda (b)
                          (with-current-buffer b (derived-mode-p 'ghostel-mode)))
                        (buffer-list)))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((and win (eq win (selected-window))) (delete-window win))
     (win (select-window win))
     (buf (pop-to-buffer buf))
     (t (call-interactively #'ghostel)))))

;; ==========================================
;; EXTENSIONS
;; ==========================================
(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package ghostel-eshell
  :ensure nil
  :after ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-compile
  :ensure nil
  :after ghostel
  :hook (elpaca-after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :ensure nil
  :after ghostel
  :hook (elpaca-after-init . ghostel-comint-global-mode))
#+end_src
```

```org
#+begin_src emacs-lisp
(global-corfu-modes '((not erc-mode
                           eshell-mode
                           circe-mode
                           help-mode
                           gud-mode
                           vterm-mode
                           ghostel-mode
                           ghostel-compile-view-mode)
                      t))
#+end_src
```

```org
#+begin_src emacs-lisp
(use-package hide-mode-line
  :autoload turn-off-hide-mode-line-mode
  :hook (((eat-mode org-agenda-mode
                    eshell-mode shell-mode
                    term-mode vterm-mode
                    ghostel-mode ghostel-compile-view-mode
                    embark-collect-mode lsp-ui-imenu-mode
                    pdf-annot-list-mode) . turn-on-hide-mode-line-mode)))
#+end_src
```

```org
#+begin_src emacs-lisp
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
  (add-to-list 'dabbrev-ignored-buffer-modes mode))
#+end_src
```

```org
#+begin_src emacs-lisp
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode ghostel-mode ghostel-compile-view-mode) .
          (lambda () (setq-local global-hl-line-mode nil))))
#+end_src
```

```elisp
;; Open operations (SPC o)
(ar/global-leader
  "o" '(:ignore t :wk "open")
  "o t" '(ar/ghostel-toggle :wk "toggle ghostel")
  "o T" '(ghostel-project :wk "Ghostel project")
  "o s" '(shell :wk "Shell")
  "o d" '(dired :wk "Dired")
  "o D" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :wk "Dashboard")
  "o P" '(popper-toggle :wk "Toggle popup"))
```

```bash
# Add to your OS-level ~/.zshrc
# Ghostel IDE Integration
if [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]]; then
  # Open a file in Emacs from the terminal (e.g., `e main.py`)
  alias e='ghostel_cmd find-file-other-window'

  # Open Dired in another window at the current directory
  alias dow='ghostel_cmd dired-other-window'

  # Open Magit for the current directory
  alias gst='ghostel_cmd magit-status-setup-buffer'
fi
```

```tmux
# Add to your OS-level ~/.tmux.conf
set -g default-terminal "tmux-256color"
set -ga terminal-overrides ",xterm-ghostty:Tc"
```

### Architectural Notes on Consult Keybindings

The global `general.el` configuration already binds `consult-line` to `SPC s s` and `SPC s b`, alongside other `consult` commands (`SPC s i`, `SPC s o`, etc.). Because these are global leader bindings, they are mathematically guaranteed to work inside `ghostel-mode` **provided the buffer is in Emacs mode (`C-c C-e`) or Copy mode (`C-c C-t`)**.

Attempting to route the `SPC` leader key through `ghostel-semi-char-mode-map` would require adding `SPC` to `ghostel-keymap-exceptions`, which would catastrophically break the ability to type spaces in the terminal. Relying on the read-only modes for `consult` interactions preserves both the terminal's input stream and your global muscle memory.
