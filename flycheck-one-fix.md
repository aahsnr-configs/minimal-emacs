```org
** TODO Code Diagnostics
Highlights syntax errors and warnings inline via save-triggered diagnostics and Error Lens, upgrading buffer navigation with live minibuffer previews and severity narrowing.
#+begin_src emacs-lisp
(use-package flycheck
  :defer t
  :hook (((prog-mode yaml-mode markdown-mode) . flycheck-mode)
         ((prog-mode yaml-mode markdown-mode) . flycheck-annotate-mode))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Routes diagnostic indicators to the right fringe for a modern IDE gutter.
  (flycheck-indication-mode 'left-fringe)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-help-echo-function nil)
  ;; Events triggering automatic syntax checks. Order matches upstream `defcustom`:
  ;; `save` checks immediately after the buffer is saved; `mode-enabled` checks
  ;; immediately when `flycheck-mode` is non-nil. `idle-change` and `new-line`
  ;; are excluded to prevent redisplay storms during active typing.
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-display-errors-delay 0.25)
  (flycheck-buffer-switch-check-intermediate-buffers nil)
  (flycheck-standard-error-navigation nil)
  (flycheck-checker-error-threshold 500)
  (flycheck-annotate-other-lines-style nil)
  ;; TRAMP/remote syntax checking disabled per user directive.
  ;; (flycheck-check-syntax-automatically-remote '(save mode-enabled))
  :config
  ;; Disables org-lint to prevent false positives in Denote/Org silos.
  (setq-default flycheck-disabled-checkers '(org-lint))
  ;; Hardened Emacs-Lisp predicate: lints project.el roots AND standalone personal configs.
  (eval '(setf (flycheck-checker-get 'emacs-lisp 'predicate)
               (lambda ()
                 (and (not (bound-and-true-p no-byte-compile))
                      (or (project-current)
                          (and (buffer-file-name)
                               (file-in-directory-p (buffer-file-name) user-emacs-directory)))))) t)
  ;; Main-Thread Protection: aborts activation in massive buffers (>500KB / >10k lines)
  ;; to prevent redisplay stutter and main-thread freezing.
  (define-advice flycheck-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p))))
  ;; Prevents asynchronous LSP diagnostic payloads from triggering heavy overlay
  ;; rendering (Error Lens) mid-keystroke, eliminating "chasing cursor" redisplay jitter.
  (define-advice flycheck-buffer (:around (fn &rest args) guard-typing-burst)
    (unless (and (input-pending-p)
                 (memq this-command '(self-insert-command
                                      newline
                                      evil-ret
                                      delete-char
                                      backward-delete-char
                                      yank)))
      (apply fn args)))
  ;; Prevents the *Flycheck errors* buffer from stealing input focus when popped.
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck error messages\\*\\|\\*Flycheck errors\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t)))))
#+end_src
```
