---

[Prompt]: prevent the native compile of dape and org-gtd-mode everytime I reload my emacs daemon.

Based on my analysis of your config and the web search results, the issue is that **`dape` and `org-gtd-mode` are being loaded at daemon startup**, not actually being recompiled. The "Loading... (native compiled elisp)" message is just Emacs loading the already-compiled `.eln` files.

The root cause is the **`:hook (after-init . dape-breakpoint-load)`** in your dape configuration, which forces dape to load immediately when Emacs starts.

Here are the required changes:

## Fix 1: Dape Configuration

Replace your dape configuration's `:hook` section:

```elisp
(use-package dape
  :defer t
  :bind ("<f5>" . ar/toggles-dape)
  :commands (dape-breakpoint-save dape-breakpoint-load)
  :init
  ;; Load breakpoints manually when first starting dape
  (with-eval-after-load 'dape
    (dape-breakpoint-load))

  ;; Save on exit - this doesn't load the package
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (featurep 'dape)
                (dape-breakpoint-save))))

  :custom
  (dape-breakpoint-global-mode +1)
  (dape-buffer-window-arrangement 'right)

  :config
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  (add-hook 'dape-compile-hook #'kill-buffer)

  ;; ... rest of your transient config
  )
```

## Fix 2: Org-GTD Configuration

Your org-gtd config looks fine, but make sure the `run-with-idle-timer` isn't causing premature loading. Replace the `:config` section:

```elisp
(use-package org-gtd
  :defer t
  :after (org org-edna)
  :commands (org-gtd-capture
             org-gtd-process-inbox
             org-gtd-engage
             org-gtd-show-all-next
             org-gtd-show-stuck-projects
             org-gtd-organize)

  :init
  (setq org-gtd-directory my/org-gtd-directory)
  (my/ensure-org-dir "gtd")

  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
     (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)")
     (sequence "INBOX(i)" "GTD-NEXT(N)" "GTD-WAIT(W@/!)" "|" "GTD-DONE(D!)" "GTD-CNCL(C@)")))

  (org-gtd-keyword-mapping
   '((todo . "INBOX")
     (next . "GTD-NEXT")
     (wait . "GTD-WAIT")
     (done . "GTD-DONE")
     (canceled . "GTD-CNCL")))

  (org-gtd-mode nil)

  :config
  (setq org-agenda-files
        (list my/org-directory
              my/org-gtd-directory))

  ;; Defer mode activation until actually needed
  ;; Remove this if you don't need org-gtd-mode active globally
  (run-with-idle-timer 30 nil
    (lambda ()
      (when (and (featurep 'org-gtd)
                 (not (bound-and-true-p org-gtd-mode)))
        (org-gtd-mode 1))))

  (setq org-todo-keyword-faces
        (append org-todo-keyword-faces
                '(("INBOX"     . (:foreground "#e0af68" :weight bold))
                  ("GTD-NEXT"  . (:foreground "#7aa2f7" :weight bold))
                  ("GTD-WAIT"  . (:foreground "#ff9e64" :weight bold))
                  ("GTD-DONE"  . (:foreground "#9ece6a" :weight bold))
                  ("GTD-CNCL"  . (:foreground "#565f89" :weight bold))))))
```

## Key Changes Explained:

1. **Removed `after-init` hook for dape-breakpoint-load** - This was forcing dape to load immediately
2. **Used `with-eval-after-load`** - Breakpoints load only when dape is actually used
3. **Protected `kill-emacs-hook`** - Only saves if dape is loaded
4. **Added `:commands`** - Ensures autoloading works correctly
5. **Increased idle timer** - Changed from 5 to 30 seconds for org-gtd-mode activation

These changes will prevent both packages from loading at daemon startup while maintaining all functionality.
