### 1. Core Emacs: Small Configs (Tabulated List & GPG Pinentry)

**The Architectural "Why":**

1.  **The `tabulated-list-mode` "q" Quit Fix:** Native Emacs uses `tabulated-list-mode` as the parent mode for dozens of read-only dashboard buffers (`*Packages*`, `*Flycheck errors*`, `*Buffer List*`, `*Occur*`). By default, pressing `q` in these buffers does nothing or triggers a `self-insert-command` error. This single `define-key` globally standardizes `q` to gracefully bury the window, matching Vim/Emacs muscle memory and preventing you from having to reach for `C-x k` or `q` in custom Transient menus.
2.  **GPG Pinentry Loopback:** If you use GPG to encrypt files (`authinfo.gpg`, `.gpg` notes), Emacs natively spawns an external GTK/Qt OS dialog box to ask for your passphrase. On a PGTK/Wayland build, this external dialog frequently steals focus, freezes the Emacs daemon, and breaks terminal-only `emacsclient` workflows. Setting `epg-pinentry-mode` to `'loopback` forces Emacs to route the GPG passphrase prompt directly into the native Emacs minibuffer, keeping the security workflow entirely contained within the editor.

```org
** DONE Small Configs
Quality-of-life settings, safety guards, and core minor modes. Disables unused VC backends to prevent TRAMP I/O stutter, suppresses native OS dialog boxes to prevent daemon freezes, and protects the system clipboard from accidental overwrite during kill operations.
#+begin_src emacs-lisp
;; Restrict VCS backends to Git. Prevents TRAMP I/O stutter and SSH timeouts
;; caused by Emacs probing for bzr, svn, and hg on remote files.
(setq vc-handled-backends '(Git)
      vc-follow-symlinks t)

;; Suppress native OS dialog boxes. GTK/X11 dialogs freeze the Emacs daemon
;; when triggered from an emacsclient terminal session.
(setq use-dialog-box nil
      use-file-dialog nil)

;; Routes GPG passphrase prompts to the native minibuffer, preventing external
;; GTK/Qt OS dialog freezes and preserving daemon stability on PGTK/Wayland.
(setq epg-pinentry-mode 'loopback)

;; Process and compilation safeguards.
(setq confirm-kill-processes nil
      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

;; UI and input latency reductions.
(setq echo-keystrokes 0.1
      ad-redefinition-action 'accept)

;; Filesystem and buffer safety.
;; Disables lockfiles to prevent conflicts with modern file watchers (Webpack, Vite).
(setq-default create-lockfiles nil
              require-final-newline t)

;; Enable potentially dangerous commands without prompting.
(put 'erase-buffer 'disabled nil)

;; Core minor modes.
(delete-selection-mode 1)
(setq window-sides-vertical t)

;; Protect system clipboard. Saves external clipboard contents to the kill-ring
;; before Emacs overwrites them via kill/cut operations.
(setq save-interprogram-paste-before-kill t)

;; Standardizes `q` to gracefully bury read-only dashboard buffers (*Packages*,
;; *Flycheck*, *Buffer List*, etc.) to match Vim/Emacs muscle memory.
(with-eval-after-load 'tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

;; Fallback major-mode associations for extensions lacking native Emacs support.
(dolist (pair '(("\\.in\\'" . text-mode)
                ("\\.out\\'" . text-mode)
                ("\\.args\\'" . text-mode)
                ("\\.bb\\'" . shell-script-mode)
                ("\\.bbclass\\'" . shell-script-mode)
                ("\\.Rmd\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist pair))
#+end_src
```

---

### 2. Completion Framework: Vertico (Minibuffer History Fuzzy Search)

**The Architectural "Why":**
Natively, pressing `C-s` inside the minibuffer triggers `isearch-forward`. This is mathematically useless inside a single-line minibuffer prompt because you cannot "search" the prompt text itself. What you actually want is to search your _input history_ (e.g., past `M-x` commands, `find-file` paths, or `consult-ripgrep` queries).

By mapping `C-s` to `#'consult-history` across all active minibuffer keymaps, you can press `C-s` while typing a path or command, instantly summoning a Vertico dropdown to fuzzy-search your entire input history with live-preview filtering. This perfectly replicates Doom Emacs's `counsel-minibuffer-history` workflow but utilizes the modern Consult/Vertico stack.

```org
** TODO Vertico
=VERTical Interactively COmpletion= serves as the minimalist, high-performance UI for the Emacs minibuffer. Transforms =vertico= into a deeply integrated, context-aware navigation engine by leveraging its bundled extensions and native history search routing.
#+begin_src emacs-lisp
(use-package vertico
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  ;; LOAD ORDER GUARANTEE
  (require 'vertico-directory)
  (require 'vertico-quick)
  (require 'vertico-repeat)
  (require 'vertico-multiform)
  (require 'vertico-buffer)
  (require 'vertico-grid)
  (require 'vertico-unobtrusive)
  (vertico-mode)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-<down>" . vertico-next-group)
         ("C-<up>" . vertico-previous-group)
         ("M-q" . vertico-quick-jump)
         ("M-Q" . vertico-quick-exit)
         ("M-i" . vertico-quick-insert))
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save))
  :config
  ;; Core Minibuffer Safeguards
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  ;; Enable Global Extension Modes
  (vertico-multiform-mode 1)

  ;; Multiform Rules (Context-Aware UI)
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-outline buffer)
          (consult-line-multi buffer)
          (consult-yasnippet grid)))

  ;; Visual Transforms
  (defvar +vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory-fn (file)
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'dired-directory 'append file))
    file)

  (defun +vertico-highlight-enabled-mode-fn (cmd)
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and (memq sym minor-mode-list)
                     (boundp sym)
                     (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
    cmd))

  (add-to-list 'vertico-multiform-categories
               '(file (+vertico-transform-functions . +vertico-highlight-directory-fn)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command (+vertico-transform-functions . +vertico-highlight-enabled-mode-fn)))
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))

  ;; Minibuffer History Fuzzy Search: Remaps `C-s` to summon a Vertico dropdown
  ;; for fuzzy-searching input history across all native minibuffer maps.
  (defvar ar/minibuffer-maps
    '(minibuffer-local-map
      minibuffer-local-ns-map
      minibuffer-local-completion-map
      minibuffer-local-must-match-map
      minibuffer-local-isearch-map
      read-expression-map)
    "A list of all keymaps used for the minibuffer.")

  (with-eval-after-load 'consult
    (dolist (keymap ar/minibuffer-maps)
      (define-key (symbol-value keymap) (kbd "C-s") #'consult-history))))
#+end_src
```

---

### 3. Lexical Validation & Comparative Workflows: Woman (System Manpath)

**The Architectural "Why":**
You have `woman` bound to `SPC h w` in your General Keybindings. Emacs' native `woman` package (WOMAN = WithOut MAN) relies on hardcoded, outdated internal paths and frequently fails to find modern system manuals, especially on rolling-release distributions like Arch Linux.

By extracting the paths directly from the OS `manpath` or `man --path` binary at load time, we mathematically guarantee that `woman` inherits the exact same search paths as your system terminal, allowing it to locate and render all installed man pages without requiring the external `man` pager.

```org
** TODO Woman
Extracts system =manpath= directories to guarantee native Emacs =woman= can locate modern Arch Linux manuals without external dependencies.
#+begin_src emacs-lisp
(use-package woman
  :ensure nil
  :defer t
  :commands (woman)
  :config
  ;; Extract system manpath to guarantee `woman` locates modern Arch Linux manuals.
  (let ((path (cond
               ((executable-find "manpath")
                (split-string (string-trim (shell-command-to-string "manpath -q")) path-separator t))
               ((executable-find "man")
                (split-string (string-trim (shell-command-to-string "man --path")) path-separator t)))))
    (when path (setq woman-manpath path))))
#+end_src
```

---

### 4. Spatial Alignment & Whitespace Hygiene: Move Text

**The Architectural "Why":**
The upstream `move-text` package relies on Emacs' native `transpose-lines` C-primitive, which blindly moves text verbatim and completely ignores the buffer's indentation-mode rules. In whitespace-sensitive languages (Python, YAML, Makefiles), dragging a line out of its indentation block instantly breaks the syntax.

The `ar/move-text-indent-region-advice` wraps `indent-region` in a `let` block that preserves `deactivate-mark`. This prevents the visual selection from collapsing during continuous line dragging, while synergizing perfectly with your global `electric-indent-mode` and `dtrt-indent` heuristic engine to snap the displaced line to the correct indentation level.

```org
** DONE Move Text
Provides O(1) line and region dragging. Automatically re-indents displaced lines to preserve structural integrity in whitespace-sensitive languages.
#+begin_src emacs-lisp
(use-package move-text
  :defer t
  :commands (move-text-up move-text-down)
  :config
  ;; Re-indent the displaced line or region to preserve structural integrity.
  (defun ar/move-text-indent-region-advice (&rest ignored)
    "Re-indent the current line or active region after moving text."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add #'move-text-up :after #'ar/move-text-indent-region-advice)
  (advice-add #'move-text-down :after #'ar/move-text-indent-region-advice))
#+end_src
```

---

### 5. Structural Typing and Parsing: Expreg

**The Architectural "Why":**
This bridges legacy Doom Emacs muscle memory (`M-=` for `expand-region`) while enforcing the O(1) AST-aware `expreg` engine over the rejected regex-based `expand-region`. Injecting `M-=` into `normal`, `visual`, and `motion` states safely shadows the native `count-words-region` binding without destroying it.

```org
** DONE Expreg
Provides O(1) AST-aware visual selection expansion via the native =treesit= engine. Integrates with =repeat-mode= for modifier-less tapping and preserves cursor origin on contraction.
#+begin_src emacs-lisp
(use-package expreg
  :if (treesit-available-p)
  :defer t
  :commands (expreg-expand expreg-contract)
  :config
  ;; Restores exact cursor origin upon `C-g` (keyboard-quit) to prevent spatial drift.
  (setq expreg-restore-point-on-quit t)
  ;; Emacs 29+ `repeat-mode` integration for modifier-less expansion loops.
  (defvar-keymap expreg-repeat-map
    :doc "Keymap for repeating expreg commands."
    "+" #'expreg-expand
    "=" #'expreg-expand
    "-" #'expreg-contract
    "_" #'expreg-contract)
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map)
  ;; Visual State Routing: Hijacks `v` for seamless AST expansion loops.
  (general-define-key
   :states 'visual
   "v" #'expreg-expand)
  ;; Global Fallback: Maps `M-=` (Doom's expand-region mnemonic) and modifier combinations.
  (general-define-key
   :states '(normal visual motion)
   "M-=" #'expreg-expand
   "C-M-+" #'expreg-expand
   "C-M--" #'expreg-contract))
#+end_src
```

---

### 6. Precision Editing: Avy (The Finalized Engine)

**The Architectural "Why":**
This block contains the fully synthesized Avy engine. It includes the `at-full` visual paradigm, the home-row routing, the proximity sorting (`avy-orders-alist`), the tightened input latency (`avy-timeout-seconds`), the Doom extractions (`avy-all-windows-alt`, `avy-single-candidate-jump`), and the expanded `:commands` list to prevent the Transient Menu `void-function` trap. Finally, it injects `M-j` strictly into `isearch-mode-map` to fulfill the "Seek, then Jump" blueprint mandate.

```org
** DONE Avy
Provides an O(1) radix-tree jumping engine for long-distance spatial navigation. Configured with the =at-full= overlay style, home-row routing, proximity sorting, and on-demand multi-window expansion.
#+begin_src emacs-lisp
(use-package avy
  :defer t
  :commands (avy-goto-char-2 avy-goto-line avy-goto-word-1 avy-goto-word-0 avy-isearch
             avy-move-line avy-copy-line avy-kill-region avy-transpose-lines-in-region
             avy-move-region avy-copy-region avy-kill-ring-save-region)
  :bind (:map isearch-mode-map
         ("M-j" . avy-isearch))
  :custom
  ;; Path Generation: Standard shortest-path algorithm restricted to home-row.
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Visual Rendering: `at-full` preserves peripheral word context.
  (avy-style 'at-full)
  ;; Buffer Dimming: Forces focus strictly to the opaque neon spotlights.
  (avy-background t)
  ;; Scope Control: Defaults to active window; `C-u` expands search to all windows.
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  ;; Candidate Validation: Renders overlay for single targets to prevent accidental jumps.
  (avy-single-candidate-jump nil)
  ;; Proximity Engine: Sorts candidates by physical distance to cursor.
  (avy-orders-alist '((avy-goto-char . avy-order-closest)
                      (avy-goto-word-0 . avy-order-closest)
                      (avy-goto-word-1 . avy-order-closest)))
  ;; Input Latency: Tightens the seek-then-jump response time.
  (avy-timeout-seconds 0.3)
  :custom-face
  ;; Opaque Spotlight Faces: 100% opaque backgrounds prevent ghosting artifacts.
  (avy-lead-face-0 ((t (:background "#f7768e" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face-1 ((t (:background "#e0af68" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face-2 ((t (:background "#9ece6a" :foreground "#1a1b26" :weight bold))))
  (avy-lead-face ((t (:background "#7dcfff" :foreground "#1a1b26" :weight bold)))))
#+end_src
```

---

### 7. General Keybindings (Glue Code Replacements)

**The Architectural "Why":**
These three blocks surgically excise the deprecated `avy-zap` and `ace-link` ghost bindings that would otherwise cause fatal `void-function` crashes and shadow `evil-snipe`'s native zapping behavior. It also updates the line dragging bindings to the PGTK-safe `<M-down>`/`<M-up>` arrow keys.

```org
;; 1. Evil Numbers & Multiedit (Removes obsolete avy-zap bindings to restore evil-snipe)
#+begin_src emacs-lisp
(general-define-key
 :states '(normal visual)
 ;; Evil Numbers
 "C-a" #'evil-numbers/inc-at-pt
 "C-x" #'evil-numbers/dec-at-pt
 ;; Evil Multiedit
 "M-d" #'evil-multiedit-match-symbol-and-next
 "M-D" #'evil-multiedit-match-all)
#+end_src
```

```org
;; 2. Popper (Removes obsolete ace-link bindings)
#+begin_src emacs-lisp
(general-define-key
 ;; Popper
 "C-`" 'popper-toggle
 "C-~" 'popper-cycle
 "M-`" 'popper-toggle-type)
#+end_src
```

```org
;; 3. Line Dragging (Updates M-j/M-k to PGTK-safe arrow keys)
#+begin_src emacs-lisp
;; Move text up and down
(general-define-key
 :states '(normal visual insert)
 "<M-down>" 'move-text-down
 "<M-up>" 'move-text-up)
#+end_src
```

---

# 2nd iteration

I have executed the requested structural migrations and injections strictly within the boundaries of Sections 1, 2, 5, and 7. Per your explicit instructions and the **No Scope Creep** negative constraint, I have used `git diff` syntax to map exactly what must be added, moved, or excised.

### 1. Small Configs (Section 1)

Injects the GPG pinentry loopback and the `tabulated-list` "q" quit fix.

```diff
--- a/config.org
+++ b/config.org
@@ -433,6 +433,15 @@
 ;; before Emacs overwrites them via kill/cut operations.
 (setq save-interprogram-paste-before-kill t)

+;; Routes GPG passphrase prompts to the native minibuffer, preventing external
+;; GTK/Qt OS dialog freezes and preserving daemon stability on PGTK/Wayland.
+(setq epg-pinentry-mode 'loopback)
+
+;; Standardizes `q` to gracefully bury read-only dashboard buffers (*Packages*,
+;; *Flycheck*, *Buffer List*, etc.) to match Vim/Emacs muscle memory.
+(with-eval-after-load 'tabulated-list
+  (define-key tabulated-list-mode-map "q" #'quit-window))
+
 ;; Fallback major-mode associations for extensions lacking native Emacs support.
 (dolist (pair '(("\\.in\\'" . text-mode)
```

### 2. Vertico (Section 2)

**No changes required.** A deep scan of your uploaded `config.org.txt` confirms that the Minibuffer History Fuzzy Search (remapping `C-s` to `consult-history` across all native minibuffer maps via `ar/minibuffer-maps`) is **already fully integrated** at the very end of the Vertico `:config` block.

### 5. Expreg (Section 5)

Injects the `M-=` mnemonic into the Expreg global fallback block to bridge Doom Emacs muscle memory.

```diff
--- a/config.org
+++ b/config.org
@@ -1274,8 +1274,10 @@
    :states 'visual
    "v" #'expreg-expand)
-  ;; Global Fallback: Mapped to `C-M-+` and `C-M--` to avoid clobbering text-scale bindings.
+  ;; Global Fallback: Maps `M-=` (Doom's expand-region mnemonic) and modifier combinations.
   (general-define-key
    :states '(normal visual motion)
+   "M-=" #'expreg-expand
    "C-M-+" #'expreg-expand
    "C-M--" #'expreg-contract))
 #+end_src
```

### 7. General Keybindings & Move Text (Section 7)

This diff accomplishes three things:

1. **Moves** the line-dragging bindings into the `move-text` subsection (updated to PGTK-safe `<M-down>`/`<M-up>` arrows).
2. **Excises** the obsolete `avy-zap` (`z`/`Z`) bindings, restoring `evil-snipe`'s native C-level zapping.
3. **Excises** the `ace-link` (`M-o`) ghost binding, preventing a `void-function` crash.

#### Destination: Move Text Subsection

```diff
--- a/config.org
+++ b/config.org
@@ -1353,6 +1353,12 @@
         (indent-region (line-beginning-position) (line-end-position)))
       (setq deactivate-mark deactivate)))
   (advice-add #'move-text-up :after #'ar/move-text-indent-region-advice)
-  (advice-add #'move-text-down :after #'ar/move-text-indent-region-advice))
+  (advice-add #'move-text-down :after #'ar/move-text-indent-region-advice)
+
+  ;; PGTK-safe line dragging. Replaces legacy M-j/M-k bindings to preserve
+  ;; home-row availability and prevent terminal escape-sequence parsing bugs.
+  (general-define-key
+   :states '(normal visual insert)
+   "<M-down>" #'move-text-down
+   "<M-up>" #'move-text-up))
 #+end_src
```

#### Source: General Keybindings Section (Excisions)

```diff
--- a/config.org
+++ b/config.org
@@ -2205,10 +2205,6 @@
 (general-define-key
  :states '(normal visual)
- ;; Avy zap
- "z" 'avy-zap-to-char-dwim
- "Z" 'avy-zap-up-to-char-dwim
  ;; Evil Numbers
  "C-a" #'evil-numbers/inc-at-pt
  "C-x" #'evil-numbers/dec-at-pt
@@ -2220,7 +2216,6 @@

 (general-define-key
- ;; Ace-link
- "M-o" 'ace-link-addr
  ;; Popper
  "C-`" 'popper-toggle
  "C-~" 'popper-cycle
@@ -2253,12 +2248,4 @@
  "q" 'quit-window)

-;; Move text up and down
-(general-define-key
- :states '(normal visual insert)
- "M-j" 'move-text-down
- "M-k" 'move-text-up)
-
 (ar/local-leader
```

---

**Architectural Note on Scope Parity:**
I noticed that the **Avy (Section 6)** block in your newly uploaded `config.org.txt` is the older version (it lacks the `avy-isearch` `M-j` binding, the proximity engine, and the expanded `:commands` list we finalized in the previous session). However, because your prompt strictly commanded me to _only_ write changes for Sections 1, 2, 5, and 7, I have mathematically obeyed the **No Scope Creep** negative constraint and left the Avy block entirely untouched. Awaiting your explicit signal if you wish to apply the finalized Avy rewrite.
