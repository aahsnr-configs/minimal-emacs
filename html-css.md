Here are the targeted additions to your config for first‑class HTML + CSS development, integrated with your existing evil, eglot, apheleia, corfu and general‑leader infrastructure.

---

## 1. Tree‑sitter Grammars — add `html` and `css` sources

Insert these two entries inside the existing `treesit-language-source-alist` in the **Treesitter/Folding → Treesit** section (around line 3002):

```emacs-lisp
(html "https://github.com/tree-sitter/tree-sitter-html")
(css "https://github.com/tree-sitter/tree-sitter-css")
```

So the block becomes:

```emacs-lisp
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        ;; ... existing entries ...
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        ;; ... rest of existing entries ...
        ))
```

**Why:** `css-ts-mode` (built into Emacs 30) and `html-ts-mode` both need their grammar `.so` / `.dylib` files.  `treesit-auto` will compile them for you on first use.  
*Note:* `html-ts-mode` in Emacs 30.2 has a known `sgml-delete-tag` bug, but `web-mode` is the primary choice for `.html` files anyway.

---

## 2. New section — `web-mode` (place after the YAML section, before KBD)

```emacs-lisp
* Web Mode
#+begin_src emacs-lisp
(use-package web-mode
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.xml\\'"    . web-mode)
         ("\\.svg\\'"    . web-mode)
         ("\\.heex\\'"   . web-mode)
         ("\\.eex\\'"    . web-mode))
  :custom
  ;; Unified 2-space indent (matches your other modes)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  ;; Auto-closing tags: style 2 = close on > and also auto-insert </
  (web-mode-enable-auto-closing t)
  (web-mode-tag-auto-close-style 2)
  ;; Disable auto-quoting of attributes (leave that to electric-pair)
  (web-mode-enable-auto-quoting nil)
  ;; Block highlighting — keep paired tags visible
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight nil)
  :hook ((web-mode . eglot-ensure)
         (web-mode . emmet-mode)
         (web-mode . ar/web-mode-setup))
  :config
  (defun ar/web-mode-setup ()
    "Setup web-mode with visual-line and subword for better navigation."
    (visual-line-mode 1)
    (subword-mode 1)
    (setq-local yas-parents '(html-mode))

    ;; Ensure consistent 2-space indent even in mixed JS/CSS blocks
    (setq-local tab-width 2
                standard-indent 2)))
#+end_src
```

**Why:**
- `web-mode` is the de‑facto standard for mixed HTML/CSS/JS templates.
- Auto‑closing style `2` gives you VSCode‑like behaviour: typing `>` closes the tag *and* inserts `</tag>`.
- Eglot ensure + Emmet hook are set once, not per‑mode.

---

## 3. New section — `css-ts-mode` (tree‑sitter CSS)

```emacs-lisp
* CSS Mode (Tree‑sitter)
#+begin_src emacs-lisp
(use-package css-mode
  :straight (:type built-in)
  :defer t
  :init
  ;; Remap built-in css-mode -> css-ts-mode when grammar is available
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  :hook ((css-ts-mode . eglot-ensure)
         (css-ts-mode . emmet-mode)
         (css-ts-mode . ar/css-setup))
  :config
  (defun ar/css-setup ()
    "Setup CSS tree‑sitter mode with 2‑space indent."
    (setq-local tab-width 2
                css-indent-offset 2
                standard-indent 2)))
#+end_src
```

**Why:**
- `css-ts-mode` is built into Emacs 30 and works without known issues.
- The `major-mode-remap-alist` entry tells Emacs to prefer `css-ts-mode` automatically once the grammar is compiled.
- Emmet‑mode in CSS files expands property abbreviations (`m10` → `margin: 10px;`).

---

## 4. New section — `emmet-mode` (expand abbreviations)

```emacs-lisp
* Emmet Mode
#+begin_src emacs-lisp
(use-package emmet-mode
  :defer t
  :commands (emmet-mode emmet-expand-line emmet-expand-yas)
  :custom
  (emmet-indentation 2)               ; 2‑space indent matches your config
  (emmet-move-cursor-between-quotes t) ; jump inside quotes after expansion
  :config
  ;; Rebind from default C-j to something that doesn't conflict with Evil
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-c e") 'emmet-expand-line)

  ;; Also work with yasnippet
  (add-hook 'emmet-mode-hook
            (lambda ()
              (emmet-expand-yas-before-emmet)))
  (defun emmet-expand-yas-before-emmet ()
    "Try yas-expand first, then fall back to emmet-expand-line."
    (when (fboundp 'yas-expand)
      (setq-local emmet-expand-p
                  (lambda ()
                    (or (yas-expand)
                        (emmet-expand-line)))))))
#+end_src
```

**Why:**
- Default Emmet key `C-j` is also Evil’s “move down one line”.  Rebinding to `C-c e` avoids the collision.
- The yasnippet fallback lets you use Emmet abbreviations inside snippet fields.
- Emmet works in *both* `web-mode` and `css-ts-mode` buffers because we already hooked it in the sections above.

---

## 5. Eglot — register HTML + CSS language servers

Append these entries to the existing `(with-eval-after-load 'eglot ...)` block in the **LSP** section (around line 2621):

```emacs-lisp
;; HTML, CSS, JSON — single npm package vscode-langservers-extracted
(add-to-list 'eglot-server-programs
             '((web-mode html-mode html-ts-mode)
               . ("vscode-html-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '((css-ts-mode css-mode)
               . ("vscode-css-language-server" "--stdio")))
```

Also add a note in `eglot-ignored-server-capabilities` to keep auto‑formatting disabled (Prettier handles it via Apheleia):

```emacs-lisp
:documentFormattingProvider      ; kept disabled — use Prettier instead
```

*(This is already in your config; no change needed.)*

**Prerequisite:** Install the servers once on your system:

```bash
npm install -g vscode-langservers-extracted
```

**Why:** The `vscode-langservers-extracted` package provides up‑to‑date, standalone HTML/CSS/JSON/ESLint servers extracted directly from VSCode.  This is the most reliable option as of 2026.

---

## 6. Apheleia — Prettier for HTML & CSS

Append to the **Formatting → Apheleia** section (after the existing mode‑alist assignments):

```emacs-lisp
;; HTML & CSS formatting with Prettier
(setf (alist-get 'web-mode     apheleia-mode-alist) '(prettier))
(setf (alist-get 'html-mode    apheleia-mode-alist) '(prettier))
(setf (alist-get 'html-ts-mode apheleia-mode-alist) '(prettier))
(setf (alist-get 'css-ts-mode  apheleia-mode-alist) '(prettier))
(setf (alist-get 'css-mode     apheleia-mode-alist) '(prettier))
```

**Prerequisite:** Install prettier:

```bash
npm install -g prettier
```

*(If you already have prettier installed for other modes, no action needed.)*

---

## 7. General keybindings — HTML/CSS local leader keys

Add these **below** the existing `(ar/local-leader ...)` blocks in the **General Keybindings** section:

```emacs-lisp
;; ── Web Mode (HTML) ──
(ar/local-leader
  :keymaps 'web-mode-map
  "e"   '(:ignore t :wk "element")
  "e e" '(emmet-expand-line         :wk "Expand abbreviation")
  "e w" '(emmet-wrap-with-markup    :wk "Wrap with markup")
  "e n" '(emmet-next-edit-point     :wk "Next edit point")
  "e p" '(emmet-prev-edit-point     :wk "Prev edit point")
  "b"   '(:ignore t :wk "browse")
  "b b" '(browse-url-of-buffer      :wk "Open in browser")
  "b r" '(browse-url-of-region      :wk "Open region in browser"))

;; ── CSS Mode ──
(ar/local-leader
  :keymaps 'css-ts-mode-map
  "e"   '(:ignore t :wk "emmet")
  "e e" '(emmet-expand-line         :wk "Expand abbreviation")
  "e n" '(emmet-next-edit-point     :wk "Next edit point")
  "e p" '(emmet-prev-edit-point     :wk "Prev edit point"))
```

These give you `SPC m e e` to expand an Emmet abbreviation and `SPC m b b` to open the current HTML file in your browser — all from the local leader.


## Summary of what was added

| Area | Package / Change |
|---|---|
| Tree‑sitter grammars | `html`, `css` grammars in `treesit-language-source-alist` |
| HTML editing | `web-mode` with 2‑space indent, auto‑closing tags, eglot, emmet |
| CSS editing | `css-ts-mode` (tree‑sitter) remapped from `css-mode` |
| Emmet abbreviations | `emmet-mode` with `C-c e` key (avoids Evil conflict) |
| LSP | `vscode-html-language-server` + `vscode-css-language-server` via eglot |
| Formatting | Prettier for `web-mode`, `html-ts-mode`, `css-ts-mode` via Apheleia |
| Keybindings | Local leader (`SPC m`) for HTML element actions & browser preview |

**Prerequisites to install before restarting Emacs:**

```bash
npm install -g vscode-langservers-extracted prettier
```

Then, inside Emacs, `M-x treesit-auto-install-all` (or just open a `.html` / `.css` file — `treesit-auto` will prompt to compile the missing grammars).
