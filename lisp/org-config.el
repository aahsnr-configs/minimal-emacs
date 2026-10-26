;;; org-config.el --- Org mode (Doom defaults) and the Denote second brain -*- lexical-binding: t; -*-

;;; Commentary:

;; Org configuration kept at Doom-default shape: sane display settings,
;; babel lazy-loading, org-modern prettification -- plus the whole Denote
;; ecosystem (silos, sequences, journaling, exploration) which owns note
;; taking in this config (org-roam deliberately not installed).

;;; Code:

(declare-function denote-directories "denote")
(declare-function evil-org-agenda-set-keys "evil-org-agenda")
(declare-function ar/denote-insert-atomic-note-template "org-config")
(declare-function ar/var-file "early-init")
(declare-function ar/icons-displayable-p "core-config")
(defvar org-babel-default-header-args:jupyter-python)

(require 'cl-lib)
(require 'subr-x)

(defun ar/org--icon (fn icon &rest props)
  "Return the nerd-icons glyph for ICON, or the empty string.
FN is one of the `nerd-icons-*' glyph functions.  Icons are
display-only: in batch runs, on terminals, or before nerd-icons is
loaded, the empty string keeps the surrounding group names well-formed."
  (if (and (fboundp fn) (display-graphic-p) (ar/icons-displayable-p))
      (apply fn icon props)
    ""))

;; Route Org's cache directories into the var tree BEFORE the real library
;; loads: org-persist reads these during its own load.  Values set before
;; the defcustom survives it (verified on Emacs 31.1).
(setopt org-persist-directory (ar/var-file "org/persist/")
        org-preview-latex-image-directory (ar/var-file "org/ltximg/")
        org-publish-timestamp-directory (ar/var-file "org/timestamps/")
        org-id-locations-file (ar/var-file "org/id-locations"))

;; org-persist's defcustom re-decides the directory at load (XDG cache
;; default with a temp-dir fallback), so re-route once the library is up;
;; every org-persist file path resolves dynamically from the variable.
(with-eval-after-load 'org-persist
  (setq org-persist-directory (ar/var-file "org/persist/")))

;; Org stays OUT of startup: it loads with the first Org buffer, and each
;; `:after org' block (org-habit, ob-async, evil-org) fires then.

;; Base directory for all Org and Denote files, respecting XDG.

(defvar ar/org-directory
  (let ((xdg-docs (getenv "XDG_DOCUMENTS_DIR")))
    (if (and xdg-docs (file-directory-p xdg-docs))
        (expand-file-name "org/" xdg-docs)
      (expand-file-name "~/org/")))
  "Base directory for all Org and Denote files, respecting XDG specifications.")

(unless (file-directory-p ar/org-directory)
  (make-directory ar/org-directory t))

(setq ar/org-directory (file-truename ar/org-directory))

(defun ar/ensure-org-dir (subdir)
  "Ensure SUBDIR exists under `ar/org-directory', creating parents if needed."
  (let ((dir (expand-file-name subdir ar/org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun ar/ensure-org-file (filepath &optional template)
  "Ensure FILEPATH exists, seeding it with TEMPLATE if newly created."
  (let ((full-path (expand-file-name filepath ar/org-directory)))
    (unless (file-exists-p full-path)
      (with-temp-file full-path
        (when template (insert template))))
    full-path))

(defun ar/ensure-denote-silo (silo-dir)
  "Isolate SILO-DIR via a native `.dir-locals.el' file."
  (setq silo-dir (file-name-as-directory silo-dir))
  (let ((dir-locals (expand-file-name ".dir-locals.el" silo-dir)))
    (unless (file-exists-p dir-locals)
      (with-temp-file dir-locals
        (insert ";;; Directory Local Variables.  For more information evaluate:\n")
        (insert ";;;     (info \"(emacs) Directory Variables\")\n")
        ;; Denote 4.x parity: 5th %s placeholder for the SIGNATURE component.
        (let ((front-matter (concat "#+title: %s\n"
                                    "#+date: %s\n"
                                    "#+filetags: %s\n"
                                    "#+identifier: %s\n"
                                    "#+signature: %s\n"
                                    "#+bind: denote-directory \""
                                    silo-dir "\"\n")))
          (pp `((nil . ((denote-directory . ,silo-dir)
                        (denote-org-front-matter . ,front-matter))))
              (current-buffer)))))
    silo-dir))

(defun ar/create-denote-project-silo (project-name)
  "Create a new isolated Denote silo for PROJECT-NAME under `projects/'."
  (interactive "sProject Name: ")
  (unless (and (stringp project-name)
               (not (string-empty-p project-name))
               (not (file-name-absolute-p project-name))
               (not (member project-name '("." "..")))
               (equal project-name (file-name-nondirectory project-name)))
    (user-error "Project name must be a single path component"))
  (let ((silo-dir (ar/ensure-org-dir (concat "projects/" project-name))))
    (ar/ensure-denote-silo silo-dir)
    (message "Created Denote Silo: %s" silo-dir)
    silo-dir))

(defvar ar/denote-zettelkasten-dir (ar/ensure-org-dir "zettelkasten/")
  "Global, evergreen concepts.  Serves as the fallback `denote-directory'.")
(ar/ensure-denote-silo ar/denote-zettelkasten-dir)

(ar/ensure-org-dir "journal/")
(ar/ensure-org-dir "attachments/")
(ar/ensure-org-dir "downloads/")

(setopt org-directory ar/org-directory
        org-attach-id-dir (expand-file-name "attachments/" ar/org-directory)
        org-attach-use-inheritance t)

(use-package org
  :defer t
  :custom
  ;; Navigation
  (org-return-follows-link t)
  ;; Display
  (org-pretty-entities t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-adapt-indentation nil)
  (org-confirm-babel-evaluate nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  ;; Source blocks
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  ;; Performance
  (org-element-use-cache t)
  (org-element-cache-persistent nil)
  (org-highlight-latex-and-related '(latex script entities))
  ;; Startup
  (org-startup-folded 'overview)
  (org-startup-with-inline-images nil)
  (org-startup-with-latex-preview nil)
  (org-cycle-separator-lines 2)
  ;; Habit graph: loaded by org-agenda's module processing, never at startup.
  (org-modules '(org-habit)))

(defun ar/org-setup-yas-parents ()
  "Set YASnippet parent tables for Org buffers."
  (setq-local yas-parents '(latex-mode)))

(with-eval-after-load 'org
  (setopt org-list-allow-alphabetical t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'ar/org-setup-yas-parents)
  ;; Org 9.8 renamed the inline-image display to link previews.
  (defun ar/org-babel-refresh-images ()
    "Refresh inline images or link previews after Babel execution."
    (cond ((fboundp 'org-link-preview-refresh) (org-link-preview-refresh))
          ((fboundp 'org-redisplay-inline-images) (org-redisplay-inline-images))))
  (add-hook 'org-babel-after-execute-hook #'ar/org-babel-refresh-images))

;; org-contrib: the retired ob-/ox- libraries (extra Babel languages and
;; exporters); org requires them on demand.
(use-package org-contrib
  :defer t
  :after org)

;; org-habit rides on the `org-modules' entry in the org block above, so
;; both org-habit and org-agenda stay out of startup.

(use-package org-habit
  :defer t
  :after org
  :custom
  (org-habit-graph-column 60)
  (org-habit-show-habits-only-for-today t)
  (org-habit-preceding-days 21)
  (org-habit-following-days 7))

;; The Denote second brain.

(use-package denote
  :defer t
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory ar/denote-zettelkasten-dir)
  (denote-file-type 'org)
  (denote-known-keywords
   '("emacs" "philosophy" "reading" "project" "journal"
     "meeting" "idea" "physics" "zettelkasten" "gtd"
     "workflow" "architecture" "literature" "reference"
     "area" "archive"))
  (denote-org-capture-specifiers "%i\n%?\n")
  :config
  ;; Org export ignores directory-local variables, breaking denote: links
  ;; in silos.  Enable #+bind so per-silo overrides resolve correctly.
  (setopt org-export-allow-bind-keywords t)
  (put 'denote-org-front-matter 'safe-local-variable #'stringp)

  (setopt denote-save-buffers nil
          denote-infer-keywords t
          denote-sort-keywords t
          denote-prompts '(title keywords)
          denote-excluded-directories-regexp "\\.git"
          denote-keywords-to-not-infer-regexp nil
          denote-rename-confirmations '(rewrite-front-matter modify-file-name)
          denote-date-prompt-use-org-read-date t)
  (denote-rename-buffer-mode 1)
  ;; Atomic note skeleton after note creation in the Zettelkasten root.
  (defun ar/denote-insert-atomic-note-template ()
    "Insert a minimal atomic note skeleton after note creation."
    (when (and (derived-mode-p 'org-mode)
               buffer-file-name
               (member (file-name-directory buffer-file-name)
                       (denote-directories)))
      (goto-char (point-max))
      (insert "\n* Summary\n")
      (save-excursion
        (insert "\n* Connections\n-\n* Sources\n- "))))
  (add-hook 'denote-after-new-note-hook
            #'ar/denote-insert-atomic-note-template))

(use-package denote-silo
  :defer t
  :after denote
  :commands (denote-silo-create-note
             denote-silo-open-or-create
             denote-silo-select-silo-then-command
             denote-silo-dired
             denote-silo-cd)
  :custom
  (denote-silo-directories
   (delete-dups
    (cons (file-name-as-directory ar/denote-zettelkasten-dir)
          (cl-loop for parent in '("projects" "areas" "resources" "journal")
                   for dir = (expand-file-name parent ar/org-directory)
                   if (file-directory-p dir)
                   nconc (cl-loop for sub in (directory-files dir t directory-files-no-dot-files-regexp)
                                  if (and (file-directory-p sub)
                                          (file-exists-p (expand-file-name ".dir-locals.el" sub)))
                                  collect (file-name-as-directory sub)))))))

(use-package denote-sequence
  :defer t
  :after denote
  :commands (denote-sequence
             denote-sequence-new-parent
             denote-sequence-new-child
             denote-sequence-new-child-of-current
             denote-sequence-new-sibling
             denote-sequence-new-sibling-of-current
             denote-sequence-rename-as-parent
             denote-sequence-reparent
             denote-sequence-reparent-recursive
             denote-sequence-find
             denote-sequence-find-dired
             denote-sequence-find-next-sibling
             denote-sequence-find-previous-sibling
             denote-sequence-link
             denote-sequence-dired
             denote-sequence-convert
             denote-sequence-view-hierarchy)
  :custom
  (denote-sequence-scheme 'alphanumeric-delimited)
  (denote-sequence-hierarchy-indentation 2)
  (denote-sequence-hierarchy-move-and-open t))

(use-package denote-org
  :defer t
  :after (denote org)
  :commands (denote-org-link-to-heading
             denote-org-backlinks-for-heading
             denote-org-extract-org-subtree
             denote-org-convert-links-to-file-type
             denote-org-convert-links-to-denote-type
             denote-org-dblock-insert-files
             denote-org-dblock-insert-links
             denote-org-dblock-insert-backlinks
             denote-org-dblock-insert-missing-links
             denote-org-dblock-insert-files-as-headings))

(use-package denote-journal
  :defer t
  :after denote
  :commands (denote-journal-new-entry
             denote-journal-new-or-existing-entry
             denote-journal-link-or-create-entry)
  :custom
  (denote-journal-directory (expand-file-name "journal" ar/org-directory))
  (denote-journal-title-format 'day-date-month-year))

(use-package denote-journal-capture
  :defer t
  :after denote-journal
  :commands (denote-journal-capture-entry-for-date
             denote-journal-capture-entry-today
             denote-journal-capture-timestamp
             denote-journal-capture-insert-date))

(use-package denote-regexp
  :defer t
  :after denote)

(use-package denote-menu
  :defer t
  :after denote
  :commands (list-denotes
             denote-menu-filter
             denote-menu-filter-by-keyword
             denote-menu-filter-out-keyword
             denote-menu-clear-filters
             denote-menu-export-to-dired)
  :custom
  (denote-menu-show-file-signature t)
  (denote-menu-show-file-type t)
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired))

(use-package denote-merge
  :defer t
  :after denote
  :commands (denote-merge-file
             denote-merge-region
             denote-merge-region-plain
             denote-merge-region-plain-indented
             denote-merge-region-org-src
             denote-merge-region-org-quote
             denote-merge-region-org-example
             denote-merge-region-markdown-quote
             denote-merge-region-markdown-fenced-block)
  :custom
  ;; Prevent data loss during backlink rewiring; verify graph before saving.
  (denote-merge-save-buffers nil)
  (denote-merge-kill-buffers nil))

(use-package denote-solo
  :after denote
  :custom
  (denote-solo-directories
   `(("Zettelkasten" . ,ar/denote-zettelkasten-dir)
     ("Journal"      . ,(expand-file-name "journal" ar/org-directory))))
  :config
  (setq denote-solo-modeline-function
        (lambda (name)
          (concat " "
                  (propertize (concat (ar/org--icon #'nerd-icons-faicon "nf-fa-book" :v-adjust 0.02)
                                      " " name)
                              'face 'font-lock-string-face)
                  " ")))
  (denote-solo-mode 1))

(use-package denote-review
  :defer t
  :after denote
  :commands (denote-review-set-date
             denote-review-display-list
             denote-review-set-date-dired-marked-files))

(use-package denote-explore
  :defer t
  :after (denote denote-regexp)
  :commands (denote-explore-count-notes
             denote-explore-count-keywords
             denote-explore-list-keywords
             denote-explore-barchart-timeline
             denote-explore-barchart-keywords
             denote-explore-barchart-filetypes
             denote-explore-random-note
             denote-explore-random-regex
             denote-explore-random-link
             denote-explore-random-keyword
             denote-explore-random-signature
             denote-explore-duplicate-notes
             denote-explore-duplicate-notes-dired
             denote-explore-missing-links
             denote-explore-zero-keywords
             denote-explore-single-keywords
             denote-explore-rename-keyword
             denote-explore-sync-metadata
             denote-explore-isolated-files
             denote-explore-network
             denote-explore-network-regenerate
             denote-explore-barchart-degree
             denote-explore-barchart-backlinks)
  :custom
  (denote-explore-network-directory
   (ar/var-file "denote-explore/")))

(use-package denote-wordcloud
  :defer t
  :after denote
  :commands (denote-wordcloud
             denote-wordcloud-list
             denote-wordcloud-list-by-frequency)
  :custom-face
  (denote-wordcloud-face-0 ((t (:foreground "#7aa2f7"))))
  (denote-wordcloud-face-1 ((t (:foreground "#e0af68"))))
  (denote-wordcloud-face-2 ((t (:foreground "#9ece6a"))))
  (denote-wordcloud-face-3 ((t (:foreground "#73daca"))))
  (denote-wordcloud-face-4 ((t (:foreground "#bb9af7"))))
  (denote-wordcloud-face-5 ((t (:foreground "#9d7cd8"))))
  (denote-wordcloud-face-6 ((t (:foreground "#ff9e64"))))
  (denote-wordcloud-face-7 ((t (:foreground "#f7768e"))))
  (denote-wordcloud-face-8 ((t (:foreground "#7dcfff"))))
  (denote-wordcloud-face-9 ((t (:foreground "#db4b4b")))))

(use-package consult-denote
  :after (consult denote)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (consult-denote-mode 1))

;; Prettification (:pretty): org-modern glyphs + org-appear markers.
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars '("◉" "○" "✸" "✿" "✤" "✜" "◇" "▷"))
  (org-modern-hide-stars nil)
  (org-modern-checkbox '((?X . "☑") (?- . "◧") (?\s . "☐")))
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-keyword t)
  (org-modern-horizontal-rule t)
  (org-modern-block-keyword t)
  (org-modern-block-name
   '(("src"        "»" "«")
     ("example"    "¶" "¶")
     ("quote"      "❝" "❞")
     ("verse"      "❝" "❞")
     ("center"     "≡" "≡")
     ("comment"    "⊘" "⊘")
     ("export"     "⇥" "⇤")
     ("latex"      "∫" "∫")
     ("theorem"    "∴" "∎")
     ("proof"      "∵" "∎")
     ("definition" "≝" "≝")
     ("warning"    "⚠" "⚠")
     ("note"       "✎" "✎")))
  (org-modern-list '((43 . "▸") (45 . "–") (42 . "•")))
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal nil)
  (org-modern-timestamp nil)
  (org-modern-progress nil))

(ar/define-large-file-guard org-modern-mode)

;; Toggle emphasis markers on cursor entry/exit.
(use-package org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t))

;; TOC heading, clipboard links, formatted clipboard export.
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'toc-org-mode))

(use-package ox-clip
  :defer t
  :commands (ox-clip-formatted-copy))

(use-package org-cliplink
  :defer t
  :commands (org-cliplink))

;; Async babel block execution.
(use-package ob-async
  :defer t
  :after org)

;; Jupyter's Org Babel backend is loaded when a `jupyter-LANG' block is used.
;; evil-org integration (+everywhere parity).
(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Structure templates (org-tempo) and Babel lazy-loading.

(with-eval-after-load 'org
  (require 'org-tempo)
  (dolist (pair '(("sh" . "src shell")
                  ("el" . "src emacs-lisp")
                  ("py" . "src python")
                  ("jp" . "src jupyter-python")
                  ("dot" . "src dot")
                  ("plantuml" . "src plantuml")
                  ("tex" . "src latex")
                  ("yml" . "src yaml")
                  ("kbd" . "src kbd")
                  ("kdl" . "src kdl")))
    (add-to-list 'org-structure-template-alist pair))

  (defvar ar/org-babel-mode-alist
    '((c . C) (cpp . C) (C++ . C) (D . C)
      (elisp . emacs-lisp) (sh . shell) (bash . shell)
      (matlab . octave) (rust . rustic-babel) (amm . ammonite))
    "Alist mapping divergent block headers to their `ob-*' package names.")

  ;; Jupyter blocks require a session; define this before the first block is
  ;; parsed so the initial execution works without a manual :session header.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "python")))

  (defun ar/org--babel-lazy-load (lang)
    "Lazy load the babel package for LANG and register it."
    (cl-check-type lang (or symbol null))
    (when lang
      (let* ((babel-lang
              (if (string-prefix-p "jupyter-" (symbol-name lang))
                  'jupyter
                lang))
             (already-enabled
              (cdr (assq babel-lang org-babel-load-languages)))
             (loaded
              (if (eq babel-lang 'jupyter)
                  ;; Ensure the backend loads even if a user has already
                  ;; listed it in `org-babel-load-languages'.
                  (require 'ob-jupyter nil t)
                (unless already-enabled
                  (or (require (intern (format "ob-%s" babel-lang)) nil t)
                      (require babel-lang nil t))))))
        (when loaded
          (if (eq babel-lang 'jupyter)
              ;; Keep Jupyter last in the registry as its Org integration adds
              ;; source-language mappings for available kernel specifications.
              (setq org-babel-load-languages
                    (append (assq-delete-all 'jupyter
                                             org-babel-load-languages)
                            '((jupyter . t))))
            (unless already-enabled
              (add-to-list 'org-babel-load-languages
                           (cons babel-lang t) t)))
          ;; If Org Babel lazy-loads the backend while processing the current
          ;; Org buffer, its `org-mode-hook' has already run. Register aliases
          ;; now so the first `jupyter-LANG' block is ready to execute.
          (when (and (eq babel-lang 'jupyter)
                     (fboundp 'org-babel-jupyter-make-local-aliases))
            (org-babel-jupyter-make-local-aliases)))
        loaded)))

  (define-advice org-babel-exp-src-block (:before (&optional element) lazy-load-export)
    "Lazy load a babel package when a block is processed during exporting."
    (let* ((info (org-babel-get-src-block-info nil element))
           (lang (nth 0 info))
           (lang (cond ((symbolp lang) lang) ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang ar/org-babel-mode-alist)) lang)))
      (ar/org--babel-lazy-load lang)))

  (define-advice org-src--get-lang-mode (:before (lang) lazy-load-syntax)
    "Lazy load a babel package to ensure syntax highlighting in edit buffers."
    (let* ((lang-str (if (stringp lang) lang (symbol-name lang)))
           (lang-sym (intern lang-str)))
      (unless (cdr (assoc-string lang-str org-src-lang-modes))
        (ar/org--babel-lazy-load lang-sym))))

  (define-advice org-babel-confirm-evaluate (:after-while (info) lazy-load-execute)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang) ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang ar/org-babel-mode-alist)) lang)))
      (ar/org--babel-lazy-load lang)
      t))

  (advice-add #'org-babel-do-load-languages :override #'ignore))

;; Simplified src edit buffer names.
(defun ar/org-src-simplify-buffer-name (_org-buffer-name lang)
  "Simplify Org source edit buffer names to `*Src LANG*'."
  (if (and (boundp 'org-src--source-type)
           (eq org-src--source-type 'example))
      "*Org Example*"
    (format "*Src %s*" (or lang "text"))))

(with-eval-after-load 'org-src
  (advice-add #'org-src--construct-edit-buffer-name
              :override #'ar/org-src-simplify-buffer-name))

;; Denote's generated autoloads cover only part of the entry matrix below;
;; register the missing autoload objects so the first keypress loads the
;; right library instead of signaling void-function.
(autoload #'denote-rename-file-keywords "denote" nil t)
(autoload #'denote-dired "denote" nil t)
(autoload #'denote-sequence-reparent "denote-sequence" nil t)
(autoload #'list-denotes "denote-menu" nil t)

;; The Denote notes leader tree (SPC n).
(ar/global-leader
  "n"   '(:ignore t :wk "notes")

  ;; Core Denote
  "n n" '(denote :wk "New note")
  "n N" '(denote-subdirectory :wk "New note in subdir")
  "n t" '(denote-type :wk "New note (choose type)")
  "n r" '(denote-rename-file :wk "Rename file")
  "n R" '(denote-rename-file-using-front-matter :wk "Rename from front matter")
  "n k" '(denote-rename-file-keywords :wk "Edit keywords")
  "n d" '(denote-dired :wk "Denote dired")

  ;; Search & Discovery
  "n f" '(consult-denote-find :wk "Find note")
  "n s" '(consult-denote-grep :wk "Search contents")

  ;; Linking
  "n l" '(denote-link :wk "Insert link")
  "n i" '(denote-link-or-create :wk "Insert link or create")
  "n b" '(denote-backlinks :wk "Backlinks")

  ;; Journaling
  "n j" '(denote-journal-new-or-existing-entry :wk "Journal (today)")
  "n J" '(denote-journal-new-entry :wk "New journal entry")

  ;; Sequence Notes / Folgezettel
  "n q"   '(:ignore t :wk "sequence")
  "n q n" '(denote-sequence :wk "New sequence")
  "n q p" '(denote-sequence-new-parent :wk "New parent")
  "n q c" '(denote-sequence-new-child-of-current :wk "New child (current)")
  "n q b" '(denote-sequence-new-sibling-of-current :wk "New sibling (current)")
  "n q f" '(denote-sequence-find :wk "Find relative")
  "n q l" '(denote-sequence-link :wk "Link to sequence")
  "n q r" '(denote-sequence-reparent :wk "Reparent file")
  "n q v" '(denote-sequence-view-hierarchy :wk "View hierarchy")

  ;; Silo Management
  "n S"   '(:ignore t :wk "silo")
  "n S s" '(denote-silo-select-silo-then-command :wk "Silo dispatch")
  "n S o" '(denote-silo-open-or-create :wk "Open/create silo")
  "n S n" '(denote-silo-create-note :wk "New note in silo")
  "n S d" '(denote-silo-dired :wk "Silo dired")
  "n S l" '(denote-solo-switch :wk "Switch solo context")

  ;; Explore & Analytics
  "n e"   '(:ignore t :wk "explore")
  "n e c" '(denote-explore-count-notes :wk "Count notes")
  "n e k" '(denote-explore-count-keywords :wk "Count keywords")
  "n e L" '(denote-explore-list-keywords :wk "List keywords")
  "n e f" '(denote-explore-barchart-filetypes :wk "Filetypes barchart")
  "n e w" '(denote-explore-barchart-keywords :wk "Keywords barchart")
  "n e t" '(denote-explore-barchart-timeline :wk "Timeline barchart")
  "n e r" '(denote-explore-random-note :wk "Random note")
  "n e x" '(denote-explore-random-regex :wk "Random regex")
  "n e l" '(denote-explore-random-link :wk "Random link")
  "n e K" '(denote-explore-random-keyword :wk "Random keyword")
  "n e S" '(denote-explore-random-signature :wk "Random signature")
  "n e d" '(denote-explore-duplicate-notes :wk "Duplicate notes")
  "n e m" '(denote-explore-missing-links :wk "Missing links")
  "n e z" '(denote-explore-zero-keywords :wk "Zero keywords")
  "n e s" '(denote-explore-single-keywords :wk "Single keywords")
  "n e R" '(denote-explore-rename-keyword :wk "Rename keyword")
  "n e y" '(denote-explore-sync-metadata :wk "Sync metadata")
  "n e i" '(denote-explore-isolated-files :wk "Isolated files")
  "n e n" '(denote-explore-network :wk "Generate network")
  "n e g" '(denote-explore-network-regenerate :wk "Regenerate network")
  "n e G" '(denote-explore-barchart-degree :wk "Degree barchart")
  "n e b" '(denote-explore-barchart-backlinks :wk "Backlinks barchart")
  "n e W" '(denote-wordcloud :wk "Wordcloud")
  "n e F" '(denote-wordcloud-list-by-frequency :wk "Wordcloud (frequency)")

  ;; Management & Refactoring
  "n m"   '(:ignore t :wk "manage")
  "n m m" '(list-denotes :wk "Tabulated menu")
  "n m f" '(denote-merge-file :wk "Merge file")
  "n m r" '(denote-merge-region :wk "Merge region")
  "n m q" '(denote-merge-region-org-quote :wk "Merge region (quote)")
  "n m v" '(denote-review-display-list :wk "Review list")
  "n m u" '(denote-review-set-date :wk "Update review date"))

;; Org agenda / capture basics (Doom defaults).
(ar/global-leader
  "o"     '(:ignore t :wk "open")
  "o c"   '(org-capture :wk "Capture")
  "o L"   '(org-store-link :wk "Store link")
  "o a"   '(:ignore t :wk "agenda")
  "o a a" '(org-agenda :wk "Agenda dashboard")
  "o A"   '(org-agenda :wk "Agenda dashboard")
  "o a m" '(org-tags-view :wk "Tags search")
  "o a t" '(org-todo-list :wk "Global TODO list")
  "o a v" '(consult-org-heading :wk "Fuzzy heading jump"))

;; Optional integration, only when the library is present.
(when (locate-library "org-src-context")
  (with-eval-after-load 'org
    (require 'org-src-context)))

(provide 'org-config)
;;; org-config.el ends here
