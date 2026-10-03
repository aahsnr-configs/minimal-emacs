;;; org-config.el --- Org mode and the Denote second brain -*- lexical-binding: t; -*-

;;; Commentary:

;; Org configuration, capture, agenda, and the whole Denote ecosystem
;; (silo discovery, journaling, sequences, transclusion, sections).
;; The module requires the real Org library eagerly by design: with no
;; user file named org.el on `load-path' the require is unambiguous, and
;; it guarantees that every `with-eval-after-load' org registration in
;; this config and in denote's autoloads fires with real Org present.

;;; Code:

;; Module preamble.  `cl-lib' supplies cl-loop/cl-pushnew/cl-check-type used
;; by the silo discovery and the transclusion bridge;
;; `subr-x' supplies string-trim/string-split for the capture helpers.
;; Requiring both here keeps this module self-contained at load and
;; byte-compile time instead of relying on init.el's bootstrap requires.

(require 'cl-lib)
(require 'subr-x)

(defun ar/org--icon (fn icon &rest props)
  "Return the nerd-icons glyph for ICON, or the empty string.
FN is one of the `nerd-icons-*' glyph functions.  Icons are display-only:
in batch runs, on terminals, or before nerd-icons is loaded, the empty
string keeps the surrounding group names and glyphs well-formed instead of
signaling void-function (`icons-displayable-p' comes from the core
module)."
  (if (and (fboundp fn) (display-graphic-p) (icons-displayable-p))
      (apply fn icon props)
    ""))

;; Route Org's cache directories into the var tree BEFORE the real library
;; loads: org-persist reads these during its own load.  The variables are
;; not bound yet, so `setopt' records plain values that the defcustoms keep
;; when Org defines them.
(setopt org-persist-directory (ar/var-file "org/persist/")
        org-preview-latex-image-directory (ar/var-file "org/ltximg/")
        org-publish-timestamp-directory (ar/var-file "org/timestamps/")
        org-id-locations-file (ar/var-file "org/id-locations"))

;; Require the real Org library.  With no user file named org.el on
;; `load-path' this resolves unambiguously to the bundled Org, and the
;; feature `org' being provided by the real library is what keeps
;; denote's eval-after-load registrations safe: they fire at the end of
;; the file that provides `org' -- the bundled one, with Org fully
;; initialized.
(require 'org)

;; Base directory for all Org and Denote files, respecting XDG specifications.

(defvar my/org-directory
  (let ((xdg-docs (getenv "XDG_DOCUMENTS_DIR")))
    (if (and xdg-docs (file-directory-p xdg-docs))
        (expand-file-name "org/" xdg-docs)
      (expand-file-name "~/org/")))
  "Base directory for all Org and Denote files, respecting XDG specifications.")

(unless (file-directory-p my/org-directory)
  (make-directory my/org-directory t))

(setq my/org-directory (file-truename my/org-directory))

(defun my/ensure-org-dir (subdir)
  "Ensure SUBDIR exists under `my/org-directory', creating parents if needed."
  (let ((dir (expand-file-name subdir my/org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun my/ensure-org-file (filepath &optional template)
  "Ensure FILEPATH exists, seeding it with TEMPLATE if newly created."
  (let ((full-path (expand-file-name filepath my/org-directory)))
    (unless (file-exists-p full-path)
      (with-temp-file full-path
        (when template (insert template))))
    full-path))

(defun my/ensure-denote-silo (silo-dir)
  "Isolate SILO-DIR via a native `.dir-locals.el' file."
  ;; Enforce trailing slash to prevent false-positive prefix collisions.
  (setq silo-dir (file-name-as-directory silo-dir))
  (let ((dir-locals (expand-file-name ".dir-locals.el" silo-dir)))
    (unless (file-exists-p dir-locals)
      (with-temp-file dir-locals
        (insert ";;; Directory Local Variables.  For more information evaluate:\n")
        (insert ";;;\n")
        (insert ";;;     (info \"(emacs) Directory Variables\")\n")
        ;; Denote 4.x parity: 5th %s placeholder for the SIGNATURE component.
        ;; Mathematically required for `denote-sequence' to render correctly
        ;; in silos.  Bakes #+bind into front matter so Org export resolves
        ;; denote: links in silos.
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

(defun my/create-denote-project-silo (project-name)
  "Create a new isolated Denote silo for PROJECT-NAME under `projects/'."
  (interactive "sProject Name: ")
  (let ((silo-dir (my/ensure-org-dir (concat "projects/" project-name))))
    (my/ensure-denote-silo silo-dir)
    (message "Created Denote Silo: %s" silo-dir)
    silo-dir))

(defvar my/denote-zettelkasten-dir (my/ensure-org-dir "zettelkasten/")
  "Global, evergreen concepts.  Serves as the fallback `denote-directory'.")
(my/ensure-denote-silo my/denote-zettelkasten-dir)

(my/ensure-org-dir "projects/")
(my/ensure-org-dir "areas/")
(my/ensure-org-dir "resources/")
(my/ensure-org-dir "archives/")
(my/ensure-org-dir "journal/")
(my/ensure-org-dir "agenda/")
(my/ensure-org-dir "agenda/archive/")
;; Seed top-level headings to prevent `file+headline' capture targets from
;; appending entries at EOF and destroying structural integrity.
(my/ensure-org-file "agenda/todo.org"
                    "#+title: Task Registry\n#+filetags: :agenda:\n* Inbox\n* Projects\n* Tasks\n* Someday\n* Waiting\n")
(my/ensure-org-file "agenda/journal.org"
                    "#+title: Journal\n#+filetags: :journal:\n* Meetings\n")
(my/ensure-org-file "agenda/habits.org"
                    "#+title: Habits\n#+filetags: :habit:\n* Habits\n")
(my/ensure-org-dir "attachments/")
(my/ensure-org-dir "downloads/")

;; Route Org's global state to the directory tree created above.

(setopt org-directory my/org-directory
        org-agenda-files (list (expand-file-name "agenda/todo.org" my/org-directory)
                               (expand-file-name "agenda/habits.org" my/org-directory)
                               (expand-file-name "agenda/journal.org" my/org-directory))
        org-attach-id-dir (expand-file-name "attachments/" my/org-directory)
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
  (org-cycle-separator-lines 2))


(defun ar/org-setup-yas-parents ()
  "Set YASnippet parent tables for Org buffers."
  (setq-local yas-parents '(latex-mode)))

(defun ar/org-disable-indent-mode ()
  "Prevent org-indent-mode from conflicting with org-modern block fringes."
  (org-indent-mode -1))

(defun ar/org-agenda-setup ()
  "Configure agenda buffer display: truncated lines, no mode-line chrome."
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil))

(defun ar/org-capture-setup ()
  "Hide all chrome in capture buffers for distraction-free entry."
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil))

(with-eval-after-load 'org
  ;; Alphabetical list items are allowed everywhere.  `org-modules' is
  ;; deliberately NOT set here: `org-habit.el' requires `org-agenda' at its
  ;; own load time, so setting it the moment Org loads would cascade
  ;; org-habit and org-agenda into every startup.  It is set from the
  ;; org-agenda block below -- the only consumer of org-habit -- where
  ;; `setopt' still runs Org's own `org-set-modules' setter, which is the
  ;; replacement for the old config's fragile pre-load `(defvar
  ;; org-modules ...)'.
  (setopt org-list-allow-alphabetical t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'ar/org-setup-yas-parents)
  (add-hook 'org-mode-hook #'ar/org-disable-indent-mode)
  (add-hook 'org-agenda-mode-hook #'ar/org-agenda-setup)
  (add-hook 'org-capture-mode-hook #'ar/org-capture-setup)
  ;; Org 9.8 renamed the inline-image display to link previews;
  ;; `org-link-preview-refresh' is the buffer-wide refresh the old
  ;; `org-display-inline-images' (obsolete in 9.8) provided.
  (add-hook 'org-babel-after-execute-hook #'org-link-preview-refresh))


(with-eval-after-load 'org
  ;; 1. TASK STATE MACHINE
  ;; `@' = note on entry, `/' = log on exit, `!' = timestamp.
  (setopt org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)"
                      "|" "DONE(d)" "CANCEL(c@)")))

  ;; State-change metadata folded into :LOGBOOK: drawer.
  (setopt org-log-into-drawer t
          org-log-done 'time
          org-log-redeadline 'time
          org-log-reschedule 'time)

  ;; 2. TODO KEYWORD FACES (Tokyo Night palette).
  (setopt org-todo-keyword-faces
          '(("TODO"   . (:foreground "#f7768e" :weight bold))
            ("NEXT"   . (:foreground "#e0af68" :weight bold))
            ("WAIT"   . (:foreground "#bb9af7" :weight bold))
            ("HOLD"   . (:foreground "#565f89" :weight bold))
            ("DONE"   . (:foreground "#9ece6a" :weight bold))
            ("CANCEL" . (:foreground "#565f89" :strike-through t))))

  ;; 3. CONTEXTUAL TAGGING: mutual exclusivity within :startgroup/:endgroup,
  ;; one context tag per task; other tags combine freely.
  (setopt org-tag-alist
          '((:startgroup)
            ("@computer" . ?c)
            ("@home" . ?h)
            ("@errands" . ?e)
            ("@phone" . ?p)
            ("@deep_work" . ?d)
            (:endgroup)
            ("read" . ?r)
            ("meeting" . ?m)
            ("urgent" . ?u)
            ("someday" . ?s)))

  ;; Tag color palette consumed by `org-modern-tag' pill engine.
  (setopt org-tag-faces
          '(("@computer"  . (:foreground "#7aa2f7" :weight bold))
            ("@home"      . (:foreground "#9ece6a" :weight bold))
            ("@errands"   . (:foreground "#e0af68" :weight bold))
            ("@phone"     . (:foreground "#bb9af7" :weight bold))
            ("@deep_work" . (:foreground "#f7768e" :weight bold))
            ("urgent"     . (:foreground "#f7768e" :weight bold :underline t))))

  ;; Priority palette consumed by `org-modern-priority' badge engine.
  (setopt org-priority-faces
          '((?A . (:foreground "#f7768e" :weight bold))
            (?B . (:foreground "#e0af68" :weight bold))
            (?C . (:foreground "#7aa2f7" :weight bold))))

  ;; 4. REFILE & ARCHIVE: `C-c C-w' routes inbox items to level-2 Project
  ;; headings; completed subtrees route to `agenda/archive/todo.org_archive'.
  (setopt org-refile-targets
          '((nil :maxlevel . 2)
            (org-agenda-files :maxlevel . 2))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-archive-location "archive/%s_archive::"))


(use-package org-agenda
  :defer t
  :after org
  :custom
  ;; Compact blocks: suppresses default headers (org-super-agenda adds its
  ;; own).
  (org-agenda-compact-blocks t)
  ;; Performance: skip startup, tag inheritance, and unavailable files.
  (org-agenda-inhibit-startup t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-ignore-properties '(effort appt))
  ;; Task visibility: hide completed scheduled/deadline items.
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-dim-blocked-tasks nil)
  ;; Window management: reuse current window, restore layout on quit.
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-sticky nil)
  ;; Span and calendar: daily default, start on current day, show all dates.
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-show-all-dates t)
  ;; Time grid: visual hour markers with current-time indicator.
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀ now ─────────────────────────────────────────────────")
  ;; Sorting: habits first, then time, then priority.  org-super-agenda
  ;; :order overrides within groups; this is the fallback.
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  ;; Prefix format: category + timestamp per item.
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  ;; Tag alignment: right-aligned at column 80.
  (org-tags-column -80)
  :config
  ;; Enable habits through Org's own setter: `setopt' runs
  ;; `org-set-modules', which requires org-habit (org-habit in turn
  ;; requires org-agenda -- already loaded here).  Firing this at agenda
  ;; load instead of module load keeps org-habit and org-agenda out of
  ;; startup; see the post-load hooks block above.
  (setopt org-modules '(org-habit))
  ;; Doom Emacs Parity: Route the native dispatcher, calendar, and export
  ;; dispatcher to a shrink-to-fit bottom drawer.  `display-buffer-at-bottom'
  ;; creates a standard horizontal split that `org-fit-window-to-buffer' can
  ;; successfully resize, bypassing the rigid side-window locking that
  ;; causes empty space artifacts.
  (add-to-list 'display-buffer-alist
               '("\\*\\(?:Org \\)?Agenda Commands\\*\\|\\*Calendar\\*\\|\\*Org Export Dispatcher\\*"
                 (display-buffer-at-bottom)
                 (window-height . fit-window-to-buffer)
                 (window-parameters (no-delete-other-windows . t)
                                    (mode-line-format . none))))
  ;; Org-agenda aggressively resets the modeline during finalization; the
  ;; ui module defines `ar/enable-hide-modeline'.
  (add-hook 'org-agenda-finalize-hook #'ar/enable-hide-modeline))


(use-package org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-header-separator ?─)
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-keep-order t)
  (org-super-agenda-unmatched-name "Other Tasks")
  (org-super-agenda-show-message nil)
  (org-super-agenda-header-prefix "▸ ")
  (org-super-agenda-final-group-separator
   "\n─────────────────────────────────────────────────\n")
  (org-super-agenda-date-format "%A, %B %d")
  :config
  (set-face-attribute 'org-super-agenda-header nil
                      :inherit 'font-lock-keyword-face :weight 'bold :height 1.05)

  (defun ar/org-super-agenda-toggle-group ()
    "Toggle visibility of the current agenda group at point."
    (interactive)
    (let ((pos (if (get-text-property (point) 'org-agenda-structural-header)
                   (point)
                 (or (previous-single-property-change
                      (point) 'org-agenda-structural-header)
                     (user-error "No group header found")))))
      (let* ((header-end (save-excursion
                           (goto-char pos)
                           (forward-line 0)
                           (unless (get-text-property
                                    (point) 'org-agenda-structural-header)
                             (forward-line -1))
                           (line-end-position)))
             (next-header (next-single-property-change
                           header-end 'org-agenda-structural-header
                           nil (point-max)))
             (next-header-start (if next-header
                                    (save-excursion
                                      (goto-char next-header)
                                      (forward-line 0)
                                      (point))
                                  (point-max)))
             (ovs (overlays-in (1+ header-end) next-header-start))
             (fold-ovs (seq-filter
                        (lambda (ov) (overlay-get ov 'ar/sa-fold)) ovs)))
        (if fold-ovs
            (dolist (ov fold-ovs) (delete-overlay ov))
          (let ((ov (make-overlay (1+ header-end) next-header-start)))
            (overlay-put ov 'invisible t)
            (overlay-put ov 'ar/sa-fold t))))))

  (defun ar/org-super-agenda-goto-first-item ()
    "Move point to the first item in the current group."
    (interactive)
    (let ((origin (point)))
      (forward-line 1)
      (if (or (get-text-property (point) 'org-agenda-structural-header)
              (eobp))
          (progn (goto-char origin)
                 (user-error "Empty group")))))

  (define-key org-super-agenda-header-map (kbd "TAB")
              #'ar/org-super-agenda-toggle-group)
  (define-key org-super-agenda-header-map (kbd "RET")
              #'ar/org-super-agenda-goto-first-item)
  (define-key org-super-agenda-header-map (kbd "q")
              #'org-agenda-quit)

  ;; :log closed MUST precede :discard; both match DONE items and
  ;; first-match-wins would otherwise empty the Completed group.
  ;;
  ;; setq, deliberately not setopt: upstream declares the variable with
  ;; `:type 'list', which rejects every real group alist (nested plists), so
  ;; setopt makes Emacs print "Value does not match org-super-agenda-groups's
  ;; type 'list'" the first time the package loads (org-super-agenda 1.4-pre
  ;; on Emacs 31.1).  The variable has no custom setter, so setq is lossless.
  (setq org-super-agenda-groups
          `((:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-check_circle" :v-adjust 0.02)
                            " Completed Today")
             :log closed
             :order 0)
            (:discard (:todo ("DONE" "CANCEL")))
            (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-alert" :v-adjust 0.02)
                            " Overdue")
             :deadline past
             :scheduled past
             :face font-lock-warning-face
             :order 1)
            (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-inbox" :v-adjust 0.02)
                            " Inbox")
             :todo "TODO"
             :file-path "agenda/todo\\.org"
             :transformer (string-remove-prefix "TODO " it)
             :order 2)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-calendar" :v-adjust 0.02)
                            " Today")
             :time-grid t
             :order 3)
            (:name ,(concat (ar/org--icon #'nerd-icons-mdicon "nf-md-chart_line" :v-adjust 0.02)
                            " Habits")
             :habit t
             :order 4)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-fire" :v-adjust 0.02)
                            " High Priority")
             :priority "A"
             :order 5)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-laptop" :v-adjust 0.02)
                            " @Computer")
             :and (:tag "@computer" :todo ("NEXT" "TODO"))
             :transformer (string-remove-prefix "NEXT " (string-remove-prefix "TODO " it))
             :order 6)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-home" :v-adjust 0.02)
                            " @Home")
             :and (:tag "@home" :todo ("NEXT" "TODO"))
             :transformer (string-remove-prefix "NEXT " (string-remove-prefix "TODO " it))
             :order 7)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-car" :v-adjust 0.02)
                            " @Errands")
             :and (:tag "@errands" :todo ("NEXT" "TODO"))
             :transformer (string-remove-prefix "NEXT " (string-remove-prefix "TODO " it))
             :order 8)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-phone" :v-adjust 0.02)
                            " @Phone")
             :and (:tag "@phone" :todo ("NEXT" "TODO"))
             :transformer (string-remove-prefix "NEXT " (string-remove-prefix "TODO " it))
             :order 9)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-brain" :v-adjust 0.02)
                            " @Deep Work")
             :and (:tag "@deep_work" :todo ("NEXT" "TODO"))
             :transformer (string-remove-prefix "NEXT " (string-remove-prefix "TODO " it))
             :order 10)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-hourglass_half" :v-adjust 0.02)
                            " Waiting For")
             :todo "WAIT"
             :face shadow
             :transformer (string-remove-prefix "WAIT " it)
             :order 11)
            (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-clock" :v-adjust 0.02)
                            " Due Soon")
             :deadline (before "+7d")
             :order 12)
            (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-kebab_horizontal"
                                                :v-adjust 0.02)
                            " Other Tasks")
             :take (15 (:anything t))
             :order 99))))


(with-eval-after-load 'org-agenda
  (defvar ar/org-sa-daily-groups
    `((:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-check_circle" :v-adjust 0.02)
                      " Completed Today")
       :log closed
       :order 0)
      (:discard (:todo ("DONE" "CANCEL")))
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-alert" :v-adjust 0.02)
                      " Overdue")
       (:take (10 (:deadline past :scheduled past)))
       :face font-lock-warning-face
       :order 1)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-inbox" :v-adjust 0.02)
                      " Inbox")
       :todo "TODO"
       :file-path "agenda/todo\\.org"
       :transformer (string-remove-prefix "TODO " it)
       :order 2)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-calendar" :v-adjust 0.02)
                      " Today")
       :time-grid t
       :order 3)
      (:name ,(concat (ar/org--icon #'nerd-icons-mdicon "nf-md-chart_line" :v-adjust 0.02)
                      " Habits")
       :habit t
       :order 4)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-fire" :v-adjust 0.02)
                      " High Priority")
       :priority "A"
       :order 5)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-laptop" :v-adjust 0.02)
                      " @Computer")
       :and (:tag "@computer" :todo ("NEXT" "TODO"))
       :transformer (string-remove-prefix "NEXT "
                                          (string-remove-prefix "TODO " it))
       :order 6)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-home" :v-adjust 0.02)
                      " @Home")
       :and (:tag "@home" :todo ("NEXT" "TODO"))
       :transformer (string-remove-prefix "NEXT "
                                          (string-remove-prefix "TODO " it))
       :order 7)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-car" :v-adjust 0.02)
                      " @Errands")
       :and (:tag "@errands" :todo ("NEXT" "TODO"))
       :transformer (string-remove-prefix "NEXT "
                                          (string-remove-prefix "TODO " it))
       :order 8)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-phone" :v-adjust 0.02)
                      " @Phone")
       :and (:tag "@phone" :todo ("NEXT" "TODO"))
       :transformer (string-remove-prefix "NEXT "
                                          (string-remove-prefix "TODO " it))
       :order 9)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-brain" :v-adjust 0.02)
                      " @Deep Work")
       :and (:tag "@deep_work" :todo ("NEXT" "TODO"))
       :transformer (string-remove-prefix "NEXT "
                                          (string-remove-prefix "TODO " it))
       :order 10)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-hourglass_half" :v-adjust 0.02)
                      " Waiting For")
       :todo "WAIT"
       :face shadow
       :transformer (string-remove-prefix "WAIT " it)
       :order 11)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-clock" :v-adjust 0.02)
                      " Due Soon")
       (:take (7 (:deadline (before "+7d"))))
       :order 12)
      ;; :children restricted to daily/weekly agenda views only.  NEVER use
      ;; in org-todo-list or org-tags-view (O(n×depth) recursive scan).
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-project" :v-adjust 0.02)
                      " Active Projects")
       :children todo
       :order 14)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-repo" :v-adjust 0.02)
                      " By Project")
       :auto-parent t
       :order 15)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-kebab_horizontal"
                                          :v-adjust 0.02)
                      " Other Tasks")
       (:take (15 (:anything t)))
       :order 99))
    "Groups for the Daily Dashboard command.")

  (defvar ar/org-sa-weekly-groups
    `((:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-inbox" :v-adjust 0.02)
                      " Inbox (Process to Zero)")
       :todo "TODO"
       :file-path "agenda/todo\\.org"
       :order 1)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-hourglass_half" :v-adjust 0.02)
                      " Waiting For")
       :todo "WAIT"
       :order 2)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-alert" :v-adjust 0.02)
                      " Stuck (No NEXT Actions)")
       :and (:children nil :todo ("TODO" "HOLD"))
       :order 3)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-moon" :v-adjust 0.02)
                      " Someday / Maybe")
       :tag "someday"
       :order 4)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-kebab_horizontal"
                                          :v-adjust 0.02)
                      " Everything Else")
       :anything t
       :order 99))
    "Groups for the Weekly Review command.")

  (defvar ar/org-sa-effort-groups
    `((:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-bolt" :v-adjust 0.02)
                      " Quick Wins (< 15 min)")
       :effort< "0:15"
       :order 1)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-fire" :v-adjust 0.02)
                      " High Energy")
       :property ("ENERGY" "high")
       :order 2)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-leaf" :v-adjust 0.02)
                      " Low Energy")
       :property ("ENERGY" "low")
       :order 3)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-clock" :v-adjust 0.02)
                      " Medium (15–60 min)")
       :and ((:not (:effort< "0:15")) (:not (:effort> "1:00")))
       :order 4)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-brain" :v-adjust 0.02)
                      " Deep Work (> 1 hr)")
       :effort> "1:00"
       :order 5)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-kebab_horizontal"
                                          :v-adjust 0.02)
                      " No Estimate")
       :anything t
       :order 99))
    "Groups for the Effort-Based command.")

  (defvar ar/org-sa-accomplishments-groups
    `((:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-check_circle" :v-adjust 0.02)
                      " Completed Today")
       :log closed
       :order 1)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-clock" :v-adjust 0.02)
                      " Clocked Today")
       :log clocked
       :order 2)
      (:name ,(concat (ar/org--icon #'nerd-icons-faicon "nf-fa-exchange" :v-adjust 0.02)
                      " State Changed Today")
       :log changed
       :order 3))
    "Groups for the Accomplishments command.")

  (defvar ar/org-sa-project-groups
    `((:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-project" :v-adjust 0.02)
                      " Active Projects (with open tasks)")
       :children todo
       :order 1)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-repo" :v-adjust 0.02)
                      " Tasks by Project")
       :auto-parent t
       :order 2)
      (:name ,(concat (ar/org--icon #'nerd-icons-octicon "nf-oct-kebab_horizontal"
                                          :v-adjust 0.02)
                      " Ungrouped")
       :anything t
       :order 99))
    "Groups for the Project Status command.")

  (setopt org-agenda-custom-commands
          `(("d" "Daily Dashboard"
             ((agenda ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-span 'day)
                       (org-agenda-start-day "0d")
                       (org-agenda-start-with-log-mode t)
                       (org-super-agenda-groups ,ar/org-sa-daily-groups)))))
            ("w" "Weekly Review"
             ((agenda ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-span 'week)
                       (org-agenda-start-day "-1d")
                       (org-agenda-start-with-log-mode t)
                       (org-super-agenda-groups ,ar/org-sa-weekly-groups)))))
            ("e" "Effort-Based View"
             ((alltodo ""
                       ((org-agenda-overriding-header "")
                        (org-super-agenda-groups ,ar/org-sa-effort-groups)))))
            ("A" "Accomplishments (Today)"
             ((agenda ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-span 'day)
                       (org-agenda-start-day "0d")
                       (org-agenda-start-with-log-mode t)
                       (org-super-agenda-groups ,ar/org-sa-accomplishments-groups)))))
            ("p" "Project Status"
             ((agenda ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-span 'day)
                       (org-agenda-start-day "0d")
                       (org-super-agenda-groups ,ar/org-sa-project-groups))))))))


(use-package org-habit
  :defer t
  :after org
  :custom
  (org-habit-show-habits t)
  (org-habit-graph-column 60)
  (org-habit-show-habits-only-for-today t)
  (org-habit-show-all-today t)
  (org-habit-preceding-days 21)
  (org-habit-following-days 7)
  (org-habit-scheduled-past-days 10000)
  :config
  ;; Nerd-icon completion glyphs; Org's own defaults remain when icons are
  ;; unavailable (display-only setting, same policy as `ar/org--icon').
  (when (and (fboundp 'nerd-icons-codicon) (icons-displayable-p))
    (setq org-habit-completed-glyph
          (string-to-char (nerd-icons-codicon "nf-cod-check"))
          org-habit-today-glyph
          (string-to-char (nerd-icons-codicon "nf-cod-circle_filled")))))


(use-package denote
  :defer t
  :hook (dired-mode . denote-dired-mode)
  :custom
  ;; Canonical Zettelkasten silo from the directory scaffolding above.
  (denote-directory my/denote-zettelkasten-dir)
  (denote-file-type 'org)
  (denote-known-keywords
   '("emacs" "philosophy" "reading" "project" "journal"
     "meeting" "idea" "physics" "zettelkasten" "gtd"
     "workflow" "architecture" "literature" "reference"
     "area" "archive"))
  ;; Atomic notes: no automatic link-to-origin; links added intentionally.
  (denote-org-capture-specifiers "%i\n%?\n")
  :config
  ;; Org export ignores directory-local variables, breaking denote: links
  ;; in silos.  Enable #+bind so per-silo overrides resolve correctly.
  (setopt org-export-allow-bind-keywords t)
  ;; Prevent security prompts when .dir-locals.el injects front-matter
  ;; templates.
  (put 'denote-org-front-matter 'safe-local-variable #'stringp)

  (setopt denote-save-buffers nil
          denote-infer-keywords t
          denote-sort-keywords t
          denote-prompts '(title keywords)
          denote-excluded-directories-regexp "\\.git"
          denote-keywords-to-not-infer-regexp nil
          denote-rename-confirmations '(rewrite-front-matter modify-file-name)
          denote-date-prompt-use-org-read-date t)
  ;; Clean buffer names with backlinks indicator (default "<-->").
  (denote-rename-buffer-mode 1)
  ;; Atomic note skeleton after note creation in the Zettelkasten root.
  (defun ar/denote-insert-atomic-note-template ()
    "Insert a minimal atomic note skeleton after note creation."
    (when (and (derived-mode-p 'org-mode)
               buffer-file-name
               ;; `denote-directories' (the denote >= 4.1 replacement for
               ;; the obsolete `denote-directory' function) resolves the
               ;; active silo paths to directory strings with trailing
               ;; slashes, so the membership test matches the note file's
               ;; directory exactly.
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
  ;; Discover silos: Zettelkasten root + subdirectories with
  ;; .dir-locals.el.  `file-name-as-directory' enforces trailing slashes to
  ;; prevent prefix collisions.
  (denote-silo-directories
   (delete-dups
    (cons (file-name-as-directory my/denote-zettelkasten-dir)
          (cl-loop for parent in '("projects" "areas" "resources" "journal")
                   for dir = (expand-file-name parent my/org-directory)
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
  ;; The sequence scheme: numeric, alphanumeric, or alphanumeric-delimited.
  (denote-sequence-scheme 'alphanumeric-delimited)
  ;; Number of spaces to indent by depth in the hierarchy view.
  (denote-sequence-hierarchy-indentation 2)
  ;; When non-nil, moving in the hierarchy view also displays the file.
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
  ;; Pin journal to a top-level domain to prevent silo-bleed and
  ;; Zettelkasten pollution.
  (denote-journal-directory (expand-file-name "journal" my/org-directory))
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
  ;; Surface Folgezettel signatures and file extensions in the grid.
  (denote-menu-show-file-signature t)
  (denote-menu-show-file-type t)
  :config
  ;; Wire native filtering and export commands to the tabulated-list mode
  ;; map.
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
  ;; Retain buffers post-merge for manual backlink graph verification.
  (denote-merge-kill-buffers nil))


(use-package denote-solo
  :after denote
  :custom
  ;; Dynamically map existing Denote silos from the directory scaffolding.
  (denote-solo-directories
   `(("Zettelkasten" . ,my/denote-zettelkasten-dir)
     ("Projects"     . ,(expand-file-name "projects" my/org-directory))
     ("Areas"        . ,(expand-file-name "areas" my/org-directory))
     ("Resources"    . ,(expand-file-name "resources" my/org-directory))
     ("Archives"     . ,(expand-file-name "archives" my/org-directory))
     ("Journal"      . ,(expand-file-name "journal" my/org-directory))))
  :config
  ;; Mode line indicator matching the Tokyo Night aesthetic.
  (setq denote-solo-modeline-function
        (lambda (name)
          (concat " "
                  (propertize (concat (ar/org--icon #'nerd-icons-faicon "nf-fa-book" :v-adjust 0.02)
                                      " " name)
                              'face 'font-lock-string-face)
                  " ")))
  ;; Activate the global minor mode to enable state persistence and
  ;; modeline rendering.
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
  ;; Route graph artifacts to the var directory to prevent Git pollution.
  (denote-explore-network-directory
   (ar/var-file "denote-explore/")))


(use-package denote-wordcloud
  :defer t
  :after denote
  :commands (denote-wordcloud
             denote-wordcloud-list
             denote-wordcloud-list-by-frequency)
  :custom-face
  ;; Tokyo Night colors; omit :height to inherit upstream scaling geometry.
  (denote-wordcloud-face-0 ((t (:foreground "#7aa2f7"))))
  (denote-wordcloud-face-1 ((t (:foreground "#e0af68"))))
  (denote-wordcloud-face-2 ((t (:foreground "#9ece6a"))))
  (denote-wordcloud-face-3 ((t (:foreground "#1abc9c"))))
  (denote-wordcloud-face-4 ((t (:foreground "#bb9af7"))))
  (denote-wordcloud-face-5 ((t (:foreground "#9d7cd8"))))
  (denote-wordcloud-face-6 ((t (:foreground "#ff9e64"))))
  (denote-wordcloud-face-7 ((t (:foreground "#f7768e"))))
  (denote-wordcloud-face-8 ((t (:foreground "#7aa2f7"))))
  (denote-wordcloud-face-9 ((t (:foreground "#e0af68")))))


(use-package org-transclusion
  :defer t
  :after (org denote)
  :commands (org-transclusion-add
             org-transclusion-add-all
             org-transclusion-make-from-link
             org-transclusion-mode
             org-transclusion-remove
             org-transclusion-remove-all
             org-transclusion-live-sync-start)
  :custom
  ;; Exclude property drawers and keywords to prevent Denote frontmatter
  ;; duplication.
  (org-transclusion-exclude-elements '(property-drawer keyword))
  ;; Include the first section of the transcluded document.
  (org-transclusion-include-first-section t)
  :custom-face
  ;; Tokyo Night palette for transclusion fringe and source overlays.
  (org-transclusion-fringe ((t (:foreground "#565f89"))))
  (org-transclusion-source-fringe ((t (:foreground "#565f89"))))
  (org-transclusion-source ((t (:background "#292e42"))))
  (org-transclusion-edit ((t (:background "#24283b"))))
  :config
  ;; Bridge: Translates `denote:' URIs to `file:' AST nodes for the
  ;; transclusion engine.
  (defun ar/denote-org-transclusion-add (link plist)
    "Resolve `denote:' links for org-transclusion."
    (when (string= "denote" (org-element-property :type link))
      (let* ((path (org-element-property :path link))
             (parts (string-split path "::"))
             (id (car parts))
             (search (cadr parts))
             (file (denote-get-path-by-id id)))
        (when file
          (let ((new-link (with-temp-buffer
                            (insert (concat "file:" file (when search (concat "::" search))))
                            (goto-char (point-min))
                            (org-element-link-parser))))
            (org-transclusion-add-org-file new-link plist))))))
  (cl-pushnew 'ar/denote-org-transclusion-add org-transclusion-add-functions)
  ;; Raw `define-key' required: live-sync map is a transient map, not an
  ;; Evil state map.
  (define-key org-transclusion-live-sync-map (kbd "p")
              #'org-transclusion-live-sync-paste))


(use-package consult-denote
  :after (consult denote)
  :custom
  ;; Route async content search strictly to ripgrep for performance.
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  ;; Activate global advice overrides and consult-buffer sources.
  (consult-denote-mode 1))

;; Clipboard URL extraction, habit schedule prompting.

(defun ar/capture-url-from-clipboard ()
  "Return the URL currently in the clipboard/`kill-ring'.
Falls back to `read-string' if no HTTP/HTTPS URL is found."
  ;; `current-kill' natively forces an interprogram paste query on
  ;; PGTK/Wayland before falling back, preventing stale `(car kill-ring)'
  ;; data.
  (let ((clip (string-trim (or (ignore-errors (current-kill 0)) ""))))
    (if (string-match-p "^https?://" clip)
        clip
      (read-string "URL: "))))

(defun ar/capture-url-as-org-link ()
  "Format the clipboard URL as an org link with a prompted description."
  (let* ((url  (ar/capture-url-from-clipboard))
         (desc (read-string (format "Description [%s]: " url) nil nil url)))
    (format "[[%s][%s]]" url desc)))

(defun ar/capture-habit-range-schedule ()
  "Prompt for ideal and max repeat intervals, return a SCHEDULED timestamp.
Uses `read-string' because %^{prompt} inside %(sexp) receives the literal
string, not user input, per the Org manual expansion order."
  (let* ((ideal   (read-string "Ideal interval in days (e.g. 1): " "1"))
         (ideal-n (string-to-number ideal))
         (max-def (number-to-string (* 2 ideal-n)))
         (max     (read-string
                   (format "Max interval in days (default %s): " max-def)
                   max-def)))
    (format-time-string
     (format "<%%Y-%%m-%%d %%a .+%sd/%sd>" ideal max))))

(defun ar/capture-dispatch ()
  "Hierarchical smart capture dispatcher.
Top-level keys:
[t] Task → inbox in todo.org    [n] New Denote note
[j] Journal (sub-menu)          [m] Meeting notes
[b] Book / article              [h] Habit (sub-menu)
[g] Goal (sub-menu)             [w] Web clip (sub-menu)
[i] Quick inbox with context    [q] Quit"
  (interactive)
  (let ((choice (read-char-choice
                 (concat "Capture: "
                         "[t]ask [n]ote [j]ournal [m]eeting [b]ook "
                         "[h]abit [g]oal [w]eb [i]nbox [q]uit: ")
                 '(?t ?n ?j ?m ?b ?h ?g ?w ?i ?q))))
    (pcase choice
      (?t (org-capture nil "i"))
      (?n (call-interactively #'denote))
      (?j (let ((kind (read-char-choice
                       "Journal: [d]aily  [l]ink+context  [q]uit: "
                       '(?d ?l ?q))))
            (pcase kind
              (?d (org-capture nil "jd"))
              (?l (org-capture nil "jl"))
              (?q (keyboard-quit)))))
      (?m (org-capture nil "m"))
      (?b (org-capture nil "b"))
      (?h (let ((kind (read-char-choice
                       "Habit: [d]aily flexible  [s]trict daily  [r]ange  [q]uit: "
                       '(?d ?s ?r ?q))))
            (pcase kind
              (?d (org-capture nil "hd"))
              (?s (org-capture nil "hs"))
              (?r (org-capture nil "hr"))
              (?q (keyboard-quit)))))
      (?g (let ((kind (read-char-choice
                       "Goal: [l]ife  [Q]uarterly  [w]eekly  [q]uit: "
                       '(?l ?Q ?w ?q))))
            (pcase kind
              (?l (org-capture nil "gl"))
              (?Q (org-capture nil "gQ"))
              (?w (org-capture nil "gw"))
              (?q (keyboard-quit)))))
      (?w (let ((kind (read-char-choice
                       "Web: [c]lipboard URL  [l]ink+context  [q]uit: "
                       '(?c ?l ?q))))
            (pcase kind
              (?c (org-capture nil "wc"))
              (?l (org-capture nil "wl"))
              (?q (keyboard-quit)))))
      (?i (org-capture nil "i"))
      (?q (keyboard-quit)))))

;; Capture template groups, appended into `org-capture-templates' below.
;; Journal templates are backed by denote-journal-capture state-aware date
;; routing; the Denote template spawns a full Denote note.

(defvar ar/capture-templates-journal
  `(("j" "Journal")
    ("jd" "Daily entry" entry
     (file denote-journal-capture-entry-today)
     ,(concat "* %(denote-journal-capture-timestamp) %?\n"
              ":PROPERTIES:\n"
              ":CREATED: %U\n"
              ":END:\n")
     :empty-lines 1 :kill-buffer t)
    ("jl" "Daily entry + context link" entry
     (file denote-journal-capture-entry-today)
     ,(concat "* %(denote-journal-capture-timestamp) %?\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":CONTEXT:  %a\n"
              ":END:\n")
     :empty-lines 1 :kill-buffer t))
  "Journal capture templates backed by denote-journal-capture.")

(defvar ar/capture-templates-meeting
  `(("m" "Meeting" entry
     (file+headline ,(expand-file-name "agenda/journal.org" my/org-directory)
                    "Meetings")
     ,(concat "* %^{Meeting title} :meeting:\n"
              ":PROPERTIES:\n"
              ":CREATED:    %U\n"
              ":ATTENDEES:  %^{Attendees}\n"
              ":LOCATION:   %^{Location|Remote}\n"
              ":END:\n"
              "\n"
              "** Agenda\n"
              "- %?\n"
              "\n"
              "** Notes\n"
              "\n"
              "** Action Items\n"
              "- [ ] \n"
              "\n"
              "** Follow-up\n")
     :empty-lines 1
     :clock-in t
     :clock-resume t))
  "Meeting capture template.")

(defvar ar/capture-templates-book
  `(("b" "Book / Article" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Someday")
     ,(concat "* TODO %^{Title} :read:\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":AUTHOR:   %^{Author}\n"
              ":TYPE:     %^{Type|Book|Article|Paper|Video|Podcast}\n"
              ":URL:      %^{URL or DOI|}\n"
              ":STATUS:   Unread\n"
              ":END:\n"
              "\n"
              "** Why this matters\n"
              "%?\n"
              "\n"
              "** Key ideas (fill after reading)\n")
     :empty-lines 1))
  "Book and article capture template.")

(defvar ar/capture-templates-habits
  `(("h" "Habit")
    ("hd" "Daily habit (flexible .+1d)" entry
     (file+headline ,(expand-file-name "agenda/habits.org" my/org-directory)
                    "Habits")
     ,(concat "* TODO %^{Habit name}\n"
              "SCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d>\")\n"
              ":PROPERTIES:\n"
              ":STYLE:    habit\n"
              ":END:\n")
     :empty-lines 1)
    ("hs" "Daily habit (strict ++1d)" entry
     (file+headline ,(expand-file-name "agenda/habits.org" my/org-directory)
                    "Habits")
     ,(concat "* TODO %^{Habit name}\n"
              "SCHEDULED: %(format-time-string \"<%Y-%m-%d %a ++1d>\")\n"
              ":PROPERTIES:\n"
              ":STYLE:    habit\n"
              ":END:\n")
     :empty-lines 1)
    ("hr" "Range habit (.+Xd/Yd)" entry
     (file+headline ,(expand-file-name "agenda/habits.org" my/org-directory)
                    "Habits")
     ,(concat "* TODO %^{Habit name}\n"
              "SCHEDULED: %(ar/capture-habit-range-schedule)\n"
              ":PROPERTIES:\n"
              ":STYLE:    habit\n"
              ":END:\n")
     :empty-lines 1))
  "Habit capture templates.")

(defvar ar/capture-templates-goals
  `(("g" "Goal")
    ("gl" "Life goal" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Projects")
     ,(concat "* TODO %^{Goal}\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":AREA:     %^{Area|Health|Career|Learning|Relationships|Finance|Creative}\n"
              ":END:\n"
              "\n"
              "** Why this matters\n"
              "%?\n"
              "\n"
              "** Success looks like\n"
              "\n"
              "** First next action\n"
              "- [ ] \n")
     :empty-lines 1)
    ("gQ" "Quarterly goal" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Projects")
     ,(concat "* TODO %^{Goal}\n"
              "DEADLINE: %^{Deadline}t\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":AREA:     %^{Area|Health|Career|Learning|Relationships|Finance|Creative}\n"
              ":END:\n"
              "\n"
              "** Outcome\n"
              "%?\n"
              "\n"
              "** Key milestones\n"
              "- [ ] \n"
              "\n"
              "** Weekly check-in notes\n")
     :empty-lines 1)
    ("gw" "Weekly intention" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Tasks")
     ,(concat "* TODO %^{Intention for the week}\n"
              "SCHEDULED: %t\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":END:\n"
              "\n"
              "%?\n")
     :empty-lines 1))
  "Goal capture templates.")

(defvar ar/capture-templates-web
  `(("w" "Web / Bookmark")
    ("wc" "Clipboard URL bookmark" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Someday")
     ,(concat "* %(ar/capture-url-as-org-link) :bookmark:\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":TAGS:     %^{Tags (space-separated)}\n"
              ":END:\n"
              "\n"
              "%?\n")
     :empty-lines 1
     :prepend t)
    ("wl" "Link to current Emacs location" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Someday")
     ,(concat "* %^{Description} :bookmark:context:\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":LINK:     %a\n"
              ":END:\n"
              "\n"
              "%?\n")
     :empty-lines 1
     :prepend t))
  "Web clip and bookmark capture templates.")

(defvar ar/capture-templates-inbox
  `(("i" "Quick inbox item (with context)" entry
     (file+headline ,(expand-file-name "agenda/todo.org" my/org-directory)
                    "Inbox")
     ,(concat "* TODO %?\n"
              ":PROPERTIES:\n"
              ":CREATED:  %U\n"
              ":CONTEXT:  %a\n"
              ":END:\n")
     :empty-lines 1))
  "Quick inbox capture template routed to agenda/todo.org.")

(defvar ar/capture-templates-denote
  '(("n" "Note (Denote)" plain
     (file denote-last-path)
     #'denote-org-capture
     :no-save t :immediate-finish nil :kill-buffer t :jump-to-captured t))
  "Denote note capture template.")

;; Wire the template groups into org-capture and route the *Capture* buffer
;; to a dedicated bottom drawer.

(use-package org-capture
  :defer t
  :after org
  :custom
  (org-capture-templates
   (append ar/capture-templates-journal
           ar/capture-templates-meeting
           ar/capture-templates-book
           ar/capture-templates-habits
           ar/capture-templates-goals
           ar/capture-templates-web
           ar/capture-templates-inbox
           ar/capture-templates-denote))
  :config
  ;; Doom Emacs Parity: Route the *Capture* buffer to a dedicated 33% bottom
  ;; drawer.  Isolates it from popper and winner-mode, providing a
  ;; distraction-free UI without the fragility of spawning dedicated
  ;; X11/Wayland child frames.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Capture\\*\\'\\|\\`CAPTURE-"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.33)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Save all org buffers after successful capture only (not on abort).
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (unless org-note-abort
                (org-save-all-org-buffers)))))


(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom
  ;; Stars: replace asterisks with decorative glyphs.
  (org-modern-star 'replace)
  (org-modern-replace-stars '("◉" "○" "✸" "✿" "✤" "✜" "◇" "▷"))
  (org-modern-hide-stars nil)
  ;; Checkboxes: map Org's three valid states to Unicode glyphs.
  (org-modern-checkbox '((?X . "☑") (?- . "◧") (?\s . "☐")))
  (org-modern-tag t)
  (org-modern-priority t)
  ;; Keywords and rules.
  (org-modern-keyword t)
  (org-modern-horizontal-rule t)
  (org-modern-block-keyword t)
  ;; Blocks: hide #+begin_/#+end_ prefix, show only replacement glyphs.
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
  ;; Lists: map ASCII bullet characters to Unicode glyphs.
  (org-modern-list '((43 . "▸") (45 . "–") (42 . "•")))
  ;; Tables: nil horizontal multiplier prevents fractional redisplay
  ;; stutter.
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal nil)
  ;; Disabled features.
  (org-modern-timestamp nil)
  (org-modern-progress nil)
  :config
  ;; Aborts activation in massive buffers to prevent font-lock overhead
  ;; (`too-long-file-p' is defined in the core module).
  (define-advice org-modern-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

(defun ar/org-src-simplify-buffer-name (_org-buffer-name lang)
  "Simplify Org source edit buffer names to `*Src LANG*'.
Strips the verbose parent file name to reduce mode-line clutter."
  (if (and (boundp 'org-src--source-type)
           (eq org-src--source-type 'example))
      "*Org Example*"
    (format "*Src %s*" (or lang "text"))))

(with-eval-after-load 'org-src
  (advice-add #'org-src--construct-edit-buffer-name
              :override #'ar/org-src-simplify-buffer-name))


(with-eval-after-load 'org
  ;; Structure Templates (org-tempo): required only after Org itself has
  ;; loaded, keeping this module load-deferred.
  (require 'org-tempo)
  (dolist (pair '(("sh" . "src shell")
                  ("el" . "src emacs-lisp")
                  ("py" . "src python")
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

  (defun ar/org--babel-lazy-load (lang)
    "Lazy load the babel package for LANG and register it."
    (cl-check-type lang (or symbol null))
    ;; Guard against nil languages to prevent `require' crashes during
    ;; export.
    (when lang
      (unless (cdr (assq lang org-babel-load-languages))
        (prog1 (or (require (intern (format "ob-%s" lang)) nil t)
                   (require lang nil t))
          ;; Update the registry so Org's execution engine recognizes it.
          (add-to-list 'org-babel-load-languages (cons lang t))))))

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
      ;; Use `assoc-string' to correctly match string keys in
      ;; `org-src-lang-modes'.
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


(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(defun ar/org-babel-remove-result-one-or-many (&optional arg)
  "Remove babel results.  With ARG, remove all results in buffer."
  (interactive "P")
  (org-babel-remove-result-one-or-many arg))

(with-eval-after-load 'org
  (defun ar/org-insert-block (type &optional modifier region-text)
    "Insert an Org block of TYPE with optional MODIFIER.
If REGION-TEXT is provided, wrap it inside the block."
    (let* ((origin (point))
           (beg-str (if modifier
                        (format "#+begin_%s %s\n" type modifier)
                      (format "#+begin_%s\n" type)))
           (end-str (format "#+end_%s\n" type)))
      (if region-text
          (insert beg-str region-text "\n" end-str)
        (insert beg-str "\n" end-str)
        (goto-char origin)
        (forward-line 1))))

  (defun ar/org-insert-perl-tangled ()
    "Insert a Perl source block configured for tangled execution."
    (interactive)
    (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
    (ar/org-insert-block "src" "perl"))

  (defun ar/org-insert-ipython ()
    "Insert an IPython session block."
    (interactive)
    (ar/org-insert-block "src" "ipython :session :exports both :results raw drawer"))

  (defvar ar/org-block-definitions
    '(;; Basic blocks
      ("a" "ascii"   "ascii"   nil)
      ("c" "center"  "center"  nil)
      ("C" "comment" "comment" nil)
      ("x" "example" "example" nil)
      ("E" "export"  "export"  nil)
      ("h" "html"    "html"    nil)
      ("l" "latex"   "latex"   nil)
      ("n" "note"    "note"    nil)
      ("o" "quote"   "quote"   nil)
      ("v" "verse"   "verse"   nil)
      ;; Head blocks
      ("i" "index"   "index"   nil)
      ("A" "ASCII"   "ASCII"   nil)
      ("I" "INCLUDE" "INCLUDE" nil)
      ("H" "HTML"    "HTML"    nil)
      ("L" "LaTeX"   "LaTeX"   nil)
      ;; Source blocks
      ("s" "src"        "src" nil)
      ("e" "emacs-lisp" "src" "emacs-lisp")
      ("y" "python"     "src" "python")
      ("w" "powershell" "src" "powershell")
      ("r" "ruby"       "src" "ruby")
      ("S" "sh"         "src" "sh")
      ("g" "golang"     "src" "go :imports '(\"fmt\")")
      ;; Misc blocks
      ("m" "mermaid"   "src" "mermaid :file chart.png")
      ("k" "kdl"       "src" "kdl")
      ("u" "plantuml"  "src" "plantuml :file chart.png")
      ("K" "kbd"       "src" "kbd")
      ("Z" "yaml"      "src" "yaml"))
    "List of (key description type modifier) for Org block generation.")

  ;; Generate named interactive commands to prevent Transient lambda bloat.
  (dolist (def ar/org-block-definitions)
    (let* ((key (nth 0 def))
           (desc (nth 1 def))
           (type (nth 2 def))
           (mod (nth 3 def))
           (cmd-name (intern (format "ar/org-block-%s-%s"
                                     key
                                     (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "-" desc))))))
      (defalias cmd-name
        `(lambda ()
           ,(format "Insert Org %s block." desc)
           (interactive)
           (let ((text (when (region-active-p)
                         (prog1 (buffer-substring-no-properties (region-beginning) (region-end))
                           (delete-region (region-beginning) (region-end))))))
             (ar/org-insert-block ,type ,mod text))))))

  (defun ar/org-smart-less-than ()
    "Smart less-than key: trigger transient menu or insert character."
    (interactive)
    ;; O(1) spatial guard: check if we are at the beginning of the line.
    (if (or (region-active-p)
            (save-excursion (skip-chars-backward " \t") (bolp)))
        (progn
          ;; `ar/org-template-transient' is defined once `transient' loads;
          ;; requiring it here keeps the very first `<' working even before
          ;; any other transient command has loaded the library.
          (require 'transient)
          (ar/org-template-transient))
      (self-insert-command 1)))

  (define-key org-mode-map (kbd "<") #'ar/org-smart-less-than))

(with-eval-after-load 'transient
  (defun ar/org-transient-title ()
    (concat
     (if (and (fboundp 'nerd-icons-sucicon) (display-graphic-p))
         (nerd-icons-sucicon "nf-custom-orgmode" :face 'transient-heading :v-adjust 0.02)
       "")
     (propertize " Org Template" 'face 'transient-heading)))

  (transient-define-prefix ar/org-template-transient ()
    "Org Mode Structure Templates"
    [:description ar/org-transient-title
     ["Basic"
      ("a" "ascii"   ar/org-block-a-ascii)
      ("c" "center"  ar/org-block-c-center)
      ("C" "comment" ar/org-block-C-comment)
      ("x" "example" ar/org-block-x-example)
      ("E" "export"  ar/org-block-E-export)
      ("h" "html"    ar/org-block-h-html)
      ("l" "latex"   ar/org-block-l-latex)
      ("n" "note"    ar/org-block-n-note)
      ("o" "quote"   ar/org-block-o-quote)
      ("v" "verse"   ar/org-block-v-verse)]
     ["Head"
      ("i" "index"   ar/org-block-i-index)
      ("A" "ASCII"   ar/org-block-A-ascii)
      ("I" "INCLUDE" ar/org-block-I-include)
      ("H" "HTML"    ar/org-block-H-html)
      ("L" "LaTeX"   ar/org-block-L-latex)]
     ["Source"
      ("s" "src"        ar/org-block-s-src)
      ("e" "emacs-lisp" ar/org-block-e-emacs-lisp)
      ("y" "python"     ar/org-block-y-python)
      ("w" "powershell" ar/org-block-w-powershell)
      ("r" "ruby"       ar/org-block-r-ruby)
      ("S" "sh"         ar/org-block-S-sh)
      ("g" "golang"     ar/org-block-g-golang)]
     ["Misc"
      ("m" "mermaid"      ar/org-block-m-mermaid)
      ("k" "kdl"          ar/org-block-k-kdl)
      ("u" "plantuml"     ar/org-block-u-plantuml)
      ("K" "kbd"          ar/org-block-K-kbd)
      ("Z" "yaml"         ar/org-block-Z-yaml)
      ("Y" "ipython"      ar/org-insert-ipython)
      ("P" "Perl tangled" ar/org-insert-perl-tangled)
      ("<" "insert <"     self-insert-command)]]))


(use-package org-block-wrap
  :hook (org-mode . org-block-wrap-mode))

(use-package markdown-mode
  :mode "\\.Rmd\\'")


;; Denote's generated autoloads cover only part of the entry matrix below
;; (verified against the installed packages: denote-autoloads.el registers
;; `denote-sort-dired' but `denote-dired' is a plain in-file defalias;
;; `denote-rename-file-keywords' has no cookie in denote.el;
;; denote-sequence-autoloads.el contains no `denote-sequence-reparent';
;; denote-menu-autoloads.el registers only `denote-menu-list-notes', while
;; `list-denotes' is an in-file alias).  Register the missing autoload
;; objects so the first keypress loads the right library instead of
;; signaling void-function; a later load of the real library replaces them.
(autoload #'denote-rename-file-keywords "denote" nil t)
(autoload #'denote-dired "denote" nil t)
(autoload #'denote-sequence-reparent "denote-sequence" nil t)
(autoload #'list-denotes "denote-menu" nil t)

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

  ;; Linking & Transclusion
  "n l" '(denote-link :wk "Insert link")
  "n i" '(denote-link-or-create :wk "Insert link or create")
  "n b" '(denote-backlinks :wk "Backlinks")
  "n T" '(org-transclusion-add :wk "Transclude note")

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

  ;; Explore & Analytics: statistics
  "n e"   '(:ignore t :wk "explore")
  "n e c" '(denote-explore-count-notes :wk "Count notes")
  "n e k" '(denote-explore-count-keywords :wk "Count keywords")
  "n e L" '(denote-explore-list-keywords :wk "List keywords")
  "n e f" '(denote-explore-barchart-filetypes :wk "Filetypes barchart")
  "n e w" '(denote-explore-barchart-keywords :wk "Keywords barchart")
  "n e t" '(denote-explore-barchart-timeline :wk "Timeline barchart")

  ;; Random Walks
  "n e r" '(denote-explore-random-note :wk "Random note")
  "n e x" '(denote-explore-random-regex :wk "Random regex")
  "n e l" '(denote-explore-random-link :wk "Random link")
  "n e K" '(denote-explore-random-keyword :wk "Random keyword")
  "n e S" '(denote-explore-random-signature :wk "Random signature")

  ;; Janitor
  "n e d" '(denote-explore-duplicate-notes :wk "Duplicate notes")
  "n e D" '(denote-explore-duplicate-notes-dired :wk "Duplicates via  Dired")
  "n e m" '(denote-explore-missing-links :wk "Missing links")
  "n e z" '(denote-explore-zero-keywords :wk "Zero keywords")
  "n e s" '(denote-explore-single-keywords :wk "Single keywords")
  "n e R" '(denote-explore-rename-keyword :wk "Rename keyword")
  "n e y" '(denote-explore-sync-metadata :wk "Sync metadata")
  "n e i" '(denote-explore-isolated-files :wk "Isolated files")

  ;; Network Graphs & Visualisation
  "n e n" '(denote-explore-network :wk "Generate network")
  "n e g" '(denote-explore-network-regenerate :wk "Regenerate network")
  "n e G" '(denote-explore-barchart-degree :wk "Degree barchart")
  "n e b" '(denote-explore-barchart-backlinks :wk "Backlinks barchart")

  ;; Wordcloud
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


(ar/global-leader
  "o"     '(:ignore t :wk "open")
  ;; Org capture and linking.
  "o c"   '(org-capture :wk "Capture")
  "o l"   '(org-store-link :wk "Store link")
  ;; Org agenda dispatchers and custom views.
  "o a"   '(:ignore t :wk "agenda")
  "o a a" '(org-agenda :wk "Agenda dashboard")
  "o a m" '(org-tags-view :wk "Tags search")
  "o a t" '(org-todo-list :wk "Global TODO list")
  "o a v" '(consult-org-heading :wk "Fuzzy heading jump")
  "o a d" '((lambda () (interactive) (org-agenda nil "d")) :wk "Daily dashboard")
  "o a w" '((lambda () (interactive) (org-agenda nil "w")) :wk "Weekly review")
  "o a e" '((lambda () (interactive) (org-agenda nil "e")) :wk "Effort-based")
  "o a A" '((lambda () (interactive) (org-agenda nil "A")) :wk "Accomplishments")
  "o a p" '((lambda () (interactive) (org-agenda nil "p")) :wk "Project status"))


(use-package universal-sidecar
  :defer t
  :commands (universal-sidecar-toggle
             universal-sidecar-insinuate
             universal-sidecar-uninsinuate
             universal-sidecar-refresh)
  :custom
  ;; Tie buffer name to the frame description.
  (universal-sidecar-buffer-name-format "*Sidecar: %F*")
  ;; Enable passive idle timer for content refresh.
  (universal-sidecar-enable-timer t)
  (universal-sidecar-refresh-time 5)
  :config
  ;; Route to a dedicated right-side window, isolated from Popper and
  ;; `C-x o'.
  (add-to-list 'display-buffer-alist
               '("\\*Sidecar:.*\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.25)
                 (window-parameters (no-delete-other-windows . t)
                                    (no-other-window . t))))
  ;; Advise core navigation functions to trigger sidecar updates.
  (universal-sidecar-insinuate))

(use-package denote-sections
  :defer t
  :after (universal-sidecar denote)
  :config
  ;; Append the backlinks section to preserve rendering order.
  (add-to-list 'universal-sidecar-sections 'denote-sections-backlinks-section t))


(when (locate-library "org-src-context")
  (with-eval-after-load 'org
    (require 'org-src-context)))

;; Everything after the bootstrap block above is registration and deferred
;; configuration.  init.el requires this module (as the feature
;; `org-config') after `core-config', `editor-config', `ui-config' and
;; `completion-config' (whose helpers and definers this module uses) and
;; before `dev-config', `vc-projects-config' and `keybinds-config'; the
;; `require' above loaded the real Org library, which is what provides
;; the feature `org'.

(provide 'org-config)
;;; org-config.el ends here
