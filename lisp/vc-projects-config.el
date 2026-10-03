;;; vc-projects-config.el --- Version control, projects, files, workspaces, terminal -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control (magit, forge, diff-hl, git-timemachine), project
;; management (projection), Dired/Dirvish, Treemacs, ibuffer, bufferlo
;; workspaces, and the ghostel terminal.

;;; Code:

;;; Git porcelain (magit + forge)
(use-package magit
  :defer t
  :custom
  ;; Display settings.
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; Performance.
  (magit-refresh-status-buffer t)
  (magit-refresh-verbose nil)
  ;; Behavior.
  (magit-save-repository-buffers 'dontask)
  (magit-commit-ask-to-stage 'stage)
  (magit-commit-show-diff t)
  ;; Diff settings.
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace t)
  (magit-diff-highlight-trailing t)
  ;; Gravatars are opt-in: enable only on graphical frames with imagemagick.
  (magit-revision-show-gravatars nil)
  :config
  (when (and (display-graphic-p)
             (image-type-available-p 'imagemagick))
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))))

(use-package forge
  :after magit
  :defer t
  :init
  ;; Forge binding slots stay free for user prefixes; invoke via `forge-dispatch'.
  (setq forge-add-default-bindings nil))

;; TODO injection into Magit: counters in the status buffer plus keyword
;; highlighting in the log/revision washes.  The hl-todo + consult-todo +
;; magit-todos trio is kept together by design §6.1 and the package is in
;; init.el's manifest -- without this block it installs but never activates.
(use-package magit-todos
  :defer t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  ;; Exclude build artifacts, dependencies, and minified files from scanning.
  (magit-todos-exclude-globs
   '("*.map" "*.min.*" ".git/" "node_modules/" "dist/" "build/" "vendor/"))
  ;; Restrict scanning to 1 level below repo root to prevent I/O latency in
  ;; massive monorepos.  Set to nil for unlimited depth.
  (magit-todos-depth 1)
  :config
  ;; Highlight TODO keywords in commit summaries and revision messages.
  (add-hook 'magit-log-wash-summary-hook #'hl-todo-search-and-highlight t)
  (add-hook 'magit-revision-wash-message-hook #'hl-todo-search-and-highlight t))

;;; VC gutter (diff-hl)
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  ;; Doom-style thin bitmaps: vertical bars for insert/change, a wedge for delete.
  (define-fringe-bitmap 'ar-diff-hl-bmp-insert [224] nil nil '(center repeated))
  (define-fringe-bitmap 'ar-diff-hl-bmp-modify [224] nil nil '(center repeated))
  (define-fringe-bitmap 'ar-diff-hl-bmp-delete [128 192 224 240] nil nil 'bottom)
  :custom
  (diff-hl-draw-borders nil)
  :config
  ;; Update the gutter while editing, not only on save/refresh.
  (diff-hl-flydiff-mode 1)
  ;; Force the custom thin bitmaps defined in :init.
  (setq diff-hl-fringe-bmp-function
        (lambda (type _pos)
          (cond
           ((eq type 'delete) 'ar-diff-hl-bmp-delete)
           ((eq type 'insert) 'ar-diff-hl-bmp-insert)
           ((eq type 'change) 'ar-diff-hl-bmp-modify)
           (t 'ar-diff-hl-bmp-insert))))
  ;; Clean foreground-only gutter colors (Tokyo Night palette; no background blocks).
  (unless noninteractive
    (custom-set-faces
     '(diff-hl-insert ((t (:foreground "#9ece6a" :background unspecified))))
     '(diff-hl-change ((t (:foreground "#e0af68" :background unspecified))))
     '(diff-hl-delete ((t (:foreground "#f7768e" :background unspecified)))))))

;;; Time travel (git-timemachine)
(use-package git-timemachine
  :defer t
  :commands (git-timemachine git-timemachine-toggle)
  :init
  ;; git-timemachine miscompiles under JIT native-comp; skip it.
  (with-eval-after-load 'comp
    (add-to-list 'native-comp-jit-compilation-deny-list "git-timemachine"))
  :custom
  (git-timemachine-abbreviation-length 7)
  (git-timemachine-show-minibuffer-details t)
  :config
  (defun ar/git-timemachine-setup ()
    "Disable interfering modes when entering git-timemachine."
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-mode -1)
      (setq-local ar/diff-hl-was-active t))
    (read-only-mode 1))
  (defun ar/git-timemachine-teardown ()
    "Re-enable modes when exiting git-timemachine.
Runs after `git-timemachine-quit', which kills the timemachine
buffer first; every step is a no-op unless the current buffer is
still a timemachine buffer, so the surviving buffer (normally the
file buffer the advice hands control back to) is never touched."
    (when (bound-and-true-p git-timemachine-mode)
      (when (bound-and-true-p ar/diff-hl-was-active)
        (diff-hl-mode 1)
        (kill-local-variable 'ar/diff-hl-was-active))
      (read-only-mode -1)))
  (add-hook 'git-timemachine-mode-hook #'ar/git-timemachine-setup)
  ;; git-timemachine defines no exit hook: `git-timemachine-mode' is a plain
  ;; minor mode (git-timemachine.el:413) and nothing in the source defines or
  ;; runs `git-timemachine-mode-exit-hook'.  Arm the teardown on the one real
  ;; exit path instead -- `git-timemachine-quit' (git-timemachine.el:336),
  ;; which `git-timemachine-toggle' also calls to turn itself off
  ;; (git-timemachine.el:474-479).
  (advice-add #'git-timemachine-quit :after #'ar/git-timemachine-teardown))

;;; Inline git blame (blamer)

;; GitLens-style inline blame (build/design-lsp.md §7).  Deferred: loads on the
;; first prog-mode buffer (same pattern as yasnippet in the dev module).
;; Values from the blamer README, quoted in research2/ecosystem-facts.md §5:
;; idle 0.5 ("Time before blame showing", README:109-110), view 'overlay
;; ("Blamer view", README:93-95).
(defun ar/blamer-maybe ()
  "Enable `blamer-mode' except in huge files.
Blame spawns a git process per buffer; skip minified/generated
trees (`too-long-file-p' comes from the core module, same guard
the dape inlay hints use)."
  (unless (too-long-file-p)
    (blamer-mode 1)))

(use-package blamer
  :defer t
  :hook (prog-mode . ar/blamer-maybe)
  :custom
  (blamer-idle-time 0.5)
  (blamer-view 'overlay)
  ;; Keep the right-margin text short and human ("2 days ago" formatting).
  (blamer-min-offset 40)
  (blamer-max-commit-message-length 80)
  (blamer-prettify-time-p t)
  :custom-face
  ;; Theme-independent: inherit the comment face instead of inventing a hex
  ;; (the dape face precedent hard-codes Tokyo Night; blame should follow the
  ;; active theme).
  (blamer-face ((t (:inherit font-lock-comment-face :italic t)))))

;;; Version control helpers, dashboard transient and SPC g bindings
(defun ar/magit-stage-all ()
  "Stage all changes in the project (equivalent to \\=`git add .\\=')."
  (interactive)
  (magit-run-git "add" ".")
  (message "Staged all changes."))

(with-eval-after-load 'transient
  (defun ar/vc-menu-title ()
    "Title line for `ar/toggles-vc-menu'."
    (concat
     (if (fboundp 'nerd-icons-octicon)
         (nerd-icons-octicon "nf-oct-git_branch" :face 'nerd-icons-green :v-adjust 0.02)
       "")
     (propertize " Version Control" 'face 'transient-heading)))

  (transient-define-prefix ar/toggles-vc-menu ()
    "Version Control Dashboard."
    [:description ar/vc-menu-title
     ["Magit Core"
      ("s" "Status" magit-status)
      ("a" "Stage All" ar/magit-stage-all)
      ("c" "Commit" magit-commit)
      ("p" "Push" magit-push-current-to-pushremote)
      ("P" "Pull" magit-pull-from-upstream)
      ("f" "Fetch" magit-fetch)
      ("F" "Fetch All" magit-fetch-all)]
     ["Branch & Log"
      ("b" "Branches" magit-branch)
      ("B" "Checkout" magit-checkout)
      ("m" "Merge" magit-merge)
      ("r" "Rebase" magit-rebase)
      ("l" "File Log" magit-log-buffer-file)
      ("L" "Branch Log" magit-log-current)]
     ["Diff & Stage"
      ("d" "Diff Unstaged" magit-diff-unstaged)
      ("D" "Diff Staged" magit-diff-staged)
      ("S" "Stage File" magit-stage-files :transient t)
      ("u" "Unstage File" magit-unstage-files :transient t)
      ("U" "Unstage All" magit-unstage-all :transient t)]
     ["Advanced"
      ("z" "Stash" magit-stash)
      ("Z" "Pop Stash" magit-stash-pop)
      ("x" "Reset" magit-reset)
      ("!" "Run Cmd" magit-run)
      ("R" "Refresh" magit-refresh :transient t)
      ("?" "Dispatch" magit-dispatch)
      ("<escape>" "" transient-quit-one :format " ")]]))

(ar/global-leader
  "g" '(:ignore t :wk "git")
  "g s" '(magit-status :wk "Status")
  "g c" '(magit-commit :wk "Commit")
  "g C" '(magit-commit-amend :wk "Commit amend")
  "g p" '(magit-push-current-to-pushremote :wk "Push")
  "g P" '(magit-pull-from-upstream :wk "Pull")
  "g b" '(magit-branch :wk "Branches")
  "g l" '(magit-log-buffer-file :wk "Log current file")
  "g L" '(magit-log-current :wk "Log current branch")
  "g f" '(magit-fetch :wk "Fetch")
  "g h" '(ar/toggles-vc-menu :wk "VC transient menu")
  "g m" '(magit-merge :wk "Merge")
  "g r" '(magit-rebase :wk "Rebase")
  "g t" '(git-timemachine-toggle :wk "Timemachine")
  "g n" '(diff-hl-next-hunk :wk "Next hunk")
  "g N" '(diff-hl-previous-hunk :wk "Previous hunk")
  "g S" '(diff-hl-stage-current-hunk :wk "Stage hunk")
  "g R" '(diff-hl-revert-hunk :wk "Revert hunk")
  "g H" '(diff-hl-show-hunk :wk "Preview hunk"))

;;; Dired (built-in)
(use-package dired
  :defer t
  :hook (dired-mode . hl-line-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask)
  (dired-clean-up-buffers-too t)
  (dired-isearch-filenames 'dwim)
  (dired-mouse-drag-files t)
  (dired-auto-revert-buffer (lambda (dir)
                              (and (not (file-remote-p dir))
                                   (dired-buffer-stale-p))))
  :config
  ;; GNU ls switches; `ar/dired-strip-unsupported-ls-flags' removes them over TRAMP.
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Never auto-revert "virtual dired" buffers (saved `ls -lR' listings, dired-x).
  (defun ar/dired--no-revert-in-virtual-buffers-p (&rest _)
    "Return nil in virtual Dired buffers so they are never auto-reverted."
    (not (eq revert-buffer-function #'dired-virtual-revert)))
  (advice-add #'dired-buffer-stale-p :before-while #'ar/dired--no-revert-in-virtual-buffers-p)
  ;; Minimal remote hosts often lack GNU ls: strip its extensions over TRAMP.
  (defun ar/dired-strip-unsupported-ls-flags ()
    "Strip GNU-specific ls flags over TRAMP to prevent blank remote listings."
    (when (file-remote-p default-directory)
      (let ((switches dired-actual-switches))
        (setq switches (replace-regexp-in-string "--group-directories-first" "" switches))
        (setq switches (replace-regexp-in-string "-v" "" switches))
        (setq switches (replace-regexp-in-string "  +" " " (string-trim switches)))
        (setq-local dired-actual-switches switches))))
  (add-hook 'dired-before-readin-hook #'ar/dired-strip-unsupported-ls-flags)
  ;; Open-with associations (xdg first, then known specialists).
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "xdg-open" "evince" "zathura")
          ("\\.\\(?:jpe?g\\|png\\|gif\\|svg\\|webp\\)\\'" "xdg-open" "imv")
          ("\\.\\(?:mp4\\|mkv\\|webm\\|mov\\)\\'" "xdg-open" "mpv")
          ("\\.\\(?:mp3\\|flac\\|wav\\|ogg\\)\\'" "xdg-open"))))

(defvar ar/config-dir) ;; the real configuration root, set by early-init.el

;;; Dirvish: dired with previews, sidebars and layouts
(use-package dirvish
  :defer t
  :custom
  ;; Cache under var/ instead of ~/.cache.
  (dirvish-cache-dir (ar/var-file "dirvish/"))
  ;; Layout: (parent-depth preview-width file-list-width).
  (dirvish-default-layout '(1 0.11 0.55))
  (dirvish-layout-recipes
   '((0 0 0.4)      ; no parent, smaller preview
     (1 0.1 0.5)    ; balanced
     (1 0.15 0.6))) ; more preview space
  ;; Quick access shortcuts for commonly visited directories.  The Org entry
  ;; falls back to ~/org/ if the org module's `my/org-directory' is not set.
  (dirvish-quick-access-entries
   `(("h" "~/" "Home")
     ("e" ,ar/config-dir "Emacs")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/Documents/" "Documents")
     ("p" "~/projects/" "Projects")
     ("o" ,(if (boundp 'my/org-directory)
               my/org-directory
             (expand-file-name "~/org/")) "Org")
     ("c" "~/.config/" "Config")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  ;; Visual attributes rendered in the main file list.
  (dirvish-attributes '(vc-state nerd-icons subtree-state collapse git-msg file-time file-size))
  ;; Stripped-down attributes for the narrow sidebar.
  (dirvish-side-attributes '(vc-state nerd-icons collapse file-size))
  (dirvish-use-header-line 'global)
  (dirvish-use-mode-line t)
  (dirvish-mode-line-bar-image-width 0)
  (dirvish-header-line-height '(25 . 35))
  (dirvish-mode-line-height 25)
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink)
     :right (omit yank index)))
  ;; Session behavior.
  (dirvish-reuse-session 'open)
  (dirvish-hide-cursor '(dirvish dirvish-side))
  (dirvish-hide-details '(dirvish dirvish-side))
  ;; Preview dispatchers for common file types.
  (dirvish-preview-dispatchers '(image gif video audio epub archive pdf dired))
  ;; Open huge directories asynchronously via `fd' (fd 10.5 is installed).
  (dirvish-large-directory-threshold 20000)
  (dirvish-subtree-always-show-state t)
  ;; Side window configuration.
  (dirvish-side-width 30)
  (dirvish-side-display-alist '((side . left) (slot . -1)))
  :config
  ;; Dirvish takes over every dired buffer (previews, headers, VC overlays).
  (dirvish-override-dired-mode)
  ;; TRAMP direct async: prevents Dirvish from freezing over synchronous SSH.
  (connection-local-set-profile-variables
   'remote-tramp-direct-async
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-tramp-direct-async)
  ;; Debounce the redisplay hook to stop multi-frame flickering (dirvish#353).
  (when (fboundp 'dirvish-pre-redisplay-h)
    (defun ar/dirvish--debounce-redisplay (fn window)
      "Run FN's redisplay hook only for the selected WINDOW."
      (when (eq (frame-selected-window) window)
        (funcall fn window)))
    (advice-add #'dirvish-pre-redisplay-h :around #'ar/dirvish--debounce-redisplay))
  ;; Unified transient dashboard replacing the default dispatch.
  (with-eval-after-load 'transient
    (defun ar/dirvish-title ()
      "Title line for `ar/dirvish-transient'."
      (concat
       (if (fboundp 'nerd-icons-mdicon)
           (nerd-icons-mdicon "nf-md-folder" :face 'transient-heading :v-adjust 0.02)
         "")
       (propertize " Dirvish Dashboard" 'face 'transient-heading)))
    (transient-define-prefix ar/dirvish-transient ()
      "Dirvish Dashboard."
      [:description ar/dirvish-title
       ["Navigation"
        ("h" "History Back" dirvish-history-go-backward :transient t)
        ("l" "History Forward" dirvish-history-go-forward :transient t)
        ;; dirvish 2.3.0 renamed the fd jump; the old `dirvish-fd-jump' binding
        ;; was dead (no such command in 2.3.0) - `dirvish-fd' is the command.
        ("j" "Jump (fd)" dirvish-fd)
        ("z" "History Jump" dirvish-history-jump)
        ("b" "Quick Access" dirvish-quick-access)]
       ["View & Layout"
        ("F" "Toggle Layout" dirvish-layout-toggle)
        ("S" "Quicksort" dirvish-quicksort)
        ("s" "Setup Menu" dirvish-setup-menu)
        ("n" "Narrow" dirvish-narrow)
        ("i" "File Info" dirvish-file-info-menu)
        ("e" "Emerge Groups" dirvish-emerge-menu)]
       ["Tools"
        ("y" "Yank Menu" dirvish-yank-menu)
        ("v" "VC Menu" dirvish-vc-menu)
        ("m" "Mark Menu" dirvish-mark-menu)
        ("r" "Rsync" dirvish-rsync)]
       ["Exit"
        ("q" "Quit Dirvish" dirvish-quit)
        ("<escape>" "" transient-quit-one :format " ")]])))

;;; Dirvish evil-style keymaps (bound once dirvish has loaded its maps)
(with-eval-after-load 'dirvish
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "?"   #'ar/dirvish-transient
   "q"   #'dirvish-quit
   "b"   #'dirvish-quick-access
   "p"   #'dirvish-yank
   "F"   #'dirvish-layout-toggle
   "z"   #'dirvish-history-jump
   "gh"  #'dirvish-subtree-up
   "gl"  #'dirvish-subtree-toggle
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   "TAB" #'dirvish-subtree-toggle)
  (general-define-key
   :states '(normal visual)
   :keymaps 'dirvish-mode-map
   "f"   #'dirvish-file-info-menu
   "S"   #'dirvish-quicksort
   "M-b" #'dirvish-history-go-backward
   "M-f" #'dirvish-history-go-forward
   "M-n" #'dirvish-narrow
   "M-m" #'dirvish-mark-menu
   "M-s" #'dirvish-setup-menu
   "M-e" #'dirvish-emerge-menu)
  (general-define-key
   :states 'motion
   :keymaps 'dirvish-mode-map
   "[h"  #'dirvish-history-go-backward
   "]h"  #'dirvish-history-go-forward
   "[e"  #'dirvish-emerge-next-group
   "]e"  #'dirvish-emerge-previous-group)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'dirvish-mode-map
   "<left>"  #'dired-up-directory
   "<right>" #'dired-find-file)
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   :prefix "y"
   "l"   #'dirvish-copy-file-true-path
   "n"   #'dirvish-copy-file-name
   "p"   #'dirvish-copy-file-path
   "r"   #'dirvish-copy-remote-path
   "y"   #'dired-do-copy)
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   :prefix "s"
   "s"   #'dirvish-symlink
   "S"   #'dirvish-relative-symlink
   "h"   #'dirvish-hardlink))

;;; Dired extensions (dired-x, diredfl, wdired)
(use-package dired-x
  :defer t
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  ;; Hide backups, autosaves, VCS internals and build artifacts.  Uses `\\=`'
  ;; (string-start) rather than `^' because dired matches isolated filenames.
  (dired-omit-files
   (concat "\\`[.]?#.*\\|\\`[.][.]?\\'"
           "\\|\\`\\.DS_Store\\'"
           "\\|\\`flycheck_.*"
           "\\|\\`\\.project\\(?:ile\\)?\\'"
           "\\|\\`\\.\\(?:svn\\|git\\)\\'"
           "\\|\\`\\.ccls-cache\\'"
           "\\|\\(?:\\.js\\)?\\.meta\\'"
           "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package diredfl
  :defer t
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  :custom-face
  ;; Bold directory names for visual distinction.
  (diredfl-dir-name ((t (:weight bold)))))

(use-package wdired
  :defer t
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;;; Treemacs project sidebar
(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (ar/var-file "treemacs/persist")
        treemacs-last-error-persist-file (ar/var-file "treemacs/persist-error"))
  :config
  ;; Follow mode is too jarring as a default.
  (treemacs-follow-mode -1)
  ;; Declarative git integration with a python3 fallback (python3 3.14 present).
  (defcustom ar/treemacs-git-mode 'simple
    "Type of git integration for `treemacs-git-mode'.
simple    Highlight only files.
extended  Highlight both files and directories (requires python).
deferred  Same as extended, but asynchronous.
nil       No git-based highlighting."
    :type '(choice (const :tag "Only highlight files" simple)
                   (const :tag "Highlight files and directories" extended)
                   (const :tag "Same as extended, but async" deferred)
                   (const :tag "Disabled" nil))
    :group 'treemacs)
  (when ar/treemacs-git-mode
    ;; Extended/deferred need python3; fall back to simple without it.
    (when (and (memq ar/treemacs-git-mode '(deferred extended))
               (not treemacs-python-executable)
               (not (executable-find "python3")))
      (setq ar/treemacs-git-mode 'simple))
    (treemacs-git-mode ar/treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq ar/treemacs-git-mode '(extended deferred))
              3
            0))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

;; Glue loads as soon as both of its anchors are loaded (old-config semantics).
(use-package treemacs-magit :after (treemacs magit))

(use-package treemacs-evil :after (treemacs evil))

;;; project.el (built-in): detection, ignores, switch menu, consult wiring
(use-package project
  :defer t
  :config
  ;; Known-projects list under etc/.
  (setopt project-list-file (ar/etc-file "projects.el"))
  ;; Nested project boundaries in monorepos via manifest files.
  (setopt project-vc-extra-root-markers '("package.json"
                                          "Cargo.toml"
                                          "pyproject.toml"
                                          "go.mod"
                                          "Makefile"
                                          ".project"))
  ;; Keep build artifacts and dependencies out of project file listings.
  (setopt project-vc-ignores '("node_modules/"
                               ".venv/"
                               "venv/"
                               "__pycache__/"
                               "build/"
                               ".direnv/"
                               "dist/"
                               "target/"
                               ".next/"
                               ".cache/"
                               ".cask/"
                               ".elixir_ls/"
                               ".lsp/"))
  ;; Show the full transient menu immediately on the switch prefix.
  (setopt project-switch-use-entire-map t)
  ;; Doom-like actions in the native dispatch menu (all autoloaded commands).
  (setopt project-switch-commands '((project-find-file "Find file")
                                    (project-find-regexp "Find regexp")
                                    (project-dired "Dired")
                                    (project-eshell "Eshell")
                                    (project-shell "Shell")
                                    (consult-ripgrep "Ripgrep")
                                    (consult-find "Find file (consult)")
                                    (magit-project-status "Magit")
                                    (ghostel-project "Ghostel")
                                    (ghostel-project-list-buffers "Ghostel buffers")
                                    (project-find-matching-buffer "Matching buffer")))
  ;; Emacs 31 VC-aware caching: short timeout for real work, longer for the
  ;; modeline; prune dead roots and skip transient directories.
  (setopt project-vc-cache-timeout '((nil . 5)))
  (setopt project-vc-non-essential-cache-timeout '((nil . 30)))
  (setopt project-prune-zombie-projects
          '((project-prompt-project-dir . file-directory-p)))
  (setopt project-list-exclude '("/tmp/" "/dev/shm/"))
  ;; Wire project.el into consult (root extraction) and consult-dir (jumping).
  (with-eval-after-load 'consult
    (setq consult-project-function
          (lambda (may-prompt)
            (when-let* ((proj (project-current may-prompt)))
              (project-root proj)))))
  (with-eval-after-load 'consult-dir
    (setq consult-dir-project-list-function #'project-known-project-roots)))

;;; Project utilities
(defun ar/find-file-in-emacs-config ()
  "Search the private Emacs configuration directory."
  (interactive)
  (let ((default-directory ar/config-dir))
    (call-interactively #'find-file)))

(defun ar/project-kill-buffers ()
  "Kill all buffers of the current project without confirmation."
  (interactive)
  (project-kill-buffers t))

(defun ar/find-file-in-other-project ()
  "Find a file in a different project without switching context."
  (interactive)
  (let* ((other-root (completing-read "Project: "
                                      (project-known-project-roots)
                                      nil t))
         (default-directory other-root))
    (call-interactively #'project-find-file)))

(defun ar/project-recentf ()
  "Show recently opened files scoped to the current project."
  (if-let* ((proj (project-current)))
      (let* ((root (project-root proj))
             (files (seq-filter
                     (lambda (f) (string-prefix-p root f))
                     recentf-list)))
        (if files
            (find-file (completing-read "Recent in project: " files nil t))
          (message "No recent files in this project.")))
    (user-error "Not inside a project")))

(ar/global-leader
  "f p" '(ar/find-file-in-emacs-config :wk "Find in config")
  "p"   '(:ignore t :wk "project")
  "p p" '(project-switch-project :wk "Switch project")
  "p f" '(project-find-file :wk "Find file")
  "p F" '(ar/find-file-in-other-project :wk "Find in other project")
  "p b" '(consult-project-buffer :wk "Project buffers")
  "p s" '(consult-ripgrep :wk "Search project")
  "p d" '(project-dired :wk "Dired root")
  "p e" '(project-eshell :wk "Eshell")
  "p S" '(project-shell :wk "Shell")
  "p k" '(ar/project-kill-buffers :wk "Kill buffers")
  "p o" '(projection-find-other-file :wk "Other file")
  "p C" '(projection-commands-build-project :wk "Build")
  "p t" '(projection-commands-test-project :wk "Test")
  "p r" '(project-query-replace-regexp :wk "Replace in project")
  "p u" '(projection-commands-run-project :wk "Run project")
  "p g" '(projection-commands-configure-project :wk "Configure")
  "p i" '(project-forget-zombie-projects :wk "Invalidate cache")
  "p a" '(project-remember-project :wk "Add known project")
  "p R" '(ar/project-recentf :wk "Recent files in project"))

;;; Projection: project-type-aware compile/test/run over project.el
;; projection-ibuffer is a feature inside the single `projection' package,
;; not a separate package.  (projection-multi / projection-dape exist in the
;; projection source but are excluded from the straight build by the MELPA
;; :files spec -- "src/*.el" does not match subdirectories -- so its
;; generated autoloads register neither and the commands don't resolve.
;; Hence the old SPC p c / SPC p D / SPC d P / SPC o d p compile-menu and
;; project-debug entries were removed instead of being bound to void
;; commands.  The feature list in build/design.md is stale on this point.)
(use-package projection
  :defer t
  :after project
  :commands (projection-find-other-file
             projection-commands-build-project
             projection-commands-test-project
             projection-commands-run-project
             projection-commands-configure-project
             projection-commands-package-project
             projection-commands-install-project)
  :hook (compilation-mode . projection-customize-compilation-mode)
  :init
  ;; Mark projection commands as safe in .dir-locals.el (no security prompts).
  (dolist (cmd '(projection-commands-configure-project
                 projection-commands-build-project
                 projection-commands-test-project
                 projection-commands-run-project
                 projection-commands-package-project
                 projection-commands-install-project))
    (put cmd 'safe-local-variable #'stringp))
  :config
  ;; Project-aware compilation buffer names and per-project hooks.
  (setopt compilation-buffer-name-function
          #'projection-customize-compilation-buffer-name-function)
  (global-projection-hook-mode 1))

;;; Ibuffer dashboard: formats, faces, icons, VC column, project grouping
(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :init
  ;; Filter-group headers inherit bold font-lock-string-face (Tokyo Night magenta).
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold) :foreground "#bb9af7"))
  :custom-face
  ;; Marked/deletion indicators in Tokyo Night red for bulk-operation feedback.
  (ibuffer-marked-face ((t (:foreground "#f7768e" :weight bold))))
  (ibuffer-deletion-face ((t (:foreground "#f7768e" :weight bold :strike-through t))))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  ;; `ibuffer-maybe-show-predicates' is chosen over the older
  ;; `ibuffer-never-show-predicates' (which still exists on Emacs 31.1 -- an
  ;; earlier comment here wrongly claimed 31 removed it): "maybe" entries
  ;; stay hidden but can be toggled visible with a C-u-prefixed ibuffer,
  ;; "never" entries cannot.  `add-to-list' keeps this idempotent and
  ;; preserves the built-in default.
  (dolist (rx '("^\\*Completions\\*$"
                "^\\*Messages\\*$"
                "^\\*Async-native-compile-log\\*$"
                "^\\*Backtrace\\*$"
                "^\\*Warnings\\*$"
                "^\\*tramp/.*\\*$"
                "^\\*eldoc\\*$"))
    (add-to-list 'ibuffer-maybe-show-predicates rx))
  ;; Column layout: splice in the `vc-status' column when ibuffer-vc loads and
  ;; keep the Emacs 28+ `locked' mark.
  (setq ibuffer-formats
        `((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 12 :left)))
                " "
                filename-and-process)
          (mark " "
                (name 16 -1)
                " "
                filename)))
  (setq ibuffer-default-shrink-to-minimum-size t)
  ;; Semantic fontification of buffer states (Tokyo Night parity).
  (setq ibuffer-fontification-alist
        `((10 buffer-read-only font-lock-constant-face)
          (15 (and buffer-file-name (buffer-modified-p)) warning)
          (20 (string-prefix-p "*" (buffer-name)) shadow)
          (25 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          (30 (and (boundp 'auto-save-mode) auto-save-mode) font-lock-warning-face)))
  ;; Anti-popper guard: reuse the current window instead of popping a new one.
  (add-to-list 'display-buffer-alist
               '("\\*Ibuffer\\*"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . visible))))

(use-package nerd-icons-ibuffer
  :defer t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer-vc :defer t)

;; projection-ibuffer: group buffers by project root, intersected with
;; bufferlo workspace boundaries.  Shipped inside the `projection' package;
;; the ibuffer-projection-* entry points are autoloaded, so the first
;; ibuffer invocation loads the feature (old-config behavior).
(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-projection-unset-filter-groups)
              (ibuffer-projection-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))
              (ibuffer-update nil t)))
  ;; Intersect filter groups with bufferlo's tab-local buffer list.
  (with-eval-after-load 'projection-ibuffer
    (define-advice projection-ibuffer--filter-groups (:around (fn) bufferlo-intersection)
      "Restrict filter groups to the current bufferlo workspace when active."
      (if (bound-and-true-p bufferlo-mode)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&rest _) (bufferlo-buffer-list))))
            (funcall fn))
        (funcall fn)))))

(ar/global-leader
  "b"   '(:ignore t :wk "buffer")
  "b b" '(consult-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "Ibuffer (workspace)")
  "b I" '(ibuffer-projection-current-project :wk "Ibuffer (single project)")
  "b K" '(kill-current-buffer :wk "Kill buffer")
  "b k" '(kill-buffer :wk "Kill buffer (choose)")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer-quick :wk "Revert buffer")
  "b R" '(rename-buffer :wk "Rename buffer")
  "b S" '(save-buffer :wk "Save buffer")
  "b z" '(bury-buffer :wk "Bury buffer"))

;;; Workspaces: headless tab-bar + bufferlo isolation
;; Loaded eagerly (not deferred) on purpose: bufferlo registers its startup
;; bookmark restore only when `bufferlo-mode' is enabled before
;; `after-init-time' is set (bufferlo.el: "unless after-init-time"), so
;; deferring the load would silently lose workspace restore across sessions.
;; Measured cost of the eager chain is ~13 ms batch-included (tab-bar ~1 ms;
;; the `require' of bufferlo ~11 ms) -- and bufferlo's top-level requires
;; pull seq/desktop/bookmark/frameset/ibuffer/ibuf-ext in uninvited, which
;; also defeats `use-package ibuffer :defer t' as a load boundary.  That is
;; the real, accepted cost; there is no way to take bufferlo's startup
;; bookmark restore without its require chain.
(use-package tab-bar
  :custom
  ;; Headless: structural boundaries without visible tab bar chrome.
  (tab-bar-show nil)
  ;; Every new workspace gets a dedicated local scratch buffer.
  (tab-bar-new-tab-choice #'bufferlo-create-local-scratch-buffer)
  :config
  (tab-bar-mode 1)
  ;; Isolated window-layout history per workspace (avoids winner-mode confusion).
  (tab-bar-history-mode 1)
  (add-hook 'after-make-frame-functions #'bufferlo-switch-to-local-scratch-buffer))

(use-package bufferlo
  :after tab-bar
  :custom
  ;; Must precede `bufferlo-mode': make C-level buffer cycling respect tabs.
  (bufferlo-prefer-local-buffers 'tabs)
  (bufferlo-mode-line nil)
  ;; Daemon-safe persistence: autosave every 5 min, save/restore at exit/startup.
  (bufferlo-bookmarks-auto-save-interval 300)
  (bufferlo-bookmarks-save-at-emacs-exit 'all)
  (bufferlo-bookmarks-load-at-emacs-startup 'all)
  ;; Exclude everything by default, whitelist the essentials.
  (bufferlo-exclude-buffer-filters '(".*"))
  (bufferlo-include-buffer-filters '("^\\*scratch\\*$"
                                     "^\\*Messages\\*$"))
  (bufferlo-hidden-buffers '("^\\*Completions\\*$"
                             "^\\*Flymake log\\*$"
                             "^\\*tramp/.*\\*$"
                             "^\\*eldoc\\*$"
                             "^\\*eldoc-childframe\\*$"
                             "^\\*Dirvish\\*$"
                             "^\\*Treemacs\\*$"
                             "^\\*dape-info.*\\*$"
                             "^ "))
  ;; Protect unsaved code; kill ephemeral buffers without prompts.
  (bufferlo-kill-modified-buffers-policy 'retain-modified-kill-without-file-name)
  (bufferlo-close-tab-kill-buffers-prompt nil)
  (bufferlo-kill-buffers-prompt nil)
  ;; Keep save-place and bufferlo from fighting over cursor restoration.
  (bufferlo-bookmark-inhibit-bookmark-point t)
  ;; Placeholder buffer when a file vanished: no crash on workspace load.
  (bufferlo-bookmark-tab-failed-buffer-policy 'placeholder)
  :config
  (bufferlo-mode 1)
  ;; Make native `read-buffer' calls respect workspace boundaries.
  (bufferlo-anywhere-mode 1)
  ;; Triad wiring: consult-buffer lists only workspace-local buffers.
  (with-eval-after-load 'consult
    (setq consult-buffer-list-function #'bufferlo-local-buffers))
  ;; Keep the file-manager and sidebar side-windows global, not per-workspace.
  (with-eval-after-load 'dirvish
    (add-to-list 'bufferlo-exclude-buffer-filters "^\\*Dirvish\\*$"))
  (with-eval-after-load 'treemacs
    (add-to-list 'bufferlo-exclude-buffer-filters "^\\*Treemacs\\*$")))

;;; Workspace utilities and SPC TAB bindings (bufferlo + tab-bar)
(defun ar/workspace-set-buffer (buffer)
  "Switch to BUFFER and remove it from every other workspace."
  (interactive (list (read-buffer "Set buffer (exclusive): ")))
  (let ((buf (get-buffer buffer)))
    (when buf
      (switch-to-buffer buf)
      (dolist (frame (frame-list))
        (dolist (tab (funcall tab-bar-tabs-function frame))
          (unless (eq tab (bufferlo--current-tab))
            (setf (alist-get 'buffer-list tab)
                  (delq buf (alist-get 'buffer-list tab)))
            (setf (alist-get 'buried-buffer-list tab)
                  (delq buf (alist-get 'buried-buffer-list tab)))))))))

(defun ar/workspace-kill-other-buffers ()
  "Kill all local buffers in the current workspace except the current one."
  (interactive)
  (let ((current (current-buffer))
        (killed 0))
    (dolist (buf (bufferlo-buffer-list))
      (unless (eq buf current)
        (when (kill-buffer buf)
          (setq killed (1+ killed)))))
    (message "Killed %d local buffer(s)" killed)))

(defun ar/workspace-project-switch ()
  "Switch project and isolate the workspace to the new project."
  (interactive)
  (call-interactively #'project-switch-project)
  (when (project-current)
    (bufferlo-isolate-project)))

(defun ar/workspace-project-kill ()
  "Kill the current project's buffers and close the workspace."
  (interactive)
  (let ((pr (project-current)))
    (when pr
      (project-kill-buffers t pr)))
  (bufferlo-tab-close-kill-buffers))

(ar/global-leader
  "TAB"     '(:ignore t :wk "workspace")
  "TAB TAB" '(tab-switcher :wk "Switch/create workspace")
  "TAB n"   '(tab-next :wk "Next workspace")
  "TAB N"   '(tab-new :wk "New workspace")
  "TAB p"   '(tab-previous :wk "Previous workspace")
  "TAB `"   '(tab-bar-switch-to-recent-tab :wk "Last workspace")
  "TAB r"   '(tab-bar-rename-tab :wk "Rename workspace")
  "TAB k"   '(bufferlo-tab-close-kill-buffers :wk "Kill workspace")
  "TAB K"   '(tab-bar-close-other-tabs :wk "Kill other workspaces")
  "TAB b"   '(bufferlo-switch-to-buffer :wk "Switch to buffer (local)")
  "TAB B"   '(switch-to-buffer :wk "Switch buffer (global)")
  "TAB a"   '(bufferlo-find-buffer-switch :wk "Add/import buffer")
  "TAB A"   '(ar/workspace-set-buffer :wk "Set buffer (exclusive)")
  "TAB x"   '(bufferlo-remove :wk "Remove buffer from workspace")
  "TAB X"   '(ar/workspace-kill-other-buffers :wk "Kill other local buffers")
  "TAB m"   '(bufferlo-ibuffer :wk "Ibuffer (workspace-filtered)")
  "TAB s"   '(:ignore t :wk "state")
  "TAB s s" '(bufferlo-bookmark-tab-save-current :wk "Save workspace state")
  "TAB s l" '(bufferlo-bookmark-tab-load-current :wk "Load/restore workspace state")
  "TAB P"   '(:ignore t :wk "project")
  "TAB P s" '(ar/workspace-project-switch :wk "Switch project + isolate")
  "TAB P p" '(bufferlo-isolate-project :wk "Isolate workspace to project")
  "TAB P k" '(ar/workspace-project-kill :wk "Kill project + workspace")
  "TAB l"   '(bufferlo-list-buffers :wk "List all local buffers"))

;;; Ediff (built-in): plain windows, window-config save/restore, transient
(use-package ediff
  :defer t
  :commands (ediff
             ediff-files
             ediff-buffers
             ediff-files3
             ediff-buffers3
             ediff-revision
             ediff-current-file
             ediff-merge
             ediff-merge-with-ancestor
             ediff-patch-file
             ediff-patch-buffer)
  :custom
  ;; Side-by-side layout inside the current frame, no separate control frame.
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-use-long-help-message nil)
  ;; Highlight all differences simultaneously.
  (ediff-highlight-all-diffs t)
  ;; Kill variant buffers on quit to prevent buffer bloat.
  (ediff-keep-variants nil)
  ;; Word-level refinement threshold for large diff regions.
  (ediff-auto-refine-limit 2000)
  :config
  (defvar ar/ediff-window-config nil
    "Window configuration saved before an ediff session.")
  (defun ar/ediff-save-window-config ()
    "Save current window configuration before ediff."
    (setq ar/ediff-window-config (current-window-configuration)))
  (defun ar/ediff-restore-window-config ()
    "Restore window configuration after ediff quits."
    (when ar/ediff-window-config
      (set-window-configuration ar/ediff-window-config)
      (setq ar/ediff-window-config nil)))
  (add-hook 'ediff-before-setup-hook #'ar/ediff-save-window-config)
  (add-hook 'ediff-quit-hook #'ar/ediff-restore-window-config)
  ;; Transient control panel, defined once transient is loaded.
  (with-eval-after-load 'transient
    (defun ar/ediff-title ()
      "Title line for `ar/ediff-transient'."
      (concat
       (if (fboundp 'nerd-icons-faicon)
           (nerd-icons-faicon "nf-fa-columns" :face 'transient-heading :v-adjust 0.02)
         "")
       (propertize " Ediff Control Panel" 'face 'transient-heading)))
    (transient-define-prefix ar/ediff-transient ()
      "Ediff commands - call from the control panel."
      [:description ar/ediff-title
       ["Navigate"
        ("n" "next diff" ediff-next-difference :transient t)
        ("p" "previous diff" ediff-previous-difference :transient t)
        ("d" "jump to diff" ediff-jump-to-difference :transient t)
        ("g" "goto point" ediff-jump-to-difference-at-point :transient t)
        ("v" "scroll down" ediff-scroll-vertically :transient t)
        ("V" "scroll up" (lambda () (interactive) (ediff-scroll-vertically '-)) :transient t)
        ("<" "scroll left" ediff-scroll-horizontally :transient t)
        (">" "scroll right" (lambda () (interactive) (ediff-scroll-horizontally '-)) :transient t)]
       ["Copy & Restore"
        ("a" "copy A → B" ediff-copy-A-to-B :transient t)
        ("b" "copy B → A" ediff-copy-B-to-A :transient t)
        ("r" "restore diff" ediff-restore-diff :transient t)
        ("*" "refine region" ediff-make-or-kill-fine-diffs :transient t)]
       ["Toggle"
        ("h" "highlighting" ediff-toggle-hilit :transient t)
        ("@" "auto-refine" ediff-toggle-autorefine :transient t)
        ("#" "skip similar" ediff-toggle-skip-similar :transient t)
        ("|" "split H/V" ediff-toggle-split :transient t)
        ("m" "wide display" ediff-toggle-wide-display :transient t)]
       ["Actions"
        ("!" "update diffs" ediff-update-diffs :transient t)
        ("w" "save buffers" ediff-save-buffer :transient t)
        ("i" "status info" ediff-status-info :transient t)
        ("E" "manual" ediff-documentation :transient t)]
       ["Exit"
        ("?" "toggle help" ediff-toggle-help :transient t)
        ("z" "suspend" ediff-suspend)
        ("q" "quit" ediff-quit)
        ("<escape>" "" transient-quit-one :format " ")]]))
  (defun ar/ediff-transient-active-p ()
    "Return non-nil if the ediff transient is currently active."
    (and (boundp 'transient--prefix)
         transient--prefix
         (eq transient-current-command 'ar/ediff-transient)))
  ;; Unbind SPC/C-c to prevent leader conflicts; route ? to the transient.
  (defun ar/ediff-setup-keys ()
    "Unbind conflicting keys and route ? to the transient menu."
    (define-key ediff-mode-map (kbd "SPC") nil)
    (define-key ediff-mode-map (kbd "C-c") nil)
    (when (featurep 'transient)
      (define-key ediff-mode-map (kbd "?")
                  (lambda ()
                    (interactive)
                    (if (ar/ediff-transient-active-p)
                        (ediff-toggle-help)
                      (ar/ediff-transient))))))
  (add-hook 'ediff-keymap-setup-hook #'ar/ediff-setup-keys)
  ;; Focus the control panel when a session starts.
  (add-hook 'ediff-startup-hook
            (lambda ()
              (select-window (get-buffer-window ediff-control-buffer)))))

;;; Woman (built-in): Arch manpath extraction, deferred to first use
(use-package woman
  :defer t
  :commands (woman)
  :config
  (let ((path (cond
               ((executable-find "manpath")
                (split-string (string-trim (shell-command-to-string "manpath -q"))
                              path-separator t))
               ((executable-find "man")
                (split-string (string-trim (shell-command-to-string "man --path"))
                              path-separator t)))))
    (when path (setq woman-manpath path))))

;;; Ghostel terminal drawer (libghostty-vt) + Evil/Eshell/Compile/Comint glue
(defun ar/ghostel-send-C-k-and-kill ()
  "Send \\=`C-k\\=' to ghostel and save the line to the `kill-ring'."
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (ghostel-send-key "k" "ctrl"))

(defun ar/ghostel-send-ctrl-p ()
  "Pass \\=`C-p\\=' through to the ghostel terminal."
  (interactive)
  (ghostel-send-key "p" "ctrl"))

(defun ar/ghostel-send-ctrl-n ()
  "Pass \\=`C-n\\=' through to the ghostel terminal."
  (interactive)
  (ghostel-send-key "n" "ctrl"))

(defun ar/ghostel-toggle ()
  "Toggle the ghostel terminal drawer; respect bufferlo workspaces."
  (interactive)
  (let* ((bufs (if (bound-and-true-p bufferlo-mode)
                   (bufferlo-buffer-list)
                 (buffer-list)))
         (buf (seq-find (lambda (b)
                          (with-current-buffer b (derived-mode-p 'ghostel-mode)))
                        bufs))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((and win (eq win (selected-window))) (delete-window win))
     (win (select-window win))
     (buf (pop-to-buffer buf))
     (t (call-interactively #'ghostel)))))

(use-package ghostel
  :defer t
  :commands (ghostel ghostel-project)
  :bind (:map ghostel-semi-char-mode-map
              ("C-s" . consult-line)
              ("C-k" . ar/ghostel-send-C-k-and-kill)
              ("M-p" . ar/ghostel-send-ctrl-p)
              ("M-n" . ar/ghostel-send-ctrl-n))
  :config
  ;; Route all ghostel buffers to a dedicated bottom side window.
  (add-to-list 'display-buffer-alist
               '((derived-mode . ghostel-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Whitelist magit for OSC 52 shell evaluation.
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  ;; Strip DIRENV_* variables so the spawned shell evaluates the local .envrc.
  (add-hook 'ghostel-pre-spawn-hook
            (lambda ()
              (setq process-environment
                    (seq-remove (lambda (env) (string-prefix-p "DIRENV_" env))
                                process-environment))))
  ;; Automatic TRAMP shell integration via temporary SCP transfers.
  (setq ghostel-tramp-shell-integration t))

;; Evil integration (evil-ghostel is its own melpa-stable package, 0.56.0).
(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; Eshell visual commands (vim, htop, less, ...) render in ghostel buffers.
(use-package ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; TTY-faithful rendering for compilation and comint buffers.  The old config
;; enabled these global modes on `after-init', which loaded ghostel at every
;; startup; now they activate on first use instead, so ghostel stays fully
;; deferred.  The compile trigger is deliberately `compilation-mode-hook',
;; NOT `with-eval-after-load' compile: core-config.el's `setopt' of
;; `compilation-ask-about-save' loads compile.el during startup (setopt's
;; custom-load-symbol pulls the defining library), so a compile.el load hook
;; would fire immediately and drag ghostel -- and with it its tramp, shell,
;; comint and project require chain -- right back into startup.  Enabling a
;; globalized minor mode from its first target buffer is safe: the enable
;; path applies the mode to existing buffers and repeated calls are
;; idempotent.  (comint still has no eager loader, so its load hook stays.)
(add-hook 'compilation-mode-hook (lambda () (ghostel-compile-global-mode 1)))

(with-eval-after-load 'comint
  (ghostel-comint-global-mode 1))

;;; SPC o entries for this module's tools (dirvish, treemacs, ghostel, eshell)
;; The `SPC o' prefix is shared with the org (agenda/capture), dev (debugger)
;; and keybinds (browser/frame/REPL) modules; only this module's tool
;; entries are bound here.
(ar/global-leader
  "o /" '(dirvish :wk "Dirvish")
  "o p" '(treemacs :wk "Toggle sidebar")
  "o P" '(treemacs-find-file :wk "Reveal in sidebar")
  "o t" '(ar/ghostel-toggle :wk "Toggle terminal drawer")
  "o T" '(ghostel :wk "Open terminal here")
  "o e" '(project-eshell :wk "Eshell (project)")
  "o E" '(eshell :wk "Eshell here"))

(provide 'vc-projects-config)
;;; vc-projects-config.el ends here
