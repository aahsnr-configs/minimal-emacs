;;; vc-projects-config.el --- Version control, projects, files, workspaces, terminal -*- lexical-binding: t; -*-

;;; Commentary:

;; Version control (magit, forge, blamer, git-link, git-timemachine),
;; project management (project.el + Projectile compat, per upstream Doom),
;; Dired/Dirvish, Treemacs, ibuffer, persp-mode workspaces, and the
;; ghostel terminal.  The diff gutter lives in ui-config (:ui vc-gutter).

;;; Code:

(defvar native-comp-jit-compilation-deny-list)

(declare-function git-timemachine-quit "git-timemachine")
(declare-function dired-virtual-revert "dired-x")
(declare-function dirvish-copy-file-true-path "dirvish")
(declare-function dirvish-copy-remote-path "dirvish")
(declare-function dirvish-emerge-next-group "dirvish")
(declare-function dirvish-emerge-previous-group "dirvish")
(declare-function dirvish-quit "dirvish")
(declare-function dirvish-layout-toggle "dirvish")
(declare-function ar/var-file "early-init")
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function projectile-project-root "projectile" (&optional dir))
(declare-function projectile-project-cache-file "projectile" (&optional project-root))
(defvar projectile-cache-file)
(defvar ediff-control-buffer)
(declare-function ediff-scroll-vertically "ediff")
(declare-function ediff-toggle-help "ediff")
(declare-function ar/git-timemachine-setup "vc-projects-config")
(declare-function ar/git-timemachine-teardown "vc-projects-config")
(declare-function ar/dirvish-transient "vc-projects-config")
(declare-function ar/dirvish--debounce-redisplay "vc-projects-config")
(declare-function ar/ediff-transient "vc-projects-config")
(declare-function ar/ediff-transient-active-p "vc-projects-config")
(declare-function ar/ediff-setup-keys "vc-projects-config")
(declare-function ar/ediff-save-window-config "vc-projects-config")
(declare-function ar/ediff-restore-window-config "vc-projects-config")

;;; Git porcelain (magit + forge)
(use-package magit
  :defer t
  :commands (magit-status magit-file-dispatch magit-blame)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-refresh-status-buffer t)
  (magit-refresh-verbose nil)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-ask-to-stage 'stage)
  (magit-commit-show-diff t)
  (magit-diff-refine-hunk 'all)
  (magit-diff-paint-whitespace t)
  (magit-diff-highlight-trailing t)
  (magit-revision-show-gravatars nil)
  :config
  (when (and (display-graphic-p)
             (image-type-available-p 'imagemagick))
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))))

(use-package forge
  :after magit
  :defer t
  :init
  (setq forge-add-default-bindings nil))

;; TODO injection into Magit: counters in the status buffer plus keyword
;; highlighting in log/revision washes (kept vanilla extra).
(use-package magit-todos
  :defer t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-exclude-globs
   '("*.map" "*.min.*" ".git/" "node_modules/" "dist/" "build/" "vendor/"))
  (magit-todos-depth 1)
  :config
  (add-hook 'magit-log-wash-summary-hook #'hl-todo-search-and-highlight t)
  (add-hook 'magit-revision-wash-message-hook #'hl-todo-search-and-highlight t))

;; code-review: review GitHub/GitLab PRs from Emacs (+forge).
;; Its db layer still targets the emacsql 3.x connection class (upstream
;; is dormant; forge/closql moved to emacsql 4.x), so alias the old class
;; onto the 4.x built-in sqlite backend before the package can load.
(use-package code-review
  :after (magit forge)
  :init
  ;; `find-class' lives in eieio-core, which Emacs 31.1 neither preloads
  ;; nor autoloads -- require it before probing the connection class.
  (require 'eieio-core)
  (unless (find-class 'emacsql-sqlite-connection nil)
    (require 'emacsql-sqlite-builtin)
    (defclass emacsql-sqlite-connection (emacsql-sqlite-builtin-connection) ()))
  :commands (code-review-forge-pr-at-point
             code-review-start))

;; orgit / orgit-forge: org links to magit buffers and forge topics.
(use-package orgit
  :after (magit org))

(use-package orgit-forge
  :after (orgit forge))

;; Time travel (git-timemachine).
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
    "Re-enable modes when exiting git-timemachine."
    (when (bound-and-true-p git-timemachine-mode)
      (when (bound-and-true-p ar/diff-hl-was-active)
        (diff-hl-mode 1)
        (kill-local-variable 'ar/diff-hl-was-active))
      (read-only-mode -1)))
  (add-hook 'git-timemachine-mode-hook #'ar/git-timemachine-setup)
  (advice-add #'git-timemachine-quit :after #'ar/git-timemachine-teardown))

;; git-link: copy URLs to lines & commits on forges (:emacs vc).
(use-package git-link
  :defer t
  :commands (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-open-in-browser nil))

;; git-modes: gitattributes/gitconfig/gitignore modes.
(use-package git-modes
  :defer t)

;;; Inline git blame (blamer) -- kept vanilla extra.
(use-package blamer
  :defer t
  :hook (prog-mode . blamer-mode)
  :custom
  (blamer-idle-time 0.5)
  (blamer-view 'overlay)
  (blamer-min-offset 40)
  (blamer-max-commit-message-length 80)
  (blamer-prettify-time-p t)
  :custom-face
  (blamer-face ((t (:inherit font-lock-comment-face :italic t)))))

(ar/define-large-file-guard blamer-mode)

;;; Version control dashboard transient and SPC g bindings
(defun ar/magit-stage-all ()
  "Stage all changes in the project (equivalent to `git add .')."
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
  "g H" '(diff-hl-show-hunk :wk "Preview hunk")
  "g y" '(git-link :wk "Copy remote URL"))

;;; Dired (built-in)
(use-package dired
  :ensure nil
  :defer t
  :commands (dired dired-jump dired-find-file dired-up-directory)
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
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Never auto-revert "virtual dired" buffers.
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

;;; Dirvish: dired with previews, sidebars and layouts (+dirvish +icons).
(use-package dirvish
  :after dired
  :custom
  (dirvish-cache-dir (ar/var-file "dirvish/"))
  (dirvish-default-layout '(1 0.11 0.55))
  (dirvish-layout-recipes
   '((0 0 0.4)
     (1 0.1 0.5)
     (1 0.15 0.6)))
  (dirvish-quick-access-entries
   `(("h" "~/" "Home")
     ("e" ,ar/config-dir "Emacs")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/Documents/" "Documents")
     ("p" "~/projects/" "Projects")
     ("o" ,(if (boundp 'ar/org-directory)
               ar/org-directory
             (expand-file-name "~/org/")) "Org")
     ("c" "~/.config/" "Config")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  (dirvish-attributes '(vc-state nerd-icons subtree-state collapse git-msg file-time file-size))
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
  (dirvish-reuse-session 'open)
  (dirvish-hide-cursor '(dirvish dirvish-side))
  (dirvish-hide-details '(dirvish dirvish-side))
  (dirvish-preview-dispatchers '(image gif video audio epub archive pdf dired))
  (dirvish-large-directory-threshold 20000)
  (dirvish-subtree-always-show-state t)
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
  ;; Debounce the redisplay hook to stop multi-frame flickering.
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

;; Bound below but absent from dirvish's generated autoloads: without these
;; the keys signal void-function until the extension happens to load.
(autoload #'dirvish-copy-file-true-path "dirvish-extras" nil t)
(autoload #'dirvish-copy-remote-path "dirvish-extras" nil t)
(autoload #'dirvish-emerge-next-group "dirvish-emerge" nil t)
(autoload #'dirvish-emerge-previous-group "dirvish-emerge" nil t)

;; Dirvish evil-style keymaps.
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
   "[e"  #'dirvish-emerge-previous-group
   "]e"  #'dirvish-emerge-next-group)
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

;;; Dired extensions (dired-x, diredfl, wdired).
(use-package dired-x
  :ensure nil
  :defer t
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
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
  (diredfl-dir-name ((t (:weight bold)))))

(use-package wdired
  :ensure nil
  :defer t
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;;; Treemacs project sidebar (+persp integration).
(use-package treemacs
  :defer t
  :commands (treemacs treemacs-find-file treemacs-select-window)
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (ar/var-file "treemacs/persist")
        treemacs-last-error-persist-file (ar/var-file "treemacs/persist-error"))
  :config
  (treemacs-follow-mode -1)
  (defcustom ar/treemacs-git-mode 'simple
    "Type of git integration for `treemacs-git-mode'."
    :type '(choice (const :tag "Only highlight files" simple)
                   (const :tag "Highlight files and directories" extended)
                   (const :tag "Same as extended, but async" deferred)
                   (const :tag "Disabled" nil))
    :group 'ar)
  (when ar/treemacs-git-mode
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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :config
  (treemacs-set-scope-type 'Perspectives))

;;; project.el (built-in): detection, ignores, switch menu, consult wiring.
(use-package project
  :ensure nil
  :defer t
  :custom
  (project-list-file (ar/etc-file "projects.el"))
  ;; Nested project boundaries in monorepos via manifest files.
  (project-vc-extra-root-markers '("package.json"
                                   "Cargo.toml"
                                   "pyproject.toml"
                                   "go.mod"
                                   "Makefile"
                                   ".project"
                                   ".projectile"))
  (project-vc-ignores '("node_modules/"
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
  (project-switch-use-entire-map t)
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-dired "Dired")
                             (project-eshell "Eshell")
                             (project-shell "Shell")
                             (consult-ripgrep "Ripgrep")
                             (consult-find "Find file (consult)")
                             (magit-project-status "Magit")
                             (ghostel-project "Ghostel")
                             (project-find-matching-buffer "Matching buffer")))
  (project-vc-cache-timeout '((nil . 5)))
  (project-vc-non-essential-cache-timeout '((nil . 30)))
  (project-prune-zombie-projects
   '((project-prompt-project-dir . file-directory-p)))
  (project-list-exclude '("/tmp/" "/dev/shm/"))
  :config
  ;; Wire project.el into consult (root extraction) and consult-dir.
  (with-eval-after-load 'consult
    (setq consult-project-function
          (lambda (may-prompt)
            (when-let* ((proj (project-current may-prompt)))
              (project-root proj)))))
  (with-eval-after-load 'consult-dir
    (setq consult-dir-project-list-function #'project-known-project-roots)))

;; Projectile compat layer (per upstream Doom core): layered on project.el,
;; consumed by ibuffer-projectile and treemacs-projectile.
(defun ar/projectile-project-root-from-project (dir)
  "Return the project.el root for DIR, or nil when it is not a project.
`projectile-mode' puts `project-projectile' on `project-find-functions',
which calls back into `projectile-project-root'; drop it for this lookup
or the two recurse until `max-lisp-eval-depth'."
  (let ((project-find-functions (remq #'project-projectile
                                      project-find-functions)))
    (when-let* ((project (project-current nil dir)))
      (project-root project))))

(defun ar/projectile-project-cache-file-a (original &optional project-root)
  "Store each Projectile project cache under `ar/var-dir'."
  (let* ((root (or project-root (projectile-project-root) default-directory))
         (name (file-name-nondirectory (directory-file-name root)))
         (projectile-cache-file
          (expand-file-name
           (format "%s-%s" name (secure-hash 'sha1 root))
           (ar/var-file "projectile/"))))
    (funcall original root)))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :after project
  :custom
  (projectile-project-search-path '("~/projects/" "~/work/"))
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching (if noninteractive t 'persistent))
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-ignored-projects '("~/"))
  (projectile-known-projects-file (ar/var-file "projectile/projects.eld"))
  ;; Let project.el define roots first, while retaining Projectile's local
  ;; override and fallback marker handling for compatibility integrations.
  (projectile-project-root-functions
   '(projectile-root-local
     ar/projectile-project-root-from-project
     projectile-root-marked
     projectile-root-bottom-up
     projectile-root-top-down
     projectile-root-top-down-recurring))
  :config
  (make-directory (ar/var-file "projectile/") t)
  (unless (advice-member-p #'ar/projectile-project-cache-file-a
                           'projectile-project-cache-file)
    (advice-add 'projectile-project-cache-file :around
                #'ar/projectile-project-cache-file-a))
  (projectile-mode +1))

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
  (interactive)
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
  "p r" '(project-query-replace-regexp :wk "Replace in project")
  "p i" '(project-forget-zombie-projects :wk "Invalidate cache")
  "p a" '(project-remember-project :wk "Add known project")
  "p R" '(ar/project-recentf :wk "Recent files in project"))

;;; Ibuffer dashboard (+icons): formats, faces, project grouping.
(use-package ibuffer
  :ensure nil
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold) :foreground "#bb9af7"))
  :custom-face
  (ibuffer-marked-face ((t (:foreground "#f7768e" :weight bold))))
  (ibuffer-deletion-face ((t (:foreground "#f7768e" :weight bold :strike-through t))))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (dolist (rx '("^\\*Completions\\*$"
                "^\\*Messages\\*$"
                "^\\*Async-native-compile-log\\*$"
                "^\\*Backtrace\\*$"
                "^\\*Warnings\\*$"
                "^\\*tramp/.*\\*$"
                "^\\*eldoc\\*$"))
    (add-to-list 'ibuffer-maybe-show-predicates rx))
  ;; Column layout: splice in the `vc-status' column when ibuffer-vc loads.
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
  (setq ibuffer-fontification-alist
        `((10 buffer-read-only font-lock-constant-face)
          (15 (and buffer-file-name (buffer-modified-p)) warning)
          (20 (string-prefix-p "*" (buffer-name)) shadow)
          (25 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          (30 (and (boundp 'auto-save-mode) auto-save-mode) font-lock-warning-face)))
  (add-to-list 'display-buffer-alist
               '("\\*Ibuffer\\*"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . visible))))

(use-package ibuffer-vc
  :defer t
  :after ibuffer)

(defun ar/ibuffer-project-filter-groups-h ()
  "Set projectile filter groups, loading `ibuffer-projectile' on demand.
Hooking the package's own function would miss the first ibuffer:
`ibuffer-projectile' loads after `projectile', which loads after the
lazy `project' -- the hook would not exist yet when ibuffer opens."
  (when (require 'ibuffer-projectile nil t)
    (ibuffer-projectile-set-filter-groups)))

(use-package ibuffer-projectile
  :defer t
  :after ibuffer
  :hook (ibuffer . ar/ibuffer-project-filter-groups-h))

;;; Workspaces: persp-mode (Doom :ui workspaces).
;; Isolated, persistent workspaces with per-perspective buffer lists and
;; window layouts, vim-tab-like switching, session persistence into var/.
(use-package persp-mode
  :demand t
  :init
  ;; Route persp state into the var tree; variable names verified against
  ;; persp-mode's own defcustoms during the smoke test.  Auto-resume stays
  ;; OFF: startup must never reopen files on its own -- the last session is
  ;; restored explicitly via the dashboard's "Reload last session" button
  ;; (`ar/dashboard-load-session').
  (setq persp-keymap-prefix (kbd "C-c w")
        persp-auto-save-opt 3
        persp-auto-resume-time 0.0
        persp-save-dir (ar/var-file "persp/")
        persp-auto-save-fname (ar/var-file "persp/persp-auto-save")
        persp-set-last-persp-for-new-frames t)
  :config
  ;; Batch sessions (harness, compile subprocesses) never activate persp:
  ;; they must neither resume nor write workspace state.
  (unless noninteractive
    (persp-mode 1)))

;; Workspace utilities and SPC TAB bindings (Doom-like keys over persp).
(defun ar/workspace-project-switch ()
  "Switch project and open it in a new workspace."
  (interactive)
  (let ((directory (project-prompt-project-dir)))
    (persp-switch (abbreviate-file-name (directory-file-name directory)))
    (project-switch-project directory)))

(defun ar/workspace-kill ()
  "Kill the current workspace."
  (interactive)
  (let ((persp (persp-get-current)))
    (when persp
      (persp-kill (persp-name persp)))))

(defun ar/workspace-kill-others ()
  "Kill every workspace except the current one."
  (interactive)
  (let ((current (persp-name (persp-get-current))))
    (dolist (name (persp-names))
      (unless (equal name current)
        (persp-kill name)))))

(defun ar/workspace-next ()
  "Switch to the next workspace."
  (interactive)
  (persp-next))

(defun ar/workspace-prev ()
  "Switch to the previous workspace."
  (interactive)
  (persp-prev))

(ar/global-leader
  "TAB"       '(:ignore t :wk "workspace")
  "TAB TAB"   '(persp-switch :wk "Switch/create workspace")
  "TAB n"     '(ar/workspace-next :wk "Next workspace")
  "TAB N"     '(persp-add-new :wk "New workspace")
  "TAB p"     '(ar/workspace-prev :wk "Previous workspace")
  "TAB r"     '(persp-rename :wk "Rename workspace")
  "TAB k"     '(ar/workspace-kill :wk "Kill workspace")
  "TAB K"     '(ar/workspace-kill-others :wk "Kill other workspaces")
  "TAB b"     '(persp-switch-to-buffer :wk "Switch to buffer (local)")
  "TAB B"     '(switch-to-buffer :wk "Switch buffer (global)")
  "TAB s"     '(persp-save-to-file-by-names :wk "Save workspace state")
  "TAB l"     '(persp-load-from-file-by-names :wk "Load workspace state")
  "TAB P s"   '(ar/workspace-project-switch :wk "Switch project"))

;;; Ediff (built-in): plain windows, window-config save/restore, transient.
(use-package ediff
  :ensure nil
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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-use-long-help-message nil)
  (ediff-highlight-all-diffs t)
  (ediff-keep-variants nil)
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
        ("v" "scroll down" ediff-scroll-vertically :transient t)
        ("V" "scroll up" (lambda () (interactive) (ediff-scroll-vertically '-)) :transient t)]
       ["Copy & Restore"
        ("a" "copy A → B" ediff-copy-A-to-B :transient t)
        ("b" "copy B → A" ediff-copy-B-to-A :transient t)
        ("r" "restore diff" ediff-restore-diff :transient t)]
       ["Toggle"
        ("h" "highlighting" ediff-toggle-hilit :transient t)
        ("@" "auto-refine" ediff-toggle-autorefine :transient t)
        ("|" "split H/V" ediff-toggle-split :transient t)
        ("m" "wide display" ediff-toggle-wide-display :transient t)]
       ["Actions"
        ("!" "update diffs" ediff-update-diffs :transient t)
        ("w" "save buffers" ediff-save-buffer :transient t)
        ("E" "manual" ediff-documentation :transient t)]
       ["Exit"
        ("?" "toggle help" ediff-toggle-help :transient t)
        ("q" "quit" ediff-quit)
        ("<escape>" "" transient-quit-one :format " ")]]))
  (defun ar/ediff-transient-active-p ()
    "Return non-nil if the ediff transient is currently active."
    (and (boundp 'transient--prefix)
         transient--prefix
         (eq transient-current-command 'ar/ediff-transient)))
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
                      (with-no-warnings (ar/ediff-transient)))))))
  (add-hook 'ediff-keymap-setup-hook #'ar/ediff-setup-keys))

(defun ar/ediff-focus-control-window ()
  "Select the ediff control buffer's window after setup."
  (select-window (get-buffer-window ediff-control-buffer)))

(add-hook 'ediff-startup-hook #'ar/ediff-focus-control-window)

;;; Woman (built-in): Arch manpath extraction, deferred to first use.
(use-package woman
  :ensure nil
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

;;; Ghostel terminal drawer (libghostty-vt) + Evil/Eshell/Compile glue.
(defun ar/ghostel-send-C-k-and-kill ()
  "Send `C-k' to ghostel and save the line to the `kill-ring'."
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (ghostel-send-key "k" "ctrl"))

(defun ar/ghostel-send-ctrl-p ()
  "Pass `C-p' through to the ghostel terminal."
  (interactive)
  (ghostel-send-key "p" "ctrl"))

(defun ar/ghostel-send-ctrl-n ()
  "Pass `C-n' through to the ghostel terminal."
  (interactive)
  (ghostel-send-key "n" "ctrl"))

(defun ar/ghostel-toggle ()
  "Toggle the ghostel terminal drawer."
  (interactive)
  (let* ((buf (seq-find (lambda (b)
                          (with-current-buffer b (derived-mode-p 'ghostel-mode)))
                        (buffer-list)))
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
  (add-to-list 'display-buffer-alist
               '((derived-mode . ghostel-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Whitelist magit for OSC 52 shell evaluation.
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  ;; Strip DIRENV_* variables so the spawned shell evaluates the local .envrc.
  (defun ar/ghostel-strip-direnv-h ()
    "Drop DIRENV_* from `process-environment' before ghostel spawns a shell."
    (setq process-environment
          (seq-remove (lambda (env) (string-prefix-p "DIRENV_" env))
                      process-environment)))
  (add-hook 'ghostel-pre-spawn-hook #'ar/ghostel-strip-direnv-h)
  (setq ghostel-tramp-shell-integration t))

;; Evil integration (+everywhere).
(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; Eshell visual commands render in ghostel buffers (+everywhere).
(use-package ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode)
  :config
  (unless (featurep 'ghostel)
    (require 'ghostel nil t)))

;; TTY-faithful rendering for compilation and comint buffers (+everywhere).
(defun ar/ghostel-compile-setup-h ()
  "Route compilation buffers through ghostel when it is available."
  (when (require 'ghostel nil t)
    (ghostel-compile-global-mode 1)))

(add-hook 'compilation-mode-hook #'ar/ghostel-compile-setup-h)

(with-eval-after-load 'comint
  (when (require 'ghostel nil t)
    (ghostel-comint-global-mode 1)))

;;; SPC o entries for this module's tools.
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
