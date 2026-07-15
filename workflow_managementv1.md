```org
** TODO Project Management
Provides a unified workspace engine for navigating, compiling, and managing code repositories. Replaces legacy external project managers with Emacs' native C-level project detection, augmented by an abstraction layer that exposes transient build menus and multi-target compilation.

=project= serves as the foundational, built-in workspace detector. It identifies repository boundaries, manages ignore patterns for massive monorepos, and routes project-scoped searches natively into the completion framework.
#+begin_src emacs-lisp
(use-package project
  :ensure nil
  :custom
  ;; Route known projects list to the no-littering etc directory.
  (project-list-file (no-littering-expand-etc-file-name "projects.el"))
  ;; Recognize nested project boundaries in monorepos via standard manifest files.
  (project-vc-extra-root-markers '("package.json"
                                   "Cargo.toml"
                                   "pyproject.toml"
                                   "go.mod"
                                   "Makefile"
                                   ".project"))
  ;; Massive ignore patterns to prevent indexing build artifacts and dependencies.
  (project-vc-ignores '("node_modules/"
                        ".venv/"
                        "venv/"
                        "__pycache__/"
                        "build/"
                        "dist/"
                        "target/"
                        ".next/"
                        ".cache/"
                        ".cask/"
                        ".elixir_ls/"
                        ".lsp/"))
  ;; Display the entire transient menu immediately upon invoking the prefix.
  (project-switch-use-entire-map t)
  ;; Enrich the native project dispatch menu with Doom-like actions.
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-dired "Dired")
                             (project-eshell "Eshell")
                             (project-shell "Shell")
                             (consult-ripgrep "Ripgrep")
                             (consult-find "Find file (consult)")
                             (magit-project-status "Magit")))
  :config
  ;; Wire native project.el into consult. Extracts root directory string safely,
  ;; bypassing the `project-current` instance object to prevent type mismatches.
  (with-eval-after-load 'consult
    (setq consult-project-function
          (lambda (may-prompt)
            (when-let ((proj (project-current may-prompt)))
              (project-root proj)))))
  ;; Wire project roots into consult-dir for spatial directory jumping.
  (with-eval-after-load 'consult-dir
    (setq consult-dir-project-list-function #'project-known-project-roots)))
#+end_src

=Custom Utilities= provide O(1) wrapper functions for rapid configuration navigation and frictionless workspace teardown, mirroring Doom Emacs muscle memory.
#+begin_src emacs-lisp
(defun ar/find-file-in-emacs-config ()
  "Instantly search the private Emacs configuration directory via `consult-find`.
Mirrors Doom Emacs `SPC f p` behavior for rapid config navigation."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively #'consult-find)))

(defun ar/project-kill-buffers ()
  "Kill project buffers without confirmation, mirroring Doom Emacs behavior.
Bypasses the interactive `yes-or-no-p` prompt natively via the NO-CONFIRM argument."
  (interactive)
  (project-kill-buffers t))
#+end_src

=projection= extends the native workspace detector with Projectile-like abstractions. It injects project-specific buffer hooks, exposes transient menus for build systems, and enables intelligent file jumping across diverse language ecosystems.
#+begin_src emacs-lisp
(use-package projection
  :defer t
  :after project
  :commands
  (projection-find-other-file
   projection-commands-build-project
   projection-commands-test-project
   projection-commands-run-project
   projection-commands-configure-project)
  :hook
  (compilation-mode . projection-customize-compilation-mode)
  :init
  ;; Mark projection build/test/run commands as safe for .dir-locals.el
  ;; to prevent Emacs from throwing "unsafe variable" security prompts.
  (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  (put 'projection-commands-install-project 'safe-local-variable #'stringp)
  :custom
  ;; Route compilation buffer naming to projection's project-aware function.
  (compilation-buffer-name-function
   #'projection-customize-compilation-buffer-name-function)
  :config
  ;; Enable project-specific buffer hooks (e.g., read-only for external deps).
  (global-projection-hook-mode 1))
#+end_src

=projection-multi= integrates multi-target compilation into the workspace engine. It automatically extracts available build, test, and run targets from Makefiles, CMake, and language-specific toolchains, presenting them in a unified selection menu.
#+begin_src emacs-lisp
(use-package projection-multi
  :defer t
  :after projection
  :commands
  (projection-multi-compile))
#+end_src

=projection-multi-embark= bridges the multi-target compilation engine with Embark actions. It allows interactive selection and immediate reassignment of compilation targets directly from the minibuffer candidate list.
#+begin_src emacs-lisp
(use-package projection-multi-embark
  :defer t
  :after (projection-multi embark)
  :config
  ;; Add the projection set-command bindings to the embark command map.
  (projection-multi-embark-setup-command-map))
#+end_src

=general.el= routes centralized leader keybindings for project operations, mirroring Doom Emacs mnemonics for spatial navigation, buffer switching, and workspace-scoped searches.
#+begin_src emacs-lisp
(ar/global-leader
  "f p" '(ar/find-file-in-emacs-config :wk "Find in config")
  "p"   '(:ignore t :wk "project")
  "p p" '(project-switch-project :wk "Switch project")
  "p f" '(project-find-file :wk "Find file")
  "p b" '(consult-project-buffer :wk "Project buffers")
  "p s" '(consult-ripgrep :wk "Search project")
  "p d" '(project-dired :wk "Dired root")
  "p e" '(project-eshell :wk "Eshell")
  "p S" '(project-shell :wk "Shell")
  "p k" '(ar/project-kill-buffers :wk "Kill buffers")
  "p o" '(projection-find-other-file :wk "Other file")
  "p c" '(projection-multi-compile :wk "Compile")
  "p C" '(projection-commands-build-project :wk "Build")
  "p t" '(projection-commands-test-project :wk "Test")
  "p r" '(projection-commands-run-project :wk "Run")
  "p g" '(projection-commands-configure-project :wk "Configure")
  "p i" '(project-forget-zombie-projects :wk "Invalidate cache")
  "p a" '(project-remember-project :wk "Add known project"))
#+end_src
```

```org
** TODO Workspaces
Provides mathematically strict buffer isolation per workflow context. Replaces legacy workspace managers with a headless native tab-bar engine augmented by an isolation layer that intercepts buffer creation and persists complex layouts across daemon restarts.

=tab-bar= serves as the foundational, built-in workspace container. It is configured headlessly to provide structural boundaries without rendering visual UI chrome, while spawning isolated scratch buffers for every new workspace.
#+begin_src emacs-lisp
(use-package tab-bar
  :ensure nil
  :custom
  ;; Hide UI chrome to maintain a minimalist aesthetic while preserving structural boundaries.
  (tab-bar-show nil)
  ;; Spawn a dedicated, isolated scratch buffer for every new workspace (Doom parity).
  (tab-bar-new-tab-choice #'bufferlo-create-local-scratch-buffer)
  :config
  ;; Activate the native tab engine globally.
  (tab-bar-mode 1)
  ;; Ensure new frames also receive an isolated local scratch buffer.
  (add-hook 'after-make-frame-functions #'bufferlo-switch-to-local-scratch-buffer))
#+end_src

=bufferlo= extends the native tab container with strict buffer isolation, workspace-aware minibuffer interception, and lightweight bookmark persistence.
#+begin_src emacs-lisp
(use-package bufferlo
  :after tab-bar
  :custom
  ;; CRITICAL: Must be set before `bufferlo-mode` activates.
  ;; Forces Emacs' C-level buffer cycling (next-buffer/previous-buffer) to respect strict tab boundaries.
  (bufferlo-prefer-local-buffers 'tabs)
  ;; Daemon-safe auto-save: flushes workspace layouts every 5 minutes to prevent data loss on SIGKILL/OOM.
  (bufferlo-bookmarks-auto-save-interval 300)
  ;; Persist workspace layouts across daemon restarts via native Emacs bookmarks.
  (bufferlo-bookmarks-save-at-emacs-exit 'all)
  (bufferlo-bookmarks-load-at-emacs-startup 'all)
  ;; Strict buffer filtering: exclude everything by default, whitelist core essentials.
  (bufferlo-exclude-buffer-filters '(".*"))
  (bufferlo-include-buffer-filters '("^\\*scratch\\*$"
                                     "^\\*Messages\\*$"))
  ;; Hide ephemeral noise and space-prefixed internal buffers from the local list.
  (bufferlo-hidden-buffers '("^\\*Completions\\*$"
                             "^\\*Flymake log\\*$"
                             "^\\*tramp/.*\\*$"
                             "^\\*eldoc\\*$"
                             "^ "))
  ;; Doom Parity: Protect unsaved code from accidental data loss, but aggressively kill ephemeral buffers.
  (bufferlo-kill-modified-buffers-policy 'retain-modified-kill-without-file-name)
  ;; Suppress confirmation prompts for frictionless workspace destruction.
  (bufferlo-close-tab-kill-buffers-prompt nil)
  (bufferlo-kill-buffers-prompt nil)
  ;; Prevent save-place-mode and bufferlo from fighting over cursor restoration.
  (bufferlo-bookmark-inhibit-bookmark-point t)
  ;; Spawn a placeholder buffer if a file was deleted externally, preventing workspace load crashes.
  (bufferlo-bookmark-tab-failed-buffer-policy 'placeholder)
  :config
  ;; Activate the core isolation engine.
  (bufferlo-mode 1)
  ;; Force all native `read-buffer` calls to respect workspace boundaries.
  (bufferlo-anywhere-mode 1)
  ;; Triad Integration: Wire bufferlo into consult-buffer to solve the "buffer soup" problem.
  (with-eval-after-load 'consult
    (setq consult-buffer-list-function #'bufferlo-local-buffers)))
#+end_src

=Custom Workspace Utilities= provide O(1) wrappers for Doom Emacs perspective muscle memory, bridging the gap between `persp-mode` semantics and `bufferlo`'s tab-bar physics.
#+begin_src emacs-lisp
(defun ar/workspace-set-buffer (buffer)
  "Switch to BUFFER and remove it from all other workspaces."
  (interactive (list (read-buffer "Set buffer (exclusive): ")))
  (let ((buf (get-buffer buffer)))
    (when buf
      (switch-to-buffer buf)
      ;; Iterate all frames and tabs to strip the buffer from other workspaces.
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
          (cl-incf killed))))
    (message "Killed %d local buffer(s)" killed)))

(defun ar/workspace-project-switch ()
  "Switch project and isolate workspace to the new project."
  (interactive)
  (call-interactively #'project-switch-project)
  (when (project-current)
    (bufferlo-isolate-project)))

(defun ar/workspace-project-kill ()
  "Kill project buffers and close the current workspace."
  (interactive)
  (let ((pr (project-current)))
    (when pr
      (project-kill-buffers t pr)))
  (bufferlo-tab-close-kill-buffers))
#+end_src

=general.el= routes centralized leader keybindings for workspace operations, mirroring legacy `persp-mode` mnemonics mapped strictly to `bufferlo` and `tab-bar` APIs.
#+begin_src emacs-lisp
(ar/global-leader
  "TAB"         '(:ignore t :wk "workspace")
  "TAB TAB"     '(tab-bar-switch-to-recent-tab :wk "Switch/last workspace")
  "TAB n"       '(tab-next :wk "Next workspace")
  "TAB p"       '(tab-previous :wk "Previous workspace")
  "TAB `"       '(tab-bar-switch-to-recent-tab :wk "Last workspace")
  "TAB r"       '(tab-bar-rename-tab :wk "Rename workspace")
  "TAB k"       '(bufferlo-tab-close-kill-buffers :wk "Kill workspace")
  "TAB K"       '(tab-bar-close-other-tabs :wk "Kill other workspaces")
  "TAB b"       '(bufferlo-switch-to-buffer :wk "Switch to buffer (local)")
  "TAB B"       '(switch-to-buffer :wk "Switch buffer (global)")
  "TAB a"       '(bufferlo-find-buffer-switch :wk "Import/Switch buffer")
  "TAB A"       '(ar/workspace-set-buffer :wk "Set buffer (exclusive)")
  "TAB x"       '(bufferlo-remove :wk "Remove buffer from workspace")
  "TAB X"       '(ar/workspace-kill-other-buffers :wk "Kill other local buffers")
  "TAB m"       '(bufferlo-ibuffer :wk "IBuffer (workspace-filtered)")
  "TAB s"       '(:ignore t :wk "state")
  "TAB s s"     '(bufferlo-bookmark-tab-save-current :wk "Save workspace state")
  "TAB s l"     '(bufferlo-bookmark-tab-load-current :wk "Load/restore workspace state")
  "TAB P"       '(:ignore t :wk "project")
  "TAB P s"     '(ar/workspace-project-switch :wk "Switch project + isolate")
  "TAB P p"     '(bufferlo-isolate-project :wk "Isolate workspace to project")
  "TAB P k"     '(ar/workspace-project-kill :wk "Kill project + workspace"))
#+end_src
```

### Required Excisions Outside Workflow Management

To maintain a single source of truth and permanently excise legacy `projectile`/`persp-mode` technical debt, apply the following `git diff` patches to the rest of `config.org`.

#### 1. Terminal (`ghostel`)

Remove the obsolete TODO comment. The `add-to-list` approach correctly appends Ghostel to the `project-switch-commands` alist defined in the Project Management subsection.

```diff
--- a/config.org
+++ b/config.org
@@ -1186,7 +1186,6 @@
 ;; Whitelist Magit for OSC 52;e shell evaluation.
 (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
-;; TODO: Integrate natively with projectile/persp-mode upon finalization.
 (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
 (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
```

#### 2. Dirvish

Permanently excise the commented-out `persp-before-switch-functions` hack. As verified by the `workflow_management.yaml` blueprint, `bufferlo` handles spatial isolation and window management natively without requiring manual side-window teardowns.

```diff
--- a/config.org
+++ b/config.org
@@ -2325,16 +2325,6 @@
 (funcall fn window)))
 (advice-add #'dirvish-pre-redisplay-h :around #'ar/dirvish--debounce-redisplay))
-;; TODO: modify this to work with bufferlo/projection
-;; DOOM HACK: Clean up Dirvish before switching workspaces/projects.
-;; If Dirvish is open in a dedicated side-window and you switch workspaces,
-;; Emacs will throw an error trying to delete the dedicated window.
-;; (add-hook 'persp-before-switch-functions
-;;           (lambda (&rest _)
-;;             (when-let* ((dv (cl-loop for w in (window-list)
-;;                                      if (window-dedicated-p w)
-;;                                      if (with-current-buffer (window-buffer w) (dirvish-curr))
-;;                                      return it)))
-;;               (let (dirvish-reuse-session)
-;;                 (with-selected-window (dv-root-window dv)
-;;                   (dirvish-quit))))))
 )
```

#### 3. Consult

Remove the commented-out `projectile` and `persp-mode` integration blocks. `project.el` and `bufferlo` now handle this natively via `consult-project-function` and `consult-buffer-list-function` in their respective subsections.

```diff
--- a/config.org
+++ b/config.org
@@ -3856,33 +3856,6 @@
 (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode)
 (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode))
-;; TODO: Replace with projectile/persp-mode alternatives
-;; ==========================================
-;; 2. STRICT PROJECTILE & PERSP-MODE INTEGRATION
-;; ==========================================
-;; Strict Projectile reliance: Ensure consult only uses Projectile for
-;; project root detection, ignoring Emacs' native project.el.
-;; `ignore-errors` ensures that if Projectile hasn't found a project root,
-;; it returns `nil` gracefully instead of throwing a `user-error`.
-;; (autoload 'projectile-project-root "projectile")
-;; (setq consult-project-function (lambda (_) (ignore-errors (projectile-project-root))))
-;; (with-eval-after-load 'persp-mode
-;;   ;; Define custom perspective buffer source for file-visiting buffers only
-;;   (defvar consult--source-persp-file-visiting-buffer
-;;     `(:name     "Persp Buffers"
-;;       :narrow   ?p
-;;       :category buffer
-;;       :face     consult-buffer
-;;       :history  buffer-name-history
-;;       :state    ,#'consult--buffer-state
-;;       :default  t
-;;       :items
-;;       ,(lambda ()
-;;          ;; SAFE PERSP CHECK: `persp-buffers` is a struct accessor and will crash if passed 'nil' (the global perspective). We guard against this.
-;;          (let ((persp (get-current-persp)))
-;;            (consult--buffer-query
-;;             :sort 'visibility
-;;             :predicate (lambda (buf)
-;;                          (and (buffer-file-name buf)
-;;                               (if persp
-;;                                   (memq buf (persp-buffers persp))
-;;                                 t))) ; Fallback for nil perspective
-;;             :as #'buffer-name))))
-;;     "Perspective buffer source showing only file-visiting buffers in current perspective.")
-;;   ;; Override default sources to use our custom persp source + recent files + bookmarks
-;;   (setq consult-buffer-sources
-;;         '(consult--source-persp-file-visiting-buffer
-;;           consult--source-recent-file
-;;           consult--source-bookmark
-;;           consult--source-hidden-buffer)))
 ;; ==========================================
 ;; 3. PREVIEW & NARROWING CONFIGURATION
 ;; ==========================================
```

#### 4. General Keybindings

Remove the commented-out `SPC p` and `SPC TAB` blocks at the bottom of the file to maintain a single source of truth in the Workflow Management section and prevent deferred-registration traps.

```diff
--- a/config.org
+++ b/config.org
@@ -5113,24 +5113,6 @@
 "f d" '(consult-dir :wk "Change directory")
 "f j" '(consult-dir-jump-file :wk "Jump to file in dir"))
-;; TODO: Replace with Projectile alternative
-;; Project operations (SPC p)
-;; (ar/global-leader
-;;   "p" '(:ignore t :wk "project")
-;;   "p p" '(ar/project-switch-to-file :wk "Switch project → find file")
-;;   "p P" '(ar/project-switch-with-menu :wk "Switch project (menu)")
-;;   "p f" '(projectile-find-file :wk "Find file in project")
-;;   "p s" '(consult-ripgrep :wk "Search project")
-;;   "p b" '(projectile-switch-to-buffer :wk "Project buffers")
-;;   "p k" '(projectile-kill-buffers :wk "Kill project buffers")
-;;   "p c" '(projectile-compile-project :wk "Compile project")
-;;   "p d" '(projectile-dired :wk "Dired project root")
-;;   "p e" '(projectile-run-eshell :wk "Eshell in project")
-;;   "p t" '(ar/treemacs-toggle :wk "Toggle treemacs (current project)")
-;;   "p i" '(projectile-invalidate-cache :wk "Invalidate cache")
-;;   "p r" '(projectile-replace :wk "Replace in project")
-;;   "p R" '(projectile-replace-regexp :wk "Replace regexp in project")
-;;   "p a" '(projectile-add-known-project :wk "Add known project"))
 ;; Toggle operations (SPC t)
 (ar/global-leader
@@ -5252,41 +5234,6 @@
 "n T a" '(org-table-align :wk "Align table")
 "n T r" '(org-table-recalculate :wk "Recalculate"))
-;; TODO Replace with persp-mode alternative
-;; (ar/global-leader
-;;   ;; Core perspective management (SPC TAB)
-;;   "TAB"     '(:ignore t :wk "perspective")
-;;   "TAB TAB" '(persp-switch :wk "Switch/create perspective")
-;;   "TAB n"   '(persp-next :wk "Next perspective")
-;;   "TAB p"   '(persp-prev :wk "Previous perspective")
-;;   "TAB `"   '(ar/persp-switch-last :wk "Last perspective")
-;;   "TAB r"   '(persp-rename :wk "Rename perspective")
-;;   "TAB k"   '(persp-kill :wk "Kill perspective")
-;;   "TAB K"   '(ar/persp-kill-others :wk "Kill other perspectives")
-;;   ;; Buffer management within perspectives
-;;   "TAB b"   '(persp-switch-to-buffer :wk "Switch to buffer")
-;;   "TAB B"   '(switch-to-buffer :wk "Switch buffer (all persp)")
-;;   "TAB a"   '(persp-add-buffer :wk "Add buffer")
-;;   "TAB A"   '(ar/persp-set-buffer :wk "Set buffer (remove from others)")
-;;   "TAB i"   '(persp-import-buffers :wk "Import buffer from perspective")
-;;   "TAB x"   '(persp-remove-buffer :wk "Remove buffer")
-;;   "TAB X"   '(ar/persp-kill-other-buffers :wk "Kill other buffers")
-;;   ;; IBuffer integration
-;;   "TAB m"   '(ar/persp-ibuffer :wk "IBuffer (perspective-filtered)")
-;;   ;; State management
-;;   "TAB s"   '(:ignore t :wk "state")
-;;   "TAB s s" '(persp-save-state-to-file :wk "Save state")
-;;   "TAB s l" '(persp-load-state-from-file :wk "Load/restore state")
-;;   ;; Project integration
-;;   "TAB P"   '(:ignore t :wk "project")
-;;   "TAB P s" '(ar/persp-project-switch :wk "Switch project + perspective")
-;;   "TAB P p" '(ar/persp-project-perspective :wk "Perspective for project")
-;;   "TAB P k" '(ar/persp-project-kill :wk "Kill project + perspective")
-;;   ;; Utilities
-;;   "TAB l"   '(ar/persp-list-all-buffers :wk "List all buffers/perspectives"))
 ;; Macro operations (SPC @)
 (ar/global-leader
```
