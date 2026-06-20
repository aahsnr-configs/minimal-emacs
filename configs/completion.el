(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot(styles orderless))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package vertico
  :custom
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode)

  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :init
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0 ; Always show absolute time
        marginalia-align 'right))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind (;; Buffer switching
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)

         ;; Navigation
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ([remap bookmark-jump] . consult-bookmark))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Improve register preview with consult
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (with-eval-after-load 'perspective
    ;; Hide the default consult buffer source
    (consult-customize consult-source-buffer :hidden t :default nil)

    ;; Define custom perspective buffer source for file-visiting buffers only
    (defvar consult--source-perspective-file-visiting-buffer
      `(:name     "Buffers"
                  :narrow   ?f
                  :category buffer
                  :face     consult-buffer
                  :history  buffer-name-history
                  :state    ,#'consult--buffer-state
                  :default  t
                  :items
                  ,(lambda ()
                     (let ((persp-buffers (persp-current-buffers)))
                       (consult--buffer-query
                        :sort 'visibility
                        :predicate (lambda (buf)
                                     (and (buffer-file-name buf)
                                          (memq buf persp-buffers)))
                        :as #'buffer-name))))
      "Perspective buffer source showing only file-visiting buffers in current perspective.")

    ;; Set consult-buffer-sources to use our custom source
    (setq consult-buffer-sources
          '(consult--source-perspective-file-visiting-buffer)))



  (consult-customize
   ;; Instant preview for line/imenu navigation
   consult-line consult-line-multi consult-imenu consult-imenu-multi
   :preview-key 'any

   ;; Slightly delayed preview for themes (smoother UI)
   consult-theme
   :preview-key '(:debounce 0.2 any)

   ;; Debounced preview for buffer/file operations
   consult-buffer consult-recent-file
   :preview-key '(:debounce 0.4 any)

   ;; Debounced preview for search operations
   consult-ripgrep consult-git-grep consult-grep consult-find
   :preview-key '(:debounce 0.4 any)

   ;; Other commands with moderate delay
   consult-goto-line consult-mark consult-global-mark
   consult-outline consult-bookmark
   :preview-key '(:debounce 0.4 any))

  ;; Configure narrowing
  (setq consult-narrow-key "<")

  ;; Make narrowing help available
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-dir :defer t)

(use-package consult-project-extra
  :defer t
  :custom (consult-project-function #'consult-project-extra-project-fn)
  :config
  ;; Frame-local project filtering (requires tabspaces)
  (with-eval-after-load 'tabspaces
    (defun ar/consult-project-filter (projects)
      "Filter PROJECTS to only show those in the current frame."
      (let ((frame-projects (ar/frame-projects)))
        (if (null frame-projects)
            projects
          (seq-filter (lambda (proj)
                        (member (expand-file-name proj) frame-projects))
                      projects))))

    (advice-add 'consult-project-extra--known-projects
                :filter-return
                #'ar/consult-project-filter))

  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package embark
  :commands embark-prefix-help-command
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Manual preview for non-Consult commands using Embark
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-no-warnings
    (with-eval-after-load 'which-key
      (defun embark-which-key-indicator ()
        "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))
             (if prefix
                 (pcase (lookup-key keymap prefix 'accept-default)
                   ((and (pred keymapp) km) km)
                   (_ (key-binding prefix 'accept-default)))
               keymap)
             nil nil t (lambda (binding)
                         (not (string-suffix-p "-argument" (cdr binding))))))))

      (setq embark-indicators
            '(embark-which-key-indicator
              embark-highlight-indicator
              embark-isearch-highlight-indicator))

      (defun embark-hide-which-key-indicator (fn &rest args)
        "Hide the which-key indicator immediately when using the completing-read prompter."
        (which-key--hide-popup-ignore-command)
        (let ((embark-indicators
               (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))

      (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator))))

(use-package embark-consult
  :defer t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-count 12)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'insert)
  (corfu-auto-delay 0.0)
  (global-corfu-modes '((not erc-mode
                             eshell-mode
                             circe-mode
                             help-mode
                             gud-mode
                             vterm-mode)
                        t))
  (read-extended-command-predicate #'command-completion-default-include-p)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
              ("TAB"       . corfu-insert)
              ("<tab>"     . corfu-insert)
              ("S-TAB"     . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("RET"       . corfu-insert)
              ("<escape>"  . corfu-quit))

  :config
  (corfu-history-mode 1)
  (add-hook 'before-save-hook #'corfu-quit)
  (advice-add #'persistent-scratch-save :before #'corfu-quit))

(use-package emacs
  :straight (:type built-in)
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))
#+end_src

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-tex)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)

  ;; NOTE: We do NOT add cape-elisp-block globally here.
  ;; We use the custom function below for Org mode instead.

  ;; -----------------------------------------------------------------------
  ;; 2. Robust Elisp Completion for Large Org Files (Narrowing Strategy)
  ;; -----------------------------------------------------------------------
  (defun ar/org-elisp-capf ()
    "A robust Capf for Elisp in Org mode.
    It identifies the block, narrows to the inner content (excluding headers),
    switches to Elisp syntax, and then completes."
    ;; Cheap check first to avoid overhead
    (when (org-in-src-block-p)
      (let ((element (org-element-at-point)))
        (when (and (eq (org-element-type element) 'src-block)
                   (member (org-element-property :language element)
                           '("elisp" "emacs-lisp")))
          (let* ((block-start (org-element-property :post-affiliated element))
                 (block-end (org-element-property :end element))
                 ;; Calculate inner start: Jump to header start, move down 1 line
                 (code-beg (save-excursion
                             (goto-char block-start)
                             (forward-line 1)
                             (point)))
                 ;; Calculate inner end: Jump to block end, move up 1 line
                 ;; (This excludes the #+end_src line)
                 (code-end (save-excursion
                             (goto-char block-end)
                             (forward-line -1)
                             (point))))
            ;; ROBUSTNESS CHECK 1: Ensure boundaries are valid numbers
            (when (and (numberp code-beg)
                       (numberp code-end)
                       (< code-beg code-end))
              ;; ROBUSTNESS CHECK 2: Ensure point is strictly inside the code.
              ;; If we are on #+begin_src, narrowing would fail/crash.
              ;; Returning nil lets Org handle header completion instead.
              (when (and (>= (point) code-beg)
                         (<= (point) code-end))
                (save-restriction
                  ;; Narrow to the code itself so the parser doesn't see Org headers
                  ;; and the syntax-ppss cache is reset for this region.
                  (narrow-to-region code-beg code-end)
                  (with-syntax-table emacs-lisp-mode-syntax-table
                    (elisp-completion-at-point))))))))))

  (defun ar/setup-org-cape-completion ()
    ;; Add to the VERY FRONT (-100) to bypass Org's native completion
    (add-hook 'completion-at-point-functions #'ar/org-elisp-capf -100 t))

  (add-hook 'org-mode-hook #'ar/setup-org-cape-completion)

  ;; -----------------------------------------------------------------------
  ;; 3. Advice Wrappers
  ;; -----------------------------------------------------------------------
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package dabbrev
  :straight (:type built-in)
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'vterm-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'eshell-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))
