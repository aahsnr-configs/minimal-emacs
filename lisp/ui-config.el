;;; ui-config.el --- Visual shell: theme, modeline, popup manager, dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; The visual shell: theme, modeline, fonts, nerd-icons, the folding
;; dispatcher, the ported Doom popup manager, the `*ahsan*' dashboard, and
;; the VC gutter.

;;; Code:

(declare-function too-long-file-p "core-config")
(declare-function set-fontset-font "fontset")

;; Activation guards for the deferred packages configured below.  A
;; negative or zero prefix argument still allows explicit toggling.
(ar/define-large-file-guard colorful-mode)
(ar/define-large-file-guard rainbow-delimiters-mode)
(ar/define-large-file-guard hs-minor-mode)
(ar/define-large-file-guard vimish-fold-mode)

(define-advice treesit-fold-mode (:before-while (&optional arg) guard-lisp-modes)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (derived-mode-p 'lisp-mode 'emacs-lisp-mode
                           'lisp-interaction-mode 'lisp-data-mode))))
(ar/define-large-file-guard treesit-fold-mode)

;; Theme.  Eagerly loaded so the first frame never paints unstyled.
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  ;; Patch the face alist BEFORE the theme engine evaluates it: Emacs 31
  ;; strictly forbids face inheritance cycles.
  (when (boundp 'doom-themes-base-faces)
    (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
            '(:inherit 'gnus-group-mail-1-empty :weight 'normal)))
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Solaire: swap background faces in side windows and popups.
(use-package solaire-mode
  :demand t
  :after doom-themes
  :hook ((change-major-mode ghostel-mode ghostel-compile-view-mode after-revert
          ediff-prepare-buffer org-capture-mode)
         . turn-on-solaire-mode)
  :config
  (add-to-list 'solaire-mode-remap-alist '(ghostel-default . solaire-default-face))
  (solaire-global-mode +1))

;; Mathematically corrected poshandler for near-bottom centering.
(defun ar/posframe-poshandler-frame-center-near-bottom (info)
  "Center INFO's posframe horizontally, positioned near the parent frame bottom."
  (let ((parent-width (plist-get info :parent-frame-width))
        (parent-height (plist-get info :parent-frame-height))
        (posframe-width (plist-get info :posframe-width))
        (posframe-height (plist-get info :posframe-height))
        (font-height (plist-get info :font-height)))
    (cons (/ (- parent-width posframe-width) 2)
          (- parent-height posframe-height (* 2 font-height)))))

;; Child frame base library.
(use-package posframe
  :defer t
  :custom-face
  (child-frame-border ((t (:inherit region))))
  :config
  (with-no-warnings
    (defun ar/posframe--isolate-fringe (&rest _)
      (when (and (boundp 'posframe--frame)
                 (frame-live-p posframe--frame))
        (set-face-background 'fringe nil posframe--frame)))
    (advice-add 'posframe--create-posframe :after #'ar/posframe--isolate-fringe)))

;; Icon engine.  Eagerly loaded: modeline, treemacs, corfu and transients
;; all dereference nerd-icons at their first paint.
(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-color-icons t))

;; Global typography.  Re-applied on daemon client frames.
(defun ar/setup-fonts ()
  "Apply typography, baseline faces, and fontset fallbacks to the frame."
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 125 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 125 :weight 'medium)
  ;; Variable-pitch is deliberately monospaced to keep Org tables aligned.
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height 125 :weight 'medium)
  (set-fontset-font t '(#x25A0 . #x25FF) "Noto Sans")
  (set-fontset-font t '(#x2600 . #x26FF) "Noto Sans")
  (set-fontset-font t '(#x2700 . #x27BF) "Noto Sans"))

(unless noninteractive
  (ar/setup-fonts))

(with-eval-after-load 'server
  (add-hook 'server-after-make-frame-hook #'ar/setup-fonts))

(setq font-lock-maximum-decoration t)

;; Modeline.  Demanded: part of the first paint.  The persp segment is
;; active because workspaces are owned by persp-mode.
(use-package doom-modeline
  :demand t
  :init
  (size-indication-mode -1)
  (setq-default mode-line-percent-position nil)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-window-width-limit 90)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-env-version t)
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-max-length 0)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-minor-modes nil)
  :config
  (doom-modeline-mode 1))

;; Hide modeline: native redisplay concealment for terminal, popup, and
;; agenda buffers.
(defvar-local ar/saved-mode-line-format nil "Cached modeline format.")
(defvar-local ar/saved-header-line-format nil "Cached header-line format.")
(defvar-local ar/saved-tab-line-format nil "Cached tab-line format.")

(define-minor-mode ar/hide-modeline-mode
  "Conceal the modeline, header-line, and tab-line in specific buffers."
  :init-value nil
  :lighter nil
  (if ar/hide-modeline-mode
      (progn
        (unless (eq mode-line-format nil)
          (setq-local ar/saved-mode-line-format mode-line-format))
        (unless (eq header-line-format nil)
          (setq-local ar/saved-header-line-format header-line-format))
        (unless (eq tab-line-format nil)
          (setq-local ar/saved-tab-line-format tab-line-format))
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil))
    (setq-local mode-line-format ar/saved-mode-line-format
                header-line-format ar/saved-header-line-format
                tab-line-format ar/saved-tab-line-format))
  (force-mode-line-update))

(defun ar/enable-hide-modeline ()
  "Safely enable `ar/hide-modeline-mode' without toggling."
  (ar/hide-modeline-mode 1))

(dolist (mode '(eshell-mode
                shell-mode
                term-mode
                embark-collect-mode
                ghostel-mode
                ghostel-compile-view-mode
                org-agenda-mode
                org-agenda-finalize))
  (add-hook (intern (format "%s-hook" mode)) #'ar/enable-hide-modeline))

;; Global zoom: face-remap based text scaling that must never resize frame
;; geometry on PGTK/Wayland.
(with-eval-after-load 'face-remap
  (setq global-text-scale-adjust-resizes-frames nil
        text-scale-mode-step 1.1))

(defun ar/global-zoom-in ()
  "Increase global text scale across all frames."
  (interactive)
  (global-text-scale-adjust 1))

(defun ar/global-zoom-out ()
  "Decrease global text scale across all frames."
  (interactive)
  (global-text-scale-adjust -1))

(defun ar/global-zoom-reset ()
  "Reset global text scale to default."
  (interactive)
  (global-text-scale-adjust 0))

(blink-cursor-mode -1)

;; Fill column indicator: confined to code and config buffers.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)

;; Color visualization: hex/RGB/named color previews via overlay stacking.
(use-package colorful-mode
  :defer t
  :custom
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :hook ((css-mode scss-mode less-css-mode html-mode
          org-mode markdown-mode gfm-mode prog-mode)
         . colorful-mode))

;; Rainbow delimiters: depth-based delimiter colorization in code buffers.
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; VSCode-style indent guides.  Stipples: PGTK builds support them from
;; Emacs 30; if they render wrong the documented fallback is
;; (setq indent-bars-prefer-character t).
(use-package indent-bars
  :defer t
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-scope '((python function_definition class_definition))))

(ar/define-large-file-guard indent-bars-mode)

;; Shared truncation ellipsis.
(setq truncate-string-ellipsis " ↴ ")

;; Unified folding dispatcher: routes to the best folding engine available
;; in the current buffer (treesit-fold -> outline -> hideshow -> vimish-fold).
(defun ar/fold-toggle ()
  "Toggle code fold at point using the best available folding engine."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-toggle))
        ((bound-and-true-p outline-minor-mode) (outline-cycle))
        ((bound-and-true-p hs-minor-mode)
         (if (fboundp 'hs-cycle) (hs-cycle) (hs-toggle-hiding)))
        ((fboundp 'vimish-fold-toggle)
         (condition-case nil (vimish-fold-toggle)
           (error (user-error "No foldable region at point"))))
        (t (user-error "No foldable region at point"))))

(defun ar/fold-open ()
  "Open code fold at point."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-open))
        ((bound-and-true-p outline-minor-mode) (outline-show-subtree))
        ((bound-and-true-p hs-minor-mode) (hs-show-block))
        ((fboundp 'vimish-fold-unfold) (vimish-fold-unfold))
        (t (user-error "No foldable region at point"))))

(defun ar/fold-close ()
  "Close code fold at point."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-close))
        ((bound-and-true-p outline-minor-mode) (outline-hide-subtree))
        ((bound-and-true-p hs-minor-mode) (hs-hide-block))
        (t (user-error "No foldable region at point"))))

(defun ar/fold-open-all ()
  "Open all code folds in the current buffer."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-open-all))
        ((bound-and-true-p outline-minor-mode) (outline-show-all))
        ((bound-and-true-p hs-minor-mode) (hs-show-all))
        ((fboundp 'vimish-fold-delete-all) (vimish-fold-delete-all)))
  (message "All folds opened"))

(defun ar/fold-close-all ()
  "Close all code folds in the current buffer."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-close-all))
        ((bound-and-true-p outline-minor-mode) (outline-hide-body))
        ((bound-and-true-p hs-minor-mode) (hs-hide-all)))
  (message "All folds closed"))

;; Hideshow: folds code in languages lacking tree-sitter grammars.
(use-package hideshow
  :defer t
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-show-block
             hs-hide-all
             hs-show-all
             hs-hide-level
             hs-cycle
             hs-toggle-all)
  :custom
  (hs-hide-comments-when-hiding-all nil)
  (hs-display-lines-hidden t)
  (hs-show-indicators t)
  (hs-indicator-type 'fringe))

;; Tree-sitter folding: language-aware region folding at the cursor.
(use-package treesit-fold
  :if (treesit-available-p)
  :demand t
  :custom
  (treesit-fold-summary-show t)
  (treesit-fold-line-count-show t)
  :config
  (global-treesit-fold-mode 1))

;; Manual folding: persistent region folds for the modes tree-sitter
;; folding does not reach, plus its evil integration.
(defvar evil-vimish-fold-mode-lighter)
(defvar evil-vimish-fold-target-modes)
(setq evil-vimish-fold-mode-lighter " ↴"
      evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))

(use-package vimish-fold
  :defer t
  :commands (vimish-fold-toggle
             vimish-fold-unfold
             vimish-fold-delete
             vimish-fold-delete-all)
  :custom
  (vimish-fold-dir (ar/var-file "vimish-fold/"))
  (vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode 1))

(use-package evil-vimish-fold
  :after vimish-fold
  :commands (evil-vimish-fold/create evil-vimish-fold/create-line)
  :config
  (global-evil-vimish-fold-mode 1))

(autoload #'evil-vimish-fold/create "evil-vimish-fold" nil t)
(autoload #'evil-vimish-fold/create-line "evil-vimish-fold" nil t)

;; The z-family folding bindings (evil motion state).
(general-define-key
 :states 'motion
 "za" #'ar/fold-toggle
 "zo" #'ar/fold-open
 "zc" #'ar/fold-close
 "zR" #'ar/fold-open-all
 "zM" #'ar/fold-close-all
 "zf" #'evil-vimish-fold/create
 "zF" #'evil-vimish-fold/create-line
 "zd" #'vimish-fold-delete
 "zE" #'vimish-fold-delete-all)

;; TODO keyword highlighting with Tokyo-Night keyword faces (:ui hl-todo).
(use-package hl-todo
  :demand t
  :bind (:map hl-todo-mode-map
              ("C-c n" . hl-todo-next)
              ("C-c p" . hl-todo-previous)
              ("C-c i" . hl-todo-insert))
  :custom-face
  (hl-todo ((t (:inherit font-lock-keyword-face :weight bold))))
  :custom
  (hl-todo-require-punctuation nil)
  (hl-todo-highlight-punctuation ":!")
  (hl-todo-keyword-faces
   '(("TODO"       . "#e0af68")
     ("COMMENT"    . "#e0af68")
     ("FIXME"      . "#f7768e")
     ("BUG"        . "#f7768e")
     ("ISSUE"      . "#f7768e")
     ("PROG"       . "#7aa2f7")
     ("TRICK"      . "#ff9e64")
     ("WORKAROUND" . "#ff9e64")
     ("BENCHMARK"  . "#9ece6a")
     ("REVIEW"     . "#7dcfff")
     ("NOTE"       . "#bb9af7")
     ("DEPRECATED" . "#73daca")
     ("HACK"       . "#db4b4b")
     ("DEBUG"      . "#e0af68")
     ("STUB"       . "#e0af68")))
  :config
  (global-hl-todo-mode 1)
  (with-eval-after-load 'consult
    (when (boundp 'consult-preview-allowed-hooks)
      (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode)
      (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode)))
  (defun ar/hl-todo-rg-project ()
    "Search for TODO keywords across the current project using ripgrep."
    (interactive)
    (let ((regexp (concat "\\b(" (string-join (mapcar #'car hl-todo-keyword-faces) "|") ")\\b")))
      (cond
       ((fboundp 'consult-ripgrep)
        (consult-ripgrep nil regexp))
       (t
        (user-error "consult-ripgrep is not available"))))))

;; VC gutter (:ui vc-gutter +pretty): thin Doom-style fringe bitmaps.
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (define-fringe-bitmap 'ar-diff-hl-bmp-insert [224] nil nil '(center repeated))
  (define-fringe-bitmap 'ar-diff-hl-bmp-modify [224] nil nil '(center repeated))
  (define-fringe-bitmap 'ar-diff-hl-bmp-delete [128 192 224 240] nil nil 'bottom)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (diff-hl-flydiff-mode 1)
  (setq diff-hl-fringe-bmp-function
        (lambda (type _pos)
          (cond
           ((eq type 'delete) 'ar-diff-hl-bmp-delete)
           ((eq type 'insert) 'ar-diff-hl-bmp-insert)
           ((eq type 'change) 'ar-diff-hl-bmp-modify)
           (t 'ar-diff-hl-bmp-insert))))
  (unless noninteractive
    (custom-set-faces
     '(diff-hl-insert ((t (:foreground "#9ece6a" :background unspecified))))
     '(diff-hl-change ((t (:foreground "#e0af68" :background unspecified))))
     '(diff-hl-delete ((t (:foreground "#f7768e" :background unspecified)))))))


;;;
;;; The Doom popup manager, ported to vanilla Emacs.
;;;
;;; A rule-based system for temporary windows: transient buffers spawn as
;;; side/bottom popups, are dismissed with ESC/C-g, are restorable, and
;;; clean up after themselves.  Ported from :ui popup (no popper).

(defgroup ar/popup nil
  "The rule-based popup window manager (Doom :ui popup port)."
  :group 'convenience)

(defcustom ar/popup--rules
  '(;; Core output / transient UI
    ("^\\*Warnings\\*$"        :size 0.15 :select nil :quit t)
    ("^\\*Backtrace\\*$"       :size 0.30 :select t   :quit t)
    ("^\\*Compile-Log\\*$"     :size 0.15 :select nil :quit t)
    ("^\\*Messages\\*$"        :size 0.35 :select t   :quit t)
    ("^\\*Native-compile-Log\\*$" :size 0.15 :select nil :quit t)
    ("^\\*Completions\\*$"     :size 0.20 :select nil :quit t)
    ("^\\*Async Shell Command\\*$" :size 0.25 :select nil :quit t)
    ("^\\*Shell Command Output\\*$" :size 0.25 :select nil :quit t)
    ("^\\*Pp .*Output\\*$"     :size 0.30 :select t :quit t)
    ("^\\*transient"           :size 0.35 :select t :quit t)
    ("^\\*envrc"               :size 0.20 :select nil :quit t)
    ("^\\*Local variables\\*$" :size 0.20 :select nil :quit t)
    ;; Help / info / docs
    (help-mode                 :size 0.40 :select t   :quit t)
    (helpful-mode              :size 0.40 :select t   :quit t)
    (Info-mode                 :size 0.45 :select t   :quit t)
    (Custom-mode               :size 0.40 :select t   :quit t)
    (woman-mode                :size 0.45 :select t   :quit t)
    (Man-mode                  :size 0.45 :select t   :quit t)
    (bookmark-bmenu-mode       :size 0.35 :select t   :quit t)
    ;; Diagnostics & navigation results
    ("^\\*xref\\*$"            :size 0.35 :select t   :quit t)
    ("^\\*Occur\\*$"           :size 0.35 :select t   :quit t)
    (grep-mode                 :size 0.35 :select t   :quit t)
    (occur-mode                :size 0.35 :select t   :quit t)
    (flymake-diagnostics-buffer-mode :size 0.35 :select t :quit t)
    (flymake-project-diagnostics-mode :size 0.35 :select t :quit t)
    ;; Completion/navigations
    ("^\\*Embark \\(Collect\\|Live\\|Export\\):.*\\*$" :size 0.35 :select t :quit t)
    ;; Org
    ("^\\*Org Select\\*$"      :size 0.30 :select t :quit t)
    ("^\\*Org-Babel Error Output\\*$" :size 0.30 :select nil :quit t)
    ("^\\*Org Export Dispatcher\\*$"  :size 0.30 :select t :quit t)
    ("^\\*Org Note\\*$"        :size 0.30 :select t :quit t)
    ;; Shells and processes (sticky: quit only on explicit close)
    (comint-mode               :size 0.30 :select t :quit nil)
    (inferior-python-mode      :size 0.30 :select t :quit nil)
    ("^\\*Python\\*$"          :size 0.30 :select t :quit nil)
    ("^\\*.*eshell.*\\*$"      :size 0.30 :select t :quit nil)
    ("^\\*.*shell.*\\*$"       :size 0.30 :select t :quit nil)
    ("^\\*.*terminal.*\\*$"    :size 0.30 :select t :quit nil)
    ;; LSP / VC / profiler
    ("^\\*lsp.*\\*$"           :size 0.35 :select t :quit t)
    ("^\\*vc-.*\\*$"           :size 0.40 :select t :quit t)
    ("^\\*diff-hl\\*$"         :size 0.30 :select t :quit t)
    (profiler-report-mode      :size 0.40 :select t :quit t)
    ;; Fallback (+all): any remaining special buffer is a popup, except
    ;; buffers this config manages itself.  Org capture buffers must stay
    ;; OUT: they are editable working buffers, and popup-mode's read-only
    ;; would break `org-capture-finalize'.
    ("^\\*Capture"             :ignore t)
    ("^\\*doom\\*\\'"          :ignore t)
    ("^\\*scratch\\*\\'"       :ignore t)
    ("^\\*Ibuffer\\*$"         :ignore t)
    ("^\\*ahsan\\*\\'"       :ignore t)
    ("^\\*"                    :size 0.30 :select nil :quit t))
  "Ordered popup rules: (MATCHER . PLIST).
MATCHER is a regexp string matched against the buffer name, a major
mode symbol matched with `derived-mode-p', or a string ending in
\"\\'\".  PLIST keys: :size (fraction or lines), :side, :select, :quit,
:ttl, :ignore."
  :type '(repeat sexp)
  :group 'ar/popup)

(defvar ar/popup--last-popup-buffer nil
  "The buffer of the most recently opened popup.")

(defun ar/popup--rule-for (buffer)
  "Return the first popup rule matching BUFFER, or nil.
BUFFER may be a buffer object or a buffer name string; a string with
no live buffer behind it still matches by name (mode predicates then
never match)."
  (let* ((buffer (if (stringp buffer) (or (get-buffer buffer) buffer) buffer))
         (name (if (bufferp buffer) (buffer-name buffer) buffer))
         (hit nil))
    (dolist (rule ar/popup--rules)
      (let ((matcher (car rule))
            (matched nil))
        (cond ((stringp matcher)
               (setq matched (string-match-p matcher name)))
              ((and (bufferp buffer)
                    (symbolp matcher)
                    (fboundp matcher) ; major-mode predicate
                    (with-current-buffer buffer
                      (derived-mode-p matcher)))
               (setq matched t)))
        (when (and matched (not hit))
          (setq hit rule))))
    hit))

(defun ar/popup--display (buffer _alist)
  "Display BUFFER as a popup according to its rule; return the window."
  (let* ((rule (ar/popup--rule-for buffer))
         (props (cdr rule))
         (side (or (plist-get props :side) 'bottom))
         (size (or (plist-get props :size) 0.30))
         (select (plist-get props :select))
         (prev-win (selected-window))
         (window
          (display-buffer-in-side-window
           buffer
           `((side . ,side)
             ,@(if (floatp size)
                   `((window-height . ,size))
                 `((window-height . ,size)))))))
    (when (window-live-p window)
      (set-window-parameter window 'popup t)
      (set-window-parameter window 'popup-quit (plist-get props :quit))
      (set-window-dedicated-p window t)
      (with-current-buffer buffer
        (ar/popup-buffer-mode 1))
      (setq ar/popup--last-popup-buffer buffer)
      (unless select
        (select-window prev-win)))
    window))

(defun ar/popup-p (&optional window)
  "Return non-nil if WINDOW (default: selected) is a popup window."
  (and (window-live-p (or window (selected-window)))
       (window-parameter (or window (selected-window)) 'popup)))

;; The buffer-local minor mode applied to every popup.
(define-minor-mode ar/popup-buffer-mode
  "Minor mode for buffers displayed in popup windows."
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-g") #'ar/popup-quit)
            (define-key map (kbd "q") #'ar/popup-quit)
            map)
  (when ar/popup-buffer-mode
    (setq-local buffer-read-only t)
    (setq-local mode-line-format nil)
    (setq-local header-line-format nil)))

(defun ar/popup-quit ()
  "Quit the current popup window and restore the previous window."
  (interactive)
  (if (ar/popup-p)
      (quit-restore-window (selected-window) 'kill)
    (call-interactively #'keyboard-quit)))

(defun ar/popup-toggle ()
  "Toggle the last popup: open, focus, or dismiss it."
  (interactive)
  (cond
   ;; Selected window is a popup: dismiss it.
   ((ar/popup-p)
    (quit-restore-window (selected-window) 'kill))
   ;; A popup window is visible: focus it.
   ((catch 'found
      (dolist (win (window-list))
        (when (ar/popup-p win)
          (select-window win)
          (throw 'found t)))))
   ;; Reopen the last popup buffer.
   ((buffer-live-p ar/popup--last-popup-buffer)
    (display-buffer ar/popup--last-popup-buffer))
   (t
    (display-buffer "*Messages*"))))

(defun ar/popup-close-all ()
  "Close all open popup windows."
  (interactive)
  (dolist (win (window-list))
    (when (ar/popup-p win)
      (quit-restore-window win 'kill))))

;; C-g closes the topmost popup instead of only signaling quit.
(defun ar/popup-close-on-quit (&rest _)
  "Close the topmost popup window on `C-g'."
  (when (and (not (region-active-p)))
    (let ((win (catch 'top
                 (dolist (w (window-list))
                   (when (ar/popup-p w)
                     (throw 'top w))))))
      (when (and (window-live-p win)
                 (not (eq win (selected-window)))
                 (window-parameter win 'popup-quit))
        (quit-restore-window win 'kill)))))

(defvar ar/popup--alist-entry nil
  "The display-buffer-alist entry installed by `ar/popup-global-mode'.")

(define-minor-mode ar/popup-global-mode
  "Globally enable the rule-based popup window manager."
  :global t
  :init-value nil
  :lighter ""
  (if ar/popup-global-mode
      (progn
        (setq ar/popup--alist-entry
              `((lambda (buffer &optional _alist)
                  (ar/popup--match-p buffer))
                (ar/popup--display)
                (reusable-frames . visible)))
        (add-to-list 'display-buffer-alist ar/popup--alist-entry)
        (advice-add #'keyboard-quit :before #'ar/popup-close-on-quit)
        (global-set-key (kbd "C-`") #'ar/popup-toggle))
    (when ar/popup--alist-entry
      (setq display-buffer-alist (delete ar/popup--alist-entry
                                         display-buffer-alist))))
    (advice-remove #'keyboard-quit #'ar/popup-close-on-quit))

(defun ar/popup--match-p (buffer &optional _alist)
  "Display-buffer condition: non-nil when BUFFER has a popup rule.
BUFFER may be a buffer object or a buffer name string (Emacs 31's
`display-buffer-assq-regexp' passes the name)."
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (and (bufferp buffer)
       (not (minibufferp buffer))
       (not (bound-and-true-p vertico--input))
       (ar/popup--rule-for buffer)
       t))

(unless noninteractive
  (ar/popup-global-mode 1))

;; Popup leader bindings.
(ar/global-leader
  "w p" '(ar/popup-toggle :wk "Toggle popup")
  "w P" '(ar/popup-close-all :wk "Close all popups"))


;;;
;;; The `*ahsan*' dashboard (faithful port of Doom's :ui dashboard).

(require 'cl-lib)

(defgroup ar/dashboard nil
  "Doom-style startup dashboard."
  :group 'convenience)

(declare-function ar/var-file "early-init")
(declare-function ar/etc-file "early-init")
(defvar ar/packages)  ;; the manifest, set by init.el
(defvar ar-modules)   ;; the ordered module list, set by init.el

(defcustom ar/dashboard-anchor '(center . center)
  "How to vertically and horizontally align dashboard widgets."
  :type '(cons (choice (const :tag "Top" top)
                       (const :tag "Bottom" bottom)
                       (const :tag "Centered" center))
               (choice (const :tag "Left" left)
                       (const :tag "Right" right)
                       (const :tag "Centered" center)))
  :group 'ar/dashboard)

(defcustom ar/dashboard-banner-vertical-padding '(2 . 2)
  "Number of newlines to pad the banner with, above and below, respectively."
  :type '(cons integer integer)
  :group 'ar/dashboard)

(defcustom ar/dashboard-menu-sections
  '(("Reload last session"
     :icon (nerd-icons-octicon "nf-oct-history" :face 'ar/dashboard-menu-title)
     :when (file-exists-p (ar/var-file "persp/persp-auto-save"))
     :action ar/dashboard-load-session)
    ("Open org-agenda"
     :icon (nerd-icons-octicon "nf-oct-calendar" :face 'ar/dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Recently opened files"
     :icon (nerd-icons-faicon "nf-fa-file_text" :face 'ar/dashboard-menu-title)
     :action consult-recent-file)
    ("Open project"
     :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'ar/dashboard-menu-title)
     :action project-switch-project)
    ("Jump to bookmark"
     :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'ar/dashboard-menu-title)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (nerd-icons-octicon "nf-oct-tools" :face 'ar/dashboard-menu-title)
     :action ar/dashboard-open-config)
    ("Open documentation"
     :icon (nerd-icons-octicon "nf-oct-book" :face 'ar/dashboard-menu-title)
     :action info-emacs-manual))
  "Menu buttons of the dashboard, ported from Doom's
`+dashboard-menu-sections'.  Each element is (LABEL . PLIST) with
:icon (form evaluated at render), :when (form gating display),
:action (interactive command), :key (hint override) and :face."
  :type '(repeat sexp)
  :group 'ar/dashboard)

;; Our pinned doom-themes ships no dashboard faces to inherit, so these
;; carry the Tokyo Night values seen on the real themed dashboard.
(defface ar/dashboard-menu-title
  '((t (:foreground "#f7768e" :weight bold)))
  "Face used for the title of menu widgets on the dashboard."
  :group 'ar/dashboard)
(defface ar/dashboard-menu-desc
  '((t (:foreground "#7dcfff")))
  "Face used for the key description of menu widgets on the dashboard."
  :group 'ar/dashboard)
(defface ar/dashboard-loaded
  '((t (:foreground "#565f89")))
  "Face used for the loaded packages benchmark."
  :group 'ar/dashboard)
(defface ar/dashboard-footer-icon
  '((t (:foreground "#7dcfff")))
  "Face used for the icon of the footer on the dashboard."
  :group 'ar/dashboard)

;; --- Centering engine (port of +dashboard-maxlen/+dashboard-insert) ------

(defun ar/dashboard-strlen (string)
  "Return the unicode-aware display width of STRING, in characters."
  (let ((width (frame-char-width))
        (len (string-pixel-width string)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))

(defun ar/dashboard-maxlen (string)
  "Return the display width of the longest line in multiline STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((width 0))
      (while (< (point) (point-max))
        (let ((len (ar/dashboard-strlen
                    (buffer-substring (pos-bol) (pos-eol)))))
          (setq width (max width len)))
        (forward-line 1))
      width)))

(defun ar/dashboard-insert (&rest lines)
  "Insert LINES into the dashboard buffer, aligned per `ar/dashboard-anchor'.
Alignment is a display-space spec on `line-prefix'/`indent-prefix',
so redisplay centers the block against the window at paint time --
no recomputed padding, at any window width."
  (let ((lines (delq nil lines)))
    (if-let* ((halign (cdr-safe ar/dashboard-anchor)))
        (let* ((width (ar/dashboard-maxlen (string-join lines "\n")))
               (prefix `(space :align-to
                               (- ,halign ,(if (eq halign 'right)
                                               (+ 2 width)
                                             (/ width 2))))))
          (add-text-properties
           (point) (progn (mapc (lambda (line) (insert line "\n")) lines)
                          (point))
           `(line-prefix ,prefix indent-prefix ,prefix)))
      (insert (string-join lines "\n")))))

;; --- Widgets --------------------------------------------------------------

(defun ar/dashboard-widget-banner ()
  "Draw the image banner widget.
The splash image rides on a `display' text property over a plain
space, with the line break inserted after it, so the `line-prefix'
centering spec applies.  The align-to expression references the
image by its spec cons `(image . PROPS)' -- the documented pixel
specification form; the vector descriptor `create-image' returns is
only valid as a `display' property, not inside align-to arithmetic.
Terminals render no banner."
  (let ((inhibit-read-only t))
    (when (and (display-images-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let* ((halign (cdr ar/dashboard-anchor))
             (image (create-image fancy-splash-image))
             (image-spec (cons 'image (cdr (append image nil))))
             (prefix (propertize
                      " " 'display `(space :align-to
                                           (- ,halign (0.5 . ,image-spec)))))
             (top-pad (or (car-safe ar/dashboard-banner-vertical-padding) 0))
             (bot-pad (or (cdr-safe ar/dashboard-banner-vertical-padding) 0)))
        (when (> top-pad 0)
          (insert (propertize "\n" 'display `(space :height ,top-pad))))
        (let ((beg (point)))
          (insert " ")
          (add-text-properties beg (point)
                               `(display ,image
                                 line-prefix ,prefix
                                 wrap-prefix ,prefix)))
        (insert "\n")
        (when (> bot-pad 0)
          (insert (propertize " " 'display `(space :height ,bot-pad)) "\n"))))))

(defun ar/dashboard-widget-menu ()
  "Draw dashboard menu items and keybindings.

See `ar/dashboard-menu-sections' to change the contents of the menu."
  (let ((inhibit-read-only t))
    (insert "\n")
    (dolist (section ar/dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face key) section
        (when (and (fboundp action)
                   (or (null when) (eval when t)))
          (ar/dashboard-insert
           (let ((icon (if (stringp icon) icon (eval icon t))))
             (format (format " %s%%s%%10s " (if icon "%s\t" "%s"))
                     (or icon "")
                     (with-temp-buffer
                       (insert-text-button
                        label
                        'action (lambda (_button) (call-interactively action))
                        'face (or face 'ar/dashboard-menu-title)
                        'follow-link t
                        'help-echo
                        (format "%s (%s)" label
                                (propertize (symbol-name action)
                                            'face 'ar/dashboard-menu-desc)))
                       (format "%-38s" (buffer-string)))
                     ;; Look up the command's key binding dynamically.
                     (propertize
                      (or key
                          (when-let* ((keys (where-is-internal action nil t)))
                            (with-temp-buffer
                              (save-excursion (insert (key-description keys)))
                              (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                (replace-match
                                 (let ((str (match-string 1)))
                                   (upcase (substring str 0 (min (length str) 3))))))
                              (buffer-string)))
                          "")
                      'face 'ar/dashboard-menu-desc)))
           (propertize "\n" 'display '(space . (:relative-height 0.01)))))))))

(defun ar/dashboard-widget-loaded ()
  "Draw number of modules and packages loaded, and the session's startup time."
  (ar/dashboard-insert
   (propertize
    (format "Emacs loaded %d packages across %d modules in %.3fs"
            (length ar/packages) (length ar-modules)
            (float-time (time-subtract (or after-init-time before-init-time)
                                       before-init-time)))
    'face 'ar/dashboard-loaded)))

(defun ar/dashboard-widget-footer ()
  "Draw the footer glyph button."
  (ar/dashboard-insert
   (with-temp-buffer
     (insert "\n" (propertize " " 'display '(space . (:relative-height 2.0))))
     (insert-text-button
      (or (ignore-errors
            (nerd-icons-sucicon "nf-custom-emacs"
                                :face 'ar/dashboard-footer-icon))
          (propertize "emacs" 'face 'ar/dashboard-footer-icon))
      'action (lambda (_button)
                (browse-url "https://www.gnu.org/software/emacs/"))
      'follow-link t
      'help-echo "Open the GNU Emacs project page")
     (buffer-string))))

(defcustom ar/dashboard-functions
  '(ar/dashboard-widget-banner
    ar/dashboard-widget-menu
    ar/dashboard-widget-loaded
    ar/dashboard-widget-footer)
  "Ordered widget functions run to construct the dashboard buffer.

These functions take no arguments and the dashboard buffer is current
while they run."
  :type 'hook
  :group 'ar/dashboard)

;; --- Point trap, selection and movement (Doom's button navigation) --------

(defun ar/dashboard-reposition-point-h ()
  "Trap the point in the buttons."
  (when (eq (current-buffer) (get-buffer "*ahsan*"))
    (when (region-active-p)
      (setq deactivate-mark t)
      (when (bound-and-true-p evil-local-mode)
        (evil-change-to-previous-state)))
    (cond ((button-at (point))
           (forward-button 0 nil nil t))
          ((save-restriction
             (narrow-to-region (pos-bol) (pos-eol))
             (forward-button 1 nil nil t)))
          ((backward-button 1 nil nil t))
          ((goto-char (point-min))
           (forward-button 1 nil nil t)))
    ;; Hide the cursor if there are no buttons.
    (unless (button-at (point))
      (setq-local cursor-type nil
                  evil-normal-state-cursor (list nil)))))

(defun ar/dashboard--help-echo ()
  "Echo the help-echo of the button at point."
  (when-let* ((button (button-at (point)))
              (help (button-get button 'help-echo)))
    (message "%s" help)))

(defun ar/dashboard-forward-button (n)
  "Like `forward-button', but don't wrap; echo the button's help."
  (interactive "p")
  (forward-button n nil)
  (ar/dashboard--help-echo))

(defun ar/dashboard-backward-button (n)
  "Like `backward-button', but don't wrap; echo the button's help."
  (interactive "p")
  (backward-button n nil)
  (ar/dashboard--help-echo))

(defun ar/dashboard-push-button ()
  "Push the button at point, saving the window's scroll position."
  (interactive)
  (let ((win (selected-window)))
    (set-window-parameter win 'ar/dashboard-last-window-start
                          (window-start win))
    (set-window-parameter win 'ar/dashboard-last-position (point)))
  (push-button))

(define-derived-mode ar/dashboard-mode special-mode "ahsan"
  "Major mode for the Doom-style startup dashboard."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local display-line-numbers-type nil)
  (setq-local mode-line-right-align-edge 'right-margin)
  ;; hl-line would highlight up to the BOL of the following line, which
  ;; looks ugly, so exclude the newline at EOL.
  (setq-local hl-line-range-function (lambda () (cons (pos-bol) (pos-eol))))
  (setq-local enable-local-variables nil)
  ;; Buffer-local font scale: shrinks dashboard text only.  `face-remap'
  ;; is per-buffer, so the global font settings are untouched.
  (face-remap-add-relative 'default :height 0.8)
  (hl-line-mode 1)
  (ar/hide-modeline-mode 1)
  (add-hook 'kill-buffer-hook #'ar/dashboard--reopen nil t)
  (add-hook 'post-command-hook #'ar/dashboard-reposition-point-h nil 'local))

(define-key ar/dashboard-mode-map [left-margin mouse-1] #'ignore)
(define-key ar/dashboard-mode-map [remap forward-button]
            #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map [remap backward-button]
            #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [remap push-button]
            #'ar/dashboard-push-button)
(define-key ar/dashboard-mode-map "n" #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map "p" #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map (kbd "C-n") #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map (kbd "C-p") #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [down] #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map [up] #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [tab] #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map [backtab] #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [remap evil-next-line]
            #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map [remap evil-previous-line]
            #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [remap evil-next-visual-line]
            #'ar/dashboard-forward-button)
(define-key ar/dashboard-mode-map [remap evil-previous-visual-line]
            #'ar/dashboard-backward-button)
(define-key ar/dashboard-mode-map [remap evil-delete] #'ignore)

;; --- Session actions -------------------------------------------------------

(defun ar/dashboard-load-session ()
  "Restore the last saved persp-mode session from the auto-save file."
  (interactive)
  (persp-load-state-from-file (ar/var-file "persp/persp-auto-save")))

(defun ar/dashboard-open-config ()
  "Open the configuration root in Dired (Doom's open-private-config)."
  (interactive)
  (dired ar/config-dir))

;; --- Rendering and resize --------------------------------------------------

(defun ar/dashboard-resize-h (&rest _)
  "Re-center the dashboard vertically and reset its margins and fringes.
Horizontal alignment is a display spec and needs no resize handling."
  (dlet (buffer-list-update-hook
         window-configuration-change-hook
         window-size-change-functions)
    (when-let* ((windows (get-buffer-window-list "*ahsan*" nil t)))
      (dolist (win windows)
        (unless (= (window-start win)
                   (or (window-parameter win 'ar/dashboard-last-window-start)
                       1))
          (set-window-start
           win (or (window-parameter win 'ar/dashboard-last-window-start) 0)))
        (when-let* ((pos (window-parameter win 'ar/dashboard-last-position)))
          (goto-char pos)
          (ar/dashboard-reposition-point-h))
        (cl-destructuring-bind (left right) (window-fringes win)
          (unless (and (= left 0) (= right 0))
            (set-window-fringes win 0 0))))
      (with-current-buffer "*ahsan*"
        (let ((inhibit-read-only t))
          (save-excursion
            (with-silent-modifications
              ;; Only plain blank lines are re-centering padding; banner
              ;; lines carry `display' properties (the pads and the splash
              ;; image) and must survive the recount.
              (goto-char (point-min))
              (while (and (eolp) (not (eobp))
                          (not (get-text-property (point) 'display)))
                (forward-line 1))
              (delete-region (point-min) (point))
              (insert
               (make-string
                (max 0 (pcase (car-safe ar/dashboard-anchor)
                         (`top 0)
                         (`center
                          (- (/ (window-height (get-buffer-window)) 2)
                             (round (/ (count-lines (point-min) (point-max))
                                       2))))
                         (_ 0)))
                ?\n)))))))))

(defun ar/dashboard-reload (&optional force)
  "Update the dashboard buffer (or create it, if it doesn't exist).
Without FORCE, only reload when the dashboard is actually displayed."
  (when (or force
            (and (get-buffer-window "*ahsan*" t)
                 (not (window-minibuffer-p (frame-selected-window)))))
    (with-current-buffer (get-buffer-create "*ahsan*")
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode 'ar/dashboard-mode)
            (ar/dashboard-mode))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (widget ar/dashboard-functions)
              (funcall widget)))
          (goto-char pt)
          (ar/dashboard-reposition-point-h))
        (ar/dashboard-resize-h))
      (current-buffer))))

(defun ar/dashboard-initial-buffer ()
  "Return the dashboard buffer, rendering it first."
  (ar/dashboard-reload t))

(defun ar/dashboard--reopen ()
  "Reopen the dashboard when the last window's buffer is killed."
  (when (and (null (delq nil (mapcar #'buffer-file-name (buffer-list))))
             (not noninteractive))
    (switch-to-buffer (ar/dashboard-reload t))))

(defun ar/dashboard-scratch-to-dashboard-h ()
  "Swap a fresh server frame's *scratch* for the dashboard."
  (when (string= (buffer-name) "*scratch*")
    (switch-to-buffer (ar/dashboard-reload t))))

(defun ar/dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause: new frames' dimensions
may not be fully initialized by the time this runs."
  (run-with-timer 0.1 nil #'ar/dashboard-reload t))

(setq fancy-splash-image (ar/etc-file "splash-image.png"))

(unless noninteractive
  (setq initial-buffer-choice #'ar/dashboard-initial-buffer)
  (add-hook 'window-size-change-functions #'ar/dashboard-resize-h)
  (with-eval-after-load 'server
    (add-hook 'server-after-make-frame-hook
              #'ar/dashboard-scratch-to-dashboard-h)
    (add-hook 'server-after-make-frame-hook
              #'ar/dashboard-reload-frame-h)))

;; Module boundary: ui is loaded by init.el after editor-config.

(provide 'ui-config)
;;; ui-config.el ends here
