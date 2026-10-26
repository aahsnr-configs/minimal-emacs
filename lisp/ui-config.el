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
(defun ar/setup-fonts (&optional _frame)
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
  :group 'ar)

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
(defvar-local ar/popup--hide-modeline-was-active nil
  "Non-nil when modeline hiding was active before popup mode.")

(define-minor-mode ar/popup-buffer-mode
  "Minor mode for buffers displayed in popup windows."
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-g") #'ar/popup-quit)
            (define-key map (kbd "q") #'ar/popup-quit)
            map)
  (if ar/popup-buffer-mode
      (progn
        (setq-local ar/popup--hide-modeline-was-active
                    (bound-and-true-p ar/hide-modeline-mode))
        (ar/hide-modeline-mode 1))
    (when (local-variable-p 'ar/popup--hide-modeline-was-active)
      (unless ar/popup--hide-modeline-was-active
        (ar/hide-modeline-mode -1))
      (kill-local-variable 'ar/popup--hide-modeline-was-active))))

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
    (progn
      (when ar/popup--alist-entry
        (setq display-buffer-alist (delete ar/popup--alist-entry
                                           display-buffer-alist)))
      (advice-remove #'keyboard-quit #'ar/popup-close-on-quit))))

(defun ar/popup--match-p (buffer &optional _alist)
  "Display-buffer condition: non-nil when BUFFER has a popup rule.
BUFFER may be a buffer object or a buffer name string (Emacs 31's
`display-buffer-assq-regexp' passes the name)."
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (let ((rule (ar/popup--rule-for buffer)))
    (and rule
         (not (plist-get (cdr rule) :ignore))
         (not (minibufferp buffer))
         (not (bound-and-true-p vertico--input)))))

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
  "Vanilla configuration for the Doom-styled dashboard."
  :group 'ar)

(declare-function ar/var-file "early-init")
(declare-function ar/etc-file "early-init")
(defvar ar/config-dir)
(declare-function widget-create "wid-edit")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-sucicon "nerd-icons")
(declare-function dashboard-open "dashboard")
(declare-function dashboard-setup-startup-hook "dashboard")
(declare-function dashboard-insert-startupify-lists "dashboard")
(defvar dashboard-mode-map)
(defvar ar/packages)
(defvar ar-modules)
(defvar dashboard-buffer-name)
(defvar dashboard-before-initialize-hook)
(defvar dashboard-after-initialize-hook)
(defvar dashboard-display-icons-p)
(defvar dashboard-items)
(defvar dashboard-item-generators)
(defvar doom-dashboard-set-widget-binding)
(defvar server-after-make-frame-hook)

(defun ar/dashboard-init-info ()
  "Return the startup summary shown by the dashboard."
  (format "Emacs loaded %d packages across %d modules in %.3fs"
          (length ar/packages)
          (length ar-modules)
          (float-time
           (time-subtract (or after-init-time before-init-time)
                          before-init-time))))

(defun ar/dashboard--insert-shortmenu (label command icon)
  "Insert a dashboard button for LABEL, COMMAND, and Nerd ICON."
  (when dashboard-display-icons-p
    ;; Keep alignment spaces out of the icon's Nerd Font properties.  Padding
    ;; a propertized icon with `format' can render the spaces as missing glyphs.
    (insert (nerd-icons-octicon icon :face 'dashboard-heading))
    (insert (propertize "  " 'face 'default)))
  (widget-create
   'item
   :tag (format "%-30s" label)
   :action (lambda (&rest _) (call-interactively command))
   :mouse-face 'highlight
   :button-face 'dashboard-heading
   :button-prefix ""
   :button-suffix ""
   :format "%[%t%]")
  (when doom-dashboard-set-widget-binding
    (insert
     (propertize
      (substitute-command-keys (format "\\[%s]" command))
      'face 'doom-dashboard-bindings-face))))

(defun ar/dashboard-insert-session-shortmenu (&optional _)
  "Insert the Reload last session button when a saved session exists."
  (when (file-exists-p (ar/var-file "persp/persp-auto-save"))
    (ar/dashboard--insert-shortmenu
     "Reload last session" #'ar/dashboard-load-session "nf-oct-history")))

(defun ar/dashboard-insert-agenda-shortmenu (&optional _)
  "Insert the Open org-agenda dashboard button."
  (ar/dashboard--insert-shortmenu
   "Open org-agenda" #'org-agenda "nf-oct-calendar"))

(defun ar/dashboard-insert-recents-shortmenu (&optional _)
  "Insert the Recently opened files dashboard button."
  (ar/dashboard--insert-shortmenu
   "Recently opened files" #'consult-recent-file "nf-oct-file"))

(defun ar/dashboard-insert-project-shortmenu (&optional _)
  "Insert the Open project dashboard button."
  (ar/dashboard--insert-shortmenu
   "Open project" #'project-switch-project "nf-oct-briefcase"))

(defun ar/dashboard-insert-bookmark-shortmenu (&optional _)
  "Insert the Jump to bookmark dashboard button."
  (ar/dashboard--insert-shortmenu
   "Jump to bookmark" #'bookmark-jump "nf-oct-bookmark"))

(defun ar/dashboard-insert-config-shortmenu (&optional _)
  "Insert the Open private configuration button."
  (ar/dashboard--insert-shortmenu
   "Open private configuration" #'ar/dashboard-open-config "nf-oct-tools"))

(defun ar/dashboard-insert-manual-shortmenu (&optional _)
  "Insert the Open documentation button."
  (ar/dashboard--insert-shortmenu
   "Open documentation" #'info-emacs-manual "nf-oct-book"))

(defun ar/dashboard-refresh-items ()
  "Refresh the dashboard items, including session availability."
  (setq dashboard-items
        (append (when (file-exists-p (ar/var-file "persp/persp-auto-save"))
                  '((session . 1)))
                '((agenda . 1)
                  (recents . 1)
                  (projects . 1)
                  (bookmarks . 1)
                  (config . 1)
                  (manual . 1)))))

(defun ar/dashboard-center-visible (&rest args)
  "Refresh the visible dashboard on the affected frame after a resize."
  (let* ((frame (or (car args) (selected-frame)))
         (window (get-buffer-window dashboard-buffer-name frame)))
    (when (window-live-p window)
      (with-selected-window window
        (dashboard-insert-startupify-lists t)))))

(use-package dashboard
  :demand t
  :custom
  (dashboard-buffer-name "*ahsan*")
  (dashboard-startup-banner (ar/etc-file "dashboard-emacs.txt"))
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((agenda . 1)
                     (recents . 1)
                     (projects . 1)
                     (bookmarks . 1)
                     (config . 1)
                     (manual . 1)))
  (dashboard-init-info #'ar/dashboard-init-info)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-page-separator "\n")
  (dashboard-footer-icon
   (or (ignore-errors
         (nerd-icons-sucicon "nf-custom-emacs"
                             :face 'dashboard-footer-face))
       "Emacs"))
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-newline
     doom-dashboard-insert-homepage-footer))
  :custom-face
  (dashboard-text-banner ((t (:foreground "#7aa2f7" :weight bold)))))

(use-package doom-dashboard
  :after dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("<remap> <dashboard-previous-line>" . widget-backward)
        ("<remap> <dashboard-next-line>" . widget-forward)
        ("<remap> <previous-line>" . widget-backward)
        ("<remap> <next-line>" . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>" . widget-backward))
  :custom
  (doom-dashboard-homepage-footer-url "https://www.gnu.org/software/emacs/")
  (doom-dashboard-set-widget-binding t)
  :config
  (setq dashboard-item-generators
        '((session . ar/dashboard-insert-session-shortmenu)
          (agenda . ar/dashboard-insert-agenda-shortmenu)
          (recents . ar/dashboard-insert-recents-shortmenu)
          (projects . ar/dashboard-insert-project-shortmenu)
          (bookmarks . ar/dashboard-insert-bookmark-shortmenu)
          (config . ar/dashboard-insert-config-shortmenu)
          (manual . ar/dashboard-insert-manual-shortmenu)))
  (add-hook 'dashboard-before-initialize-hook #'ar/dashboard-refresh-items)
  (unless noninteractive
    (dashboard-setup-startup-hook)
    (add-hook 'dashboard-after-initialize-hook
              #'ar/dashboard-center-visible)
    (add-hook 'window-size-change-functions
              #'ar/dashboard-center-visible t)
    (when (daemonp)
      (setq initial-buffer-choice #'dashboard-open))
    (with-eval-after-load 'server
      (add-hook 'server-after-make-frame-hook #'dashboard-open))))

(defun ar/dashboard-load-session ()
  "Restore the last saved persp-mode session from the auto-save file."
  (interactive)
  (persp-load-state-from-file (ar/var-file "persp/persp-auto-save")))

(defun ar/dashboard-open-config ()
  "Open the configuration root in Dired."
  (interactive)
  (dired ar/config-dir))

(provide 'ui-config)
;;; ui-config.el ends here
