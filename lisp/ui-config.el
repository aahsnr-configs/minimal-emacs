;;; ui-config.el --- Visual shell: theme, modeline, fonts, icons, folding -*- lexical-binding: t; -*-

;;; Commentary:

;; The visual shell: theme, modeline, fonts, nerd-icons, and the
;; unified folding dispatcher.

;;; Code:

;; Load-order assumptions (init.el requires core-config -> editor-config -> ui-config): the core
;; module provides the large-file guard used by several activation advices
;; below, and the editor module demands `evil' and `general', which the
;; folding keybindings at the end of this file bind through.
(declare-function too-long-file-p "core-config")
(declare-function global-text-scale-adjust "face-remap")
(declare-function outline-cycle "outline")
(declare-function outline-show-subtree "outline")
(declare-function outline-hide-subtree "outline")
(declare-function outline-show-all "outline")
(declare-function outline-hide-body "outline")

;; Activation guards for the deferred packages configured below.  Top-level
;; on purpose: advising a not-yet-defined function registers the advice and
;; it activates when the package defines the mode (verified on Emacs 31.1),
;; which guards activation from the very first call and keeps the
;; use-package blocks free of compiler warnings.  In every guard the
;; prefix-arg clause still allows explicit deactivation.

;; Abort activation in massive buffers (too-long-file-p is core-config.el's helper:
;; over 500k characters or 10k lines).
(define-advice colorful-mode (:before-while (&optional arg) guard-large-files)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (too-long-file-p))))

(define-advice rainbow-delimiters-mode (:before-while (&optional arg) guard-large-files)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (too-long-file-p))))

(define-advice hs-minor-mode (:before-while (&optional arg) guard-large-files)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (too-long-file-p))))

;; Lisp modes have no tree-sitter grammar in this config.
(define-advice treesit-fold-mode (:before-while (&optional arg) guard-lisp-modes)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (derived-mode-p 'lisp-mode 'emacs-lisp-mode
                           'lisp-interaction-mode 'lisp-data-mode))))

(define-advice treesit-fold-mode (:before-while (&optional arg) guard-large-files)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (too-long-file-p))))

(define-advice vimish-fold-mode (:before-while (&optional arg) guard-large-files)
  (or (and arg (< (prefix-numeric-value arg) 1))
      (not (too-long-file-p))))

;; Theme.  Eagerly loaded and applied so the first frame never paints
;; unstyled (the design's eager-load budget includes the theme).
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom") ; "doom-colors" for a less minimal look
  :config
  ;; CRITICAL: patch the face alist BEFORE the theme engine evaluates it.
  ;; Emacs 31 strictly forbids face inheritance cycles (doomemacs/themes#875).
  (when (boundp 'doom-themes-base-faces)
    (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
            '(:inherit 'gnus-group-mail-1-empty :weight 'normal)))
  (load-theme 'doom-tokyo-night t)
  ;; The old config also forced straight underlines on the `flycheck-*' faces
  ;; via the 'user theme; that died with flycheck (design §6.2) -- flymake's
  ;; own diagnostic faces apply now.
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Solaire: swap background faces in side windows and popups.  Demanded right
;; after the theme so the remap table is in place before any buffer paints.
(use-package solaire-mode
  :demand t
  :after doom-themes
  ;; (ghostel-compile-view-mode, not the nonexistent `ghostel-compile-mode'
  ;; the old entry named: ghostel 0.56.0's compile buffer mode is
  ;; `ghostel-compile-view-mode', as core's hl-line guard also uses.)
  :hook ((change-major-mode ghostel-mode ghostel-compile-view-mode after-revert
          ediff-prepare-buffer org-capture-mode)
         . turn-on-solaire-mode)
  :config
  ;; Ghostel's C engine queries its custom `ghostel-default' face directly,
  ;; bypassing the standard `default' remap; injecting it into the remap
  ;; alist forces the terminal drawer to adopt the swapped background via
  ;; native buffer-local remapping.
  (add-to-list 'solaire-mode-remap-alist '(ghostel-default . solaire-default-face))
  (solaire-global-mode +1))

;; Mathematically corrected poshandler for near-bottom centering: subtracts
;; the child frame's height to prevent bottom-edge clipping.  Defined
;; unconditionally -- it is a pure function usable by any posframe consumer
;; (e.g. ace-window hints in the keybinds module) whether or not posframe has
;; loaded yet.
(defun ar/posframe-poshandler-frame-center-near-bottom (info)
  "Center INFO's posframe horizontally, positioned near the parent frame bottom."
  (let ((parent-width (plist-get info :parent-frame-width))
        (parent-height (plist-get info :parent-frame-height))
        (posframe-width (plist-get info :posframe-width))
        (posframe-height (plist-get info :posframe-height))
        (font-height (plist-get info :font-height)))
    (cons (/ (- parent-width posframe-width) 2)
          (- parent-height posframe-height (* 2 font-height)))))

;; Child frame base library.  Deferred: it loads with its first consumer
;; (ace-window's posframe hints, enabled by the keybinds module when child
;; frames are workable), exactly as in the old config.
(use-package posframe
  :defer t
  :custom-face
  ;; Theme the native Emacs 28+ child frame border like the active region.
  (child-frame-border ((t (:inherit region))))
  :config
  (with-no-warnings
    ;; Prevent the parent frame's fringe background from bleeding into the
    ;; child frame; `frame-live-p' guards redisplay crashes during daemon
    ;; teardown.
    (defun ar/posframe--isolate-fringe (&rest _)
      (when (and (boundp 'posframe--frame)
                 (frame-live-p posframe--frame))
        (set-face-background 'fringe nil posframe--frame)))
    (advice-add 'posframe--create-posframe :after #'ar/posframe--isolate-fringe)))

;; Icon engine.  Eagerly loaded (design §4.2): the modeline below, treemacs,
;; corfu and the transients all dereference nerd-icons at their first paint,
;; and the doom-modeline declaration relies on it being available.
(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-color-icons t))

;; Global typography.  Every face-setting form here is display-only, so batch
;; runs (which -l this file) must skip it; the server hook re-applies fonts on
;; emacsclient frames (daemon-safe, design §6.1).
(defun ar/setup-fonts ()
  "Apply typography, baseline faces, and fontset fallbacks to the frame."
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 125 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 125 :weight 'medium)
  ;; Variable-pitch is deliberately monospaced to keep Org tables aligned.
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height 125 :weight 'medium)
  ;; Pre-register Noto Sans for the Unicode blocks org-modern's glyphs use.
  (set-fontset-font t '(#x25A0 . #x25FF) "Noto Sans")  ; Geometric Shapes (◉ ○ ◇ ▷)
  (set-fontset-font t '(#x2600 . #x26FF) "Noto Sans")  ; Miscellaneous Symbols
  (set-fontset-font t '(#x2700 . #x27BF) "Noto Sans")) ; Dingbats (✸ ✿ ✤ ✜)

(unless noninteractive
  (ar/setup-fonts))

(with-eval-after-load 'server
  (add-hook 'server-after-make-frame-hook #'ar/setup-fonts))

;; Maximum syntax-highlighting decoration across all programming buffers.
(setq font-lock-maximum-decoration t)

;; Modeline.  Demanded: the modeline is part of the first paint; deferring it
;; would show the stock modeline until the first command.  Project detection
;; falls back to native project.el, and the lsp segment tracks lsp-mode.
(use-package doom-modeline
  :demand t
  :init
  (size-indication-mode -1)
  (setq-default mode-line-percent-position nil)
  :custom
  ;; Visual dimensions and split-window overflow safeguard.
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-window-width-limit 90)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-buffer-file-name-style 'auto)
  ;; Disable redundant encoding and percentage indicators.  (The old
  ;; `doom-modeline-buffer-file-size nil' entry is dropped: doom-modeline
  ;; 4.2.1 defines no such defcustom.)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  ;; Native Evil state, modal icons, and major-mode glyphs.
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-env-version t)
  ;; Version control integration.
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-max-length 0)
  ;; Disable unused integrations to prevent background I/O overhead.
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-minor-modes nil)
  :config
  (doom-modeline-mode 1))

;; Hide modeline: native redisplay concealment for terminal, agenda, and popup
;; buffers, as a zero-overhead buffer-local minor mode.
(defvar-local ar/saved-mode-line-format nil "Cached modeline format.")
(defvar-local ar/saved-header-line-format nil "Cached header-line format.")
(defvar-local ar/saved-tab-line-format nil "Cached tab-line format.")

(define-minor-mode ar/hide-modeline-mode
  "Conceal the modeline, header-line, and tab-line in specific buffers."
  :init-value nil
  :lighter nil
  (if ar/hide-modeline-mode
      (progn
        ;; Cache original formats only if they are not already nil.
        (unless (eq mode-line-format nil)
          (setq-local ar/saved-mode-line-format mode-line-format))
        (unless (eq header-line-format nil)
          (setq-local ar/saved-header-line-format header-line-format))
        (unless (eq tab-line-format nil)
          (setq-local ar/saved-tab-line-format tab-line-format))
        ;; Force concealment.
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil))
    ;; Restore cached formats upon deactivation.
    (setq-local mode-line-format ar/saved-mode-line-format
                header-line-format ar/saved-header-line-format
                tab-line-format ar/saved-tab-line-format))
  ;; Force C-level redisplay to prevent phantom spacing artifacts.
  (force-mode-line-update))

(defun ar/enable-hide-modeline ()
  "Safely enable `ar/hide-modeline-mode' without toggling."
  (ar/hide-modeline-mode 1))

;; Pruned to modes that exist in this config: the old list's lsp-ui, eat,
;; vterm, and pdf-annot entries died with their packages, and plain
;; `org-agenda' produced the nonexistent `org-agenda-hook' -- the real hooks
;; are `org-agenda-mode-hook' and `org-agenda-finalize-hook' (verified on
;; Emacs 31.1).
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
;; geometry on PGTK/Wayland (design §6.1).  The variables live in
;; face-remap.el, which loads on the first zoom command -- hence the
;; after-load guard instead of free-variable setqs.
(with-eval-after-load 'face-remap
  (setq global-text-scale-adjust-resizes-frames nil
        text-scale-mode-step 1.1))

;; Hardcoded arguments bypass the interactive prefix parser to prevent sign
;; inversion.  Bound to C-=, C--, C-0, C-<wheel-up> and C-<wheel-down> by the
;; keybinds module, which loads last.
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

;; Cursor.  `setq' alone would leave the blink timer running; the mode call
;; halts it and removes the per-redisplay overhead.
(blink-cursor-mode -1)

;; Fill column indicator: native C-level display primitive rendering a vertical
;; boundary at `fill-column', confined to code and config buffers.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)

;; Color visualization: hex/RGB/named color previews via overlay stacking.
;; Activation is guarded by `colorful-mode@guard-large-files' above.
(use-package colorful-mode
  :defer t
  :custom
  ;; Confine color-name matching to strings and comments in programming
  ;; buffers.
  (colorful-only-strings 'only-prog)
  ;; Prevent double-rendering conflicts with native CSS fontification.
  (css-fontify-colors nil)
  ;; Styling, markup, prose, and programming modes.  The old config's
  ;; `LaTeX-mode' entry died with AUCTeX, which this stack does not install.
  :hook ((css-mode scss-mode less-css-mode html-mode
          org-mode markdown-mode gfm-mode prog-mode)
         . colorful-mode))

;; Rainbow delimiters: depth-based delimiter colorization in code buffers,
;; aborted in massive files to prevent main-thread blocking (`guard-large-files'
;; advice above).  (AUCTeX's `LaTeX-mode' hook from the old config is gone
;; with the package.)
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; VSCode-style indent guides (build/design-lsp.md §7).  Deferred to the first
;; prog-mode buffer.  Stipples: PGTK builds support them from Emacs 30
;; (indent-bars README:110, quoted in ecosystem-facts.md §6) so this 31.1 PGTK
;; build takes the stipple path; Arch builds are cairo-backed and the README
;; carries a cairo/high-DPI caveat (README:111) — if stipples render wrong,
;; the documented fallback is (setq indent-bars-prefer-character t)
;; (README:195).
(use-package indent-bars
  :defer t
  :hook (prog-mode . indent-bars-mode)
  :custom
  ;; Tree-sitter-aware scope detection; python scope is the README's own
  ;; example (README:82-96, quoted in ecosystem-facts.md §6).  Extend per
  ;; language from the same README section as needed.
  (indent-bars-treesit-scope '((python function_definition class_definition))))

;; Shared truncation ellipsis, inherited by `treesit-fold', `outline',
;; `hideshow' and tabulated lists (parity restore: the old config's
;; "Global Ellipsis" setting, which had been dropped without a design §6.2
;; decision entry).
(setq truncate-string-ellipsis " ↴ ")

;; Unified folding dispatcher: routes to the best folding engine available in
;; the current buffer (treesit-fold -> outline -> hideshow -> vimish-fold).
;; Bound to the z-family below and preserved verbatim from the old config.
(defun ar/fold-toggle ()
  "Toggle code fold at point using the best available folding engine."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-toggle))
        ((bound-and-true-p outline-minor-mode) (outline-cycle))
        ((bound-and-true-p hs-minor-mode)
         ;; Emacs 31+: prefer `hs-cycle' for intelligent visibility cycling.
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

;; Hideshow: folds code in languages lacking tree-sitter grammars, using the
;; Emacs 31 native indicators (all three variables verified bound on 31.1).
;; Remains deferred on its commands, as in the old config; activation is
;; guarded by `hs-minor-mode@guard-large-files' above.
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
  ;; Hide comments when hiding all blocks.
  (hs-hide-comments-when-hiding-all nil)
  ;; Emacs 31+: native hidden line count next to the ellipsis.
  (hs-display-lines-hidden t)
  ;; Emacs 31+: clickable fringe indicators for block toggling.
  (hs-show-indicators t)
  (hs-indicator-type 'fringe))

;; Tree-sitter folding: language-aware region folding at the cursor.  Demanded
;; (behind the availability guard): `global-treesit-fold-mode' has no autoload
;; trigger, so a deferred declaration would never load -- the old config
;; loaded it eagerly for the same reason.
(use-package treesit-fold
  :if (treesit-available-p)
  :demand t
  :custom
  ;; Show the first line of folded comments/docstrings inline.
  (treesit-fold-summary-show t)
  ;; Display the hidden line count next to the ellipsis.
  (treesit-fold-line-count-show t)
  :config
  ;; Activation is guarded by the `guard-lisp-modes' and `guard-large-files'
  ;; advices on `treesit-fold-mode' above.
  ;; Enable per-node folding in all tree-sitter supported modes.
  (global-treesit-fold-mode 1))

;; Manual folding: persistent region folds for the modes tree-sitter folding
;; does not reach, plus its evil integration.  Both load on first use of any
;; bound vimish command (the z-family below); activation is guarded by
;; `vimish-fold-mode@guard-large-files' above.
;;
;; The evil integration's lighter and target modes are set here, before either
;; package loads: `use-package :after' defers :init along with the rest of the
;; expansion, and the minor mode reads both variables dynamically, so an
;; eagerly-bound value is the only ordering that is correct unconditionally.
;; The bare defvars pre-declare the package's defcustoms (an already-bound
;; defcustom keeps its current value when the package loads).
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
  ;; Route fold persistence to the var directory.
  (vimish-fold-dir (ar/var-file "vimish-fold/"))
  ;; Right fringe indicators avoid conflicts with the VC/fringe indicators.
  (vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode 1))

(use-package evil-vimish-fold
  :after vimish-fold
  :commands (evil-vimish-fold/create evil-vimish-fold/create-line)
  :config
  (global-evil-vimish-fold-mode 1))

;; evil-vimish-fold defines its operators without autoload cookies, and the
;; `:commands' registration above is deferred behind `:after vimish-fold' --
;; pressing zf/zF before any vimish command has loaded would signal
;; void-function.  Register the autoload objects unconditionally here.
(autoload #'evil-vimish-fold/create "evil-vimish-fold" nil t)
(autoload #'evil-vimish-fold/create-line "evil-vimish-fold" nil t)

;; The z-family folding bindings (evil motion state; `evil' and `general' are
;; demanded by the editor module, which loads before this one).  The vimish
;; commands resolve to the autoloads declared by the blocks above, so the
;; first press loads the packages and their :config blocks run first.
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

(provide 'ui-config)
;;; ui-config.el ends here
