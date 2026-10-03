;;; keybinds-config.el --- Leader trees, transient menus, windows, misc bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; The keybinding module: the unified escape/minibuffer plumbing, the
;; global and local leader trees, transient menus, window management,
;; and popper.

;;; Code:

;; Unified escape and minibuffer plumbing.  `ar/escape-key' is defined in the
;; core module and bound here (normal/visual/motion states plus every
;; minibuffer/Ex map); it delegates to `abort-recursive-edit' when the
;; minibuffer is active.  `consult-history' is autoloaded, so binding C-s
;; before consult loads is safe.
(defvar ar/minibuffer-escape-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    evil-ex-completion-map
    evil-ex-search-keymap)
  "Minibuffer and Ex keymaps receiving the escape and isearch-history bindings.")

;; All base maps are bound in Emacs 31.1; the evil Ex maps are present because
;; the editor module demands evil before this module loads.  The boundp guard
;; keeps the loop honest under any future reordering.
(dolist (map-sym ar/minibuffer-escape-maps)
  (when (boundp map-sym)
    (define-key (symbol-value map-sym) (kbd "<escape>") #'ar/escape-key)
    (define-key (symbol-value map-sym) (kbd "C-s") #'consult-history)))

(general-define-key
 :states '(normal visual motion)
 "<escape>" #'ar/escape-key)

;; The generic file (SPC f), search (SPC s), quit/session (SPC q) and
;; open/frame (SPC o) leader trees, restored from the old config's Misc
;; Keybinds block.  Entries that belong to a module's own commands stay
;; there (SPC f E in editor, SPC f p in vc-projects, SPC s t/T/C/P in
;; editor's hl-todo block, SPC s y in dev's yasnippet block, the SPC o
;; capture/agenda/tool/debugger entries in org/vc-projects/dev); the
;; bindings below own everything else.  Every target is either a built-in
;; command or resolves through its package's generated autoloads, so the
;; bindings work before the package has loaded -- consult-dir in particular
;; is reachable again (this binding is the only entry point it ships).
(ar/global-leader
  ;; File operations (SPC f).
  "f"     '(:ignore t :wk "file")
  "f f"   '(find-file :wk "Find file")
  "f r"   '(consult-recent-file :wk "Recent files")
  "f s"   '(save-buffer :wk "Save file")
  "f S"   '(write-file :wk "Save as")
  "f l"   '(reload-init-file :wk "Reload config")
  "f e"   '(:ignore t :wk "emacs")
  "f e d" '((lambda () (interactive) (find-file ar/config-dir))
            :wk "Open the configuration root")
  "f e i" '((lambda () (interactive) (find-file user-init-file))
            :wk "Open init.el")
  "f d"   '(consult-dir :wk "Change directory")
  "f j"   '(consult-dir-jump-file :wk "Jump to file in dir")
  ;; Search operations (SPC s).
  "s"     '(:ignore t :wk "search")
  "s c"   '(evil-ex-nohighlight :wk "Clear highlights")
  "s s"   '(consult-line :wk "Search buffer")
  "s S"   '(consult-line-multi :wk "Search buffers")
  "s p"   '(consult-ripgrep :wk "Search project")
  "s d"   '(consult-dir :wk "Search directory")
  "s i"   '(consult-imenu :wk "Imenu")
  "s I"   '(consult-imenu-multi :wk "Imenu multi")
  "s o"   '(consult-outline :wk "Outline")
  "s b"   '(consult-line :wk "Search buffer")
  "s j"   '(evil-show-jumps :wk "Jump list")
  "s m"   '(consult-mark :wk "Mark ring")
  "s e"   '(consult-isearch-history :wk "Search history")
  "s r"   '(vertico-repeat :wk "Repeat last search")
  ;; Session management (SPC q).
  "q"     '(:ignore t :wk "quit/session")
  "q q"   '(save-buffers-kill-terminal :wk "Quit Emacs")
  "q Q"   '(kill-emacs :wk "Quit (no save)")
  "q f"   '(delete-frame :wk "Delete frame")
  "q s"   '(bufferlo-bookmarks-save :wk "Save session")
  "q l"   '(bufferlo-bookmarks-load :wk "Load session")
  ;; Open / applications (SPC o): browser, frames and the elisp REPL.
  "o b"   '(browse-url-of-file :wk "Browser")
  "o f"   '(make-frame :wk "New frame")
  "o F"   '(select-frame-by-name :wk "Select frame")
  "o r"   '(ielm :wk "REPL (Elisp)"))

;; Global zoom bindings.  The `ar/global-zoom-*' commands and their
;; face-remap variables are defined by the ui module, which defers the
;; variable setup to `with-eval-after-load' face-remap so the library only
;; loads on the first zoom command (`global-text-scale-adjust' is
;; autoloaded from face-remap).  A former duplicate of the commands here --
;; plus a top-level setopt of the two variables, which pulled face-remap.el
;; into startup via setopt's custom-load-symbol and made the ui module's
;; deferral dead code -- was removed so each definition exists exactly once
;; and the deferral actually works.
(general-define-key
 "C-=" #'ar/global-zoom-in
 "C--" #'ar/global-zoom-out
 "C-0" #'ar/global-zoom-reset
 "C-<wheel-up>" #'ar/global-zoom-in
 "C-<wheel-down>" #'ar/global-zoom-out)

;; Transient base: rectangular grid alignment for the custom transient menus
;; so they line up inside proportional-font Org buffers.  Deferred: the
;; setting is registered before transient loads, and the transients defined by
;; the editor/org/vc-projects modules pull the library in.
(use-package transient
  :defer t
  :custom
  (transient-align-variable-pitch t))

;; Keystroke discovery in the echo area: one-second idle delay with a fast
;; secondary pickup, alphabetical sort, operator-pending coverage.  Enabled
;; eagerly in :config rather than on `after-init-hook', which never fires when
;; init.el is loaded with -l (the batch smoke tests) -- and which-key is in
;; the design's eager-load budget anyway.  `which-key-mode' is the Emacs 31
;; entry point; there is no `global-which-key-mode'.
(use-package which-key
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'minibuffer)
  (which-key-separator " → ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 30)
  (which-key-allow-multiple-replacements t)
  (which-key-show-operator-state-maps t)
  :config
  (which-key-mode 1))

;; Cross-window jumps and dispatch via ace-window's avy engine.  Deferred
;; through :commands; :custom values are registered with the package unloaded.
;; The old config also enabled a childframe hint mode that ace-window 0.10.0
;; no longer ships (would signal void-function on first use), so it is dropped.
(use-package ace-window
  :commands (ace-window
             ace-swap-window
             ace-delete-window
             ace-delete-other-windows)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; home-row hints
  (aw-scope 'frame)                       ; never reach into other frames
  (aw-background nil)                     ; no buffer dimming under hints
  (aw-dispatch-always t)                  ; dispatch menu even with 1 window
  (aw-ignored-buffers '("*Calc Trail*" " *LV*"
                        treemacs-mode
                        dirvish-mode
                        dirvish-directory-view-mode
                        "*popper*" "*which-key*"))
  :custom-face
  ;; Tokyo-Night colors for legibility of the oversized leading hints.
  (aw-leading-char-face ((t (:foreground "#1a1b26" :background "#e0af68" :weight bold :height 2.5))))
  :config
  ;; Keep the selection characters visible in the mode line.
  (ace-window-display-mode 1))

;; Layout history, wrap-around directional movement and the SPC w leader
;; tree.  evil-window-* commands delegate to windmove, so wrap-around covers
;; both C-w h/j/k/l and the leader bindings below.  winner-mode must be on
;; from startup to record window configurations, hence the eager :config.
(use-package winner
  :custom
  ;; Boring buffers pruned to what this stack produces; *Messages* and
  ;; *Warnings* join because popper owns them as popups.
  (winner-boring-buffers '("*Completions*"
                           "*Compile-Log*"
                           "*Apropos*"
                           "*Help*"
                           "*Buffer List*"
                           "*Ibuffer*"
                           "*Messages*"
                           "*Warnings*"))
  :config
  (winner-mode 1))

(setopt windmove-wrap-around t)

(ar/global-leader
  "w"           '(:ignore t :wk "window")
  ;; Ace window routing.
  "w w"         '(ace-window :wk "Ace window")
  "w d"         '(ace-delete-window :wk "Ace delete window")
  "w m"         '(ace-delete-other-windows :wk "Maximize")
  "w x"         '(ace-swap-window :wk "Ace swap window")
  ;; Native splitting.
  "w s"         '(split-window-below :wk "Split below")
  "w v"         '(split-window-right :wk "Split right")
  ;; Directional navigation.
  "w <left>"    '(evil-window-left :wk "Window left")
  "w <down>"    '(evil-window-down :wk "Window down")
  "w <up>"      '(evil-window-up :wk "Window up")
  "w <right>"   '(evil-window-right :wk "Window right")
  ;; Directional transposition.
  "w S-<left>"  '(evil-window-move-far-left :wk "Move far left")
  "w S-<down>"  '(evil-window-move-very-bottom :wk "Move bottom")
  "w S-<up>"    '(evil-window-move-very-top :wk "Move top")
  "w S-<right>" '(evil-window-move-far-right :wk "Move far right")
  ;; Utilities and layout history.
  "w ="         '(balance-windows :wk "Balance windows")
  "w u"         '(winner-undo :wk "Winner undo")
  "w r"         '(winner-redo :wk "Winner redo")
  ;; The old config bound a name that never existed; this is the real
  ;; command, defined in the core module.
  "w t"         '(window-toggle-split-direction :wk "Toggle split"))

;; Popup routing: ephemeral output buffers render as bottom/echo popups that
;; C-g dismisses.  The reference list below is pruned to the buffer names and
;; major modes this stack actually produces (Emacs 31.1 built-ins plus
;; installed packages): the flycheck/eat/vterm/rg/deadgrep/cargo/translation/
;; dap/rustic/ivy-occur and friends entries of the old list referenced
;; packages that are no longer installed.
;;
;; Activated eagerly in :config for two verified reasons: the old config's
;; (after-init . popper-echo-mode) hook never fires under batch loads, and
;; popper-echo-mode does not enable popper-mode -- popper's window management
;; was effectively inactive in the old config.  popper ships a content-fit
;; height fitter as the `popper-window-height' default
;; (#'popper--fit-window-height, verified in popper.el), so the old config's
;; custom fitter (my-popper-fit-window-height, a fixed third-of-frame
;; fit-window-to-buffer) is deliberately not carried -- behaviorally
;; near-equivalent.  That drop is not listed in design §6.2's ledger, so it
;; is recorded here as the authoritative note.
(defvar popper-open-popup-alist)  ; bound by popper.el; advice runs post-load

(defun ar/popper-close-window-hack (&rest _args)
  "Close the topmost popper popup on `C-g'."
  (when (and (not (region-active-p))
             popper-open-popup-alist)
    (let ((window (caar popper-open-popup-alist))
          (buffer (cdar popper-open-popup-alist)))
      (when (and (window-live-p window)
                 (buffer-live-p buffer)
                 (not (with-current-buffer buffer
                        (derived-mode-p 'eshell-mode
                                        'shell-mode
                                        'term-mode
                                        'ghostel-mode))))
        (delete-window window)))))

(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  (popper-mode-line "")
  (popper-reference-buffers
   '(;; Core output / transient UI
     "\\*Messages\\*$"
     "\\*Warnings\\*$"
     "\\*Backtrace\\*$"
     "\\*Compile-Log\\*$"
     "\\*compilation\\*$"
     "\\*Native-compile-Log\\*$"
     "\\*Completions\\*$"
     "\\*Apropos\\*$"
     "\\*Packages\\*$"
     "\\*Async Shell Command\\*$"
     "\\*Shell Command Output\\*$"
     "Output\\*$"
     "\\*Pp Eval Output\\*$"
     "\\*Pp Macroexpand Output\\*$"
     "^\\*eldoc.*\\*$"
     "^\\*envrc.*\\*$"
     "\\*Local variables\\*$"
     "^\\*transient.*"
     ;; Help / info / docs / menus
     help-mode
     helpful-mode
     Info-mode
     Custom-mode
     woman-mode
     Man-mode
     dictionary-mode
     bookmark-bmenu-mode
     tabulated-list-mode
     Buffer-menu-mode
     special-mode
     ;; Calculators and calendar
     "\\*Calendar\\*$"
     "\\*Calculator\\*$"
     "\\*Calc Trail\\*$"
     ;; Completion and navigation results
     "\\*Embark \\(Collect\\|Live\\|Export\\):.*\\*$"
     "\\*Embark Actions\\*$"
     "\\*Fd\\*$"
     "\\*Find\\*$"
     "\\*Finder\\*$"
     "\\*Occur\\*$"
     "\\*xref\\*$"
     grep-mode
     occur-mode
     flymake-diagnostics-buffer-mode
     flymake-project-diagnostics-mode
     process-menu-mode
     list-environment-mode
     profiler-report-mode
     ;; Org
     "\\*Org Select\\*$"
     "\\*Org Agenda.*\\*$"
     "\\*Agenda Commands\\*$"
     org-agenda-mode
     "\\*Org-Babel Error Output\\*$"
     "\\*Org Export Dispatcher\\*$"
     "\\*Org Note\\*$"
     ;; Shells and processes
     comint-mode
     inferior-python-mode
     "\\*Python\\*$"
     "\\*inferior-python.*\\*$"
     "^\\*.*eshell.*\\*$"
     "^\\*.*shell.*\\*$"
     "^\\*.*terminal.*\\*$"
     "\\*ert\\*$"
     ;; LSP
     "^\\*lsp.*\\*$"
     ;; VC
     "\\*vc-.*\\*$"
     "\\*diff-hl\\*$"
     "\\*ediff-.*\\*$"
     ;; GDB (built-in gud)
     "\\*breakpoints of.*\\*$"
     "\\*gdb-.*\\*$"))
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  ;; C-g closes the topmost popup; the advice is the only way in, since no
  ;; command binding owns that gesture.
  (advice-add #'keyboard-quit :before #'ar/popper-close-window-hack))

(provide 'keybinds-config)
;;; keybinds-config.el ends here
