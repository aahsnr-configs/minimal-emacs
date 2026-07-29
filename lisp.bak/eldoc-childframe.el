;;; eldoc-childframe.el --- On-demand childframe documentation viewer -*- lexical-binding: t; -*-

;; Author: Ahsanur Rahman (Forked from eldoc-box by Yuan Fu / casouri)
;; Version: 2.2.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: docs, convenience, tools

;;; Commentary:

;; A modernized, stripped-down fork of `eldoc-box' optimized for Emacs 31.
;; Hardened against redisplay loops, regex backtracking, and C-stack overflows.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'face-remap)

;; Compiler silencing for external/optional dependencies.
(defvar corfu--frame)
(defvar global-tab-line-mode)
(defvar goto-address-url-regexp)
(defvar markdown-line-break-face)
(declare-function browse-url-at-mouse "browse-url" (event))

;;; ---------------------------------------------------------------------------
;;; Customization Group & Faces
;;; ---------------------------------------------------------------------------

(defgroup eldoc-childframe nil
  "Display Eldoc documentation in a floating childframe."
  :prefix "eldoc-childframe-"
  :group 'eldoc)

(defface eldoc-childframe-border
  '((((background dark))  :background "#292e42")
    (((background light)) :background "#a6accd"))
  "Border face for the documentation childframe."
  :group 'eldoc-childframe)

(defface eldoc-childframe-body
  '((((background dark))  :background "#24283b" :foreground "#c0caf5")
    (((background light)) :background "#f8f8f2" :foreground "#383a42"))
  "Body face for documentation text in the childframe."
  :group 'eldoc-childframe)

(defface eldoc-childframe-header
  '((((background dark))  :foreground "#7aa2f7" :weight bold :height 1.1)
    (((background light)) :foreground "#4078f2" :weight bold :height 1.1))
  "Face for the symbol header line in the childframe."
  :group 'eldoc-childframe)

(defface eldoc-childframe-markdown-separator
  '((t (:inherit shadow :strike-through t :height 0.4 :extend t)))
  "Face for Markdown horizontal rule separators."
  :group 'eldoc-childframe)

(defface eldoc-childframe-url
  '((t (:inherit link)))
  "Face for clickable URLs in documentation."
  :group 'eldoc-childframe)

;;; ---------------------------------------------------------------------------
;;; User Options & Internal State
;;; ---------------------------------------------------------------------------

(defcustom eldoc-childframe-clear-with-C-g t
  "If non-nil, \\[keyboard-quit] hides the documentation childframe."
  :type 'boolean :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-width 900
  "Maximum width of the documentation childframe in pixels."
  :type '(choice integer function) :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-height 800
  "Maximum height of the documentation childframe in pixels."
  :type '(choice integer function) :group 'eldoc-childframe)

(defcustom eldoc-childframe-offset '(16 16 16)
  "Pixel offsets for the childframe: (LEFT RIGHT TOP)."
  :type '(list integer integer integer) :group 'eldoc-childframe)

(defcustom eldoc-childframe-prefer-above nil
  "If non-nil, prefer placing the childframe above point."
  :type 'boolean :group 'eldoc-childframe)

(defcustom eldoc-childframe-show-header t
  "If non-nil, show the symbol name as a header line in the childframe."
  :type 'boolean :group 'eldoc-childframe)

(defvar eldoc-childframe--frame nil "The childframe used to display documentation.")
(defvar eldoc-childframe--buffer " *eldoc-childframe*" "Name of the buffer rendered inside the childframe.")
(defvar eldoc-childframe--last-point 0 "Buffer position where the childframe was last summoned.")
(defvar eldoc-childframe--last-buffer nil "Buffer from which the childframe was last summoned.")
(defvar eldoc-childframe--last-window-start nil "Window start position to detect scrolling.")
(defvar eldoc-childframe--main-frame nil "The main frame to return focus to after childframe interaction.")
(defvar-local eldoc-childframe--old-display-functions nil "Saved value of `eldoc-display-functions'.")

;;; ---------------------------------------------------------------------------
;;; Frame Parameters (STABILITY HARDENED)
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe-frame-parameters
  '((left                . -1)
    (top                 . -1)
    (width               . 0)
    (height              . 0)
    (no-accept-focus     . t)
    (no-focus-on-map     . t)
    (min-width           . 0)
    (min-height          . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars   . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe         . 12)
    (right-fringe        . 12)
    (menu-bar-lines      . 0)
    (tool-bar-lines      . 0)
    (tab-bar-lines       . 0)
    (line-spacing        . 0.25)
    (unsplittable        . t)
    (undecorated         . t)
    (visibility          . nil)
    (mouse-wheel-frame   . nil)
    (no-other-frame      . t)
    (cursor-type         . nil)
    (no-special-glyphs   . t)
    (desktop-dont-save   . t))
  "Frame parameters for the documentation childframe.
CRITICAL FIX: Removed `drag-internal-border' and `inhibit-double-buffering'.
These parameters cause fatal redisplay loops and C-level deadlocks in Emacs 31
when combined with `window-text-pixel-size' geometry calculations.")

;;; ---------------------------------------------------------------------------
;;; Hooks
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe-frame-hook nil
  "Hook run after the childframe is set up, before it is made visible.")

(defvar eldoc-childframe-buffer-hook
  '(eldoc-childframe--strip-breadcrumb
    eldoc-childframe--strip-invisible-props
    eldoc-childframe--prettify-markdown-separator
    eldoc-childframe--replace-en-space
    eldoc-childframe--remove-linked-images
    eldoc-childframe--remove-noise-chars
    eldoc-childframe--fontify-html
    eldoc-childframe--make-smaller-empty-lines
    eldoc-childframe--make-clickable-links
    eldoc-childframe--condense-large-newline-gaps)
  "Hook run after the documentation buffer content is inserted.")

;;; ---------------------------------------------------------------------------
;;; Frame Visibility & Lifecycle
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--frame-visible-p ()
  (and eldoc-childframe--frame
       (frame-live-p eldoc-childframe--frame)
       (frame-visible-p eldoc-childframe--frame)))

(defun eldoc-childframe--childframe-supported-p ()
  (or (display-graphic-p) (featurep 'tty-child-frames)))

;;;###autoload
(defun eldoc-childframe-quit-frame ()
  "Hide the documentation childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (make-frame-invisible eldoc-childframe--frame t)))

(defun eldoc-childframe--quit-frame-not-in-childframe ()
  "Hide the childframe unless point is inside it."
  (when (and eldoc-childframe-clear-with-C-g
             (not (eq (selected-frame) eldoc-childframe--frame)))
    (eldoc-childframe-quit-frame)))

;;;###autoload
(defun eldoc-childframe-scroll-up (arg)
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame (scroll-up arg))))

;;;###autoload
(defun eldoc-childframe-scroll-down (arg)
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame (scroll-down arg))))

;;;###autoload
(defun eldoc-childframe-focus-frame ()
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (setq eldoc-childframe--main-frame (selected-frame))
    (set-frame-parameter eldoc-childframe--frame 'no-accept-focus nil)
    (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map nil)
    (select-frame-set-input-focus eldoc-childframe--frame)
    (with-selected-frame eldoc-childframe--frame
      (setq-local cursor-type 'bar)
      (local-set-key (kbd "q") #'eldoc-childframe-unfocus-frame))))

;;;###autoload
(defun eldoc-childframe-unfocus-frame ()
  (interactive)
  (when (eq (selected-frame) eldoc-childframe--frame)
    (setq-local cursor-type nil)
    (set-frame-parameter eldoc-childframe--frame 'no-accept-focus t)
    (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map t)
    (when (and eldoc-childframe--main-frame (frame-live-p eldoc-childframe--main-frame))
      (select-frame-set-input-focus eldoc-childframe--main-frame))))

;;; ---------------------------------------------------------------------------
;;; Position Engine
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--point-position-relative-to-frame (&optional point window)
  (unless point (setq point (window-point window)))
  (let* ((pos (pos-visible-in-window-p point window t))
         (pos (or pos (list 0 0)))
         (x (car pos)) (en (frame-char-width))
         (y (cadr pos)) (edges (window-edges window nil nil t)))
    (cons (+ x (car edges) en) (+ y (cadr edges)))))

(defun eldoc-childframe--calc-position (width height)
  (let* ((point-pos (eldoc-childframe--point-position-relative-to-frame))
         (x (car point-pos)) (y (cdr point-pos))
         (em (frame-char-height))
         (frame-w (frame-inner-width)) (frame-h (frame-inner-height))
         (final-x (if (< (- frame-w width) x) (max 0 (- frame-w width 16)) x))
         (final-y (if eldoc-childframe-prefer-above
                      (if (< y height) (min (- frame-h height) (+ y em)) (- y height))
                    (if (< (- frame-h height) y) (max 0 (- y height)) (+ y em)))))
    (when (and (boundp 'corfu--frame) corfu--frame
               (frame-live-p corfu--frame) (frame-visible-p corfu--frame))
      (setq final-x (+ (car (frame-position corfu--frame))
                       (frame-pixel-width corfu--frame) 4)))
    (cons final-x final-y)))

;;; ---------------------------------------------------------------------------
;;; Frame Construction & Geometry (CRITICAL STABILITY FIXES)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--resolve-max (val)
  (if (functionp val) (funcall val) val))

(defun eldoc-childframe--update-geometry (frame window)
  "Resize and reposition FRAME's WINDOW to fit its content.
Wrapped in `ignore-errors' to prevent hard crashes on malformed LSP payloads."
  (let* ((parent-frame (frame-parent frame))
         (max-w (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-width))
         (max-h (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-height))
         ;; CRITICAL FIX: Guard against C-level hangs on complex display properties.
         (size (ignore-errors (window-text-pixel-size window nil nil max-w max-h t)))
         (frame-resize-pixelwise t))
    (when size
      (let* ((width (+ (car size) (frame-char-width frame)))
             (height (cdr size))
             (width (min width (- (frame-pixel-width parent-frame) 32)))
             (height (min height (- (frame-pixel-height parent-frame) 32)))
             (pos (eldoc-childframe--calc-position width height)))
        (set-frame-size frame width height t)
        (set-frame-position frame (car pos) (cdr pos))))))

(defun eldoc-childframe--get-frame (buffer)
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-childframe-frame-parameters
                            `((minibuffer . ,(minibuffer-window))
                              (background-color . ,(face-background 'eldoc-childframe-body nil t)))))
         window frame (main-frame (selected-frame)))
    (if (and eldoc-childframe--frame (frame-live-p eldoc-childframe--frame))
        (progn
          (setq frame eldoc-childframe--frame)
          (setq window (frame-selected-window frame))
          (set-frame-parameter frame 'parent-frame main-frame)
          (set-frame-parameter frame 'no-accept-focus t)
          (set-frame-parameter frame 'no-focus-on-map t))
      (setq window (display-buffer-in-child-frame
                    buffer `((child-frame-parameters . ,parameter)
                             (no-other-window . t)
                             (no-delete-other-windows . t))))
      (setq frame (window-frame window)))
    (set-face-attribute 'fringe frame :background 'unspecified :inherit 'eldoc-childframe-body)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-attribute 'internal-border frame :inherit 'eldoc-childframe-border)
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border
                           (face-attribute 'eldoc-childframe-border :background nil t) frame))
    (eldoc-childframe--update-geometry frame window)
    (set-window-margins window nil nil)
    (setq eldoc-childframe--frame frame)
    (with-selected-frame frame
      (set-frame-parameter nil 'left-fringe 12)
      (set-frame-parameter nil 'right-fringe 12)
      (run-hook-with-args 'eldoc-childframe-frame-hook main-frame))
    (make-frame-visible frame)))

;;; ---------------------------------------------------------------------------
;;; Buffer Setup (STABILITY HARDENED)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--buffer-setup ()
  "Set up the documentation buffer for clean rendering."
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (when (bound-and-true-p global-tab-line-mode) (setq-local tab-line-format nil))
  (setq-local show-trailing-whitespace nil)
  (setq-local cursor-type nil)
  (buffer-face-set 'eldoc-childframe-body)
  
  ;; CRITICAL FIX: Disable `visual-line-mode'.
  ;; `visual-line-mode' combined with `window-text-pixel-size' and complex
  ;; markdown display properties causes Emacs 31 redisplay engine to hang
  ;; or crash (C-stack overflow). Use native word-wrap instead.
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local wrap-prefix '(space :width 1))
  (setq-local line-prefix '(space :width 1))
  
  (setq-local window-configuration-change-hook nil)
  (when (boundp 'window-state-change-functions)
    (setq-local window-state-change-functions nil))
  (setq-local window-size-change-functions nil))

;;; ---------------------------------------------------------------------------
;;; Display Entry Point
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--display (str)
  (let ((doc-buffer (get-buffer-create eldoc-childframe--buffer)))
    (with-current-buffer doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (eldoc-childframe--buffer-setup)
        (run-hooks 'eldoc-childframe-buffer-hook)))
    (let ((frame (eldoc-childframe--get-frame doc-buffer)))
      (setq eldoc-childframe--last-point (point))
      (setq eldoc-childframe--last-buffer (current-buffer))
      (setq eldoc-childframe--last-window-start (window-start))
      (make-frame-visible frame))))

;;; ---------------------------------------------------------------------------
;;; Payload Filtering & Routing
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--filter-flymake (docs)
  (cl-remove-if (lambda (doc) (eq (plist-get (cdr doc) :origin) 'flymake-eldoc-function)) docs))

(defun eldoc-childframe--filter-breadcrumb (docs)
  (cl-remove-if (lambda (doc)
                  (or (eq (plist-get (cdr doc) :origin) 'eldoc-breadcrumb)
                      (string-match-p " \u203a " (car doc))))
                docs))

(defun eldoc-childframe--compose-doc (doc)
  (let ((thing (plist-get (cdr doc) :thing))
        (face  (plist-get (cdr doc) :face)))
    (concat (when (and eldoc-childframe-show-header thing)
              (concat (propertize (format "%s" thing) 'face (or face 'eldoc-childframe-header)) "\n"))
            (car doc))))

(defun eldoc-childframe--route-display (docs interactive)
  (when interactive
    (let* ((filtered (eldoc-childframe--filter-breadcrumb (eldoc-childframe--filter-flymake docs)))
           (composed (string-join (mapcar #'eldoc-childframe--compose-doc filtered) "\n"))
           (doc (string-trim composed)))
      (if (eldoc-childframe--childframe-supported-p)
          (eldoc-childframe--display (if (string-empty-p doc) "No documentation available at point." doc))
        (eldoc-display-in-echo-area filtered interactive)))))

;;; ---------------------------------------------------------------------------
;;; Buffer Prettifiers (REGEX STABILITY HARDENED)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--strip-breadcrumb ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.* \u203a .*$" nil t)
      (delete-region (line-beginning-position) (min (1+ (line-end-position)) (point-max))))))

(defun eldoc-childframe--strip-invisible-props ()
  (remove-text-properties (point-min) (point-max) '(invisible nil)))

(defun eldoc-childframe--prettify-markdown-separator ()
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (let* ((beg (prop-match-beginning prop)) (end (prop-match-end prop))
               (end+nl (save-excursion (goto-char end) (min (1+ (line-end-position)) (point-max)))))
          (add-text-properties beg end '(display " "))
          (add-text-properties beg end+nl '(face eldoc-childframe-markdown-separator)))))
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:---\\|\\*\\*\\*\\|___\\)[ \t]*$" nil t)
      (let ((beg (line-beginning-position)) (end (min (1+ (line-end-position)) (point-max))))
        (add-text-properties beg (line-end-position) '(display " "))
        (add-text-properties beg end '(face eldoc-childframe-markdown-separator))))))

(defun eldoc-childframe--replace-en-space ()
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (when (facep 'markdown-line-break-face)
    (face-remap-set-base 'markdown-line-break-face '(:inherit default))))

(defun eldoc-childframe--make-smaller-empty-lines ()
  "Make empty lines half-height. FIXED: Prevents zero-length match infinite loops."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*$" nil t)
      (let ((beg (line-beginning-position))
            (end (min (1+ (line-end-position)) (point-max))))
        (when (> end beg)
          (add-text-properties beg end '(face (:height 0.5))))))))

(defun eldoc-childframe--make-clickable-links ()
  (save-excursion
    (goto-char (point-min))
    (when (boundp 'goto-address-url-regexp)
      (let (case-fold-search)
        (while (re-search-forward goto-address-url-regexp nil t)
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (add-face-text-property beg end 'eldoc-childframe-url)
            (put-text-property beg end 'mouse-face 'highlight)
            (let ((map (make-sparse-keymap)))
              (define-key map [down-mouse-1] #'browse-url-at-mouse)
              (put-text-property beg end 'keymap map))))))))

(defun eldoc-childframe--condense-large-newline-gaps ()
  "Condense runs of 2+ consecutive blank lines.
CRITICAL FIX: Replaced catastrophic `rx' backtracking with safe O(N) regex."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*\n[ \t]*\n" nil t)
      (replace-match "\n\n")
      (add-text-properties (1- (point)) (point) '(face (:height 0.4))))))

(defun eldoc-childframe--remove-linked-images ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]" "(" (+? anychar) ")") nil t)
      (replace-match ""))))

(defun eldoc-childframe--remove-noise-chars ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

(defun eldoc-childframe--fontify-html ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx bol (group "<h" digit ">") (group (*? anychar)) (group "</h" digit ">") eol) nil t)
      (add-text-properties (match-beginning 2) (match-end 2) '(face (:weight bold) font-lock-face (:weight bold)))
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    (goto-char (point-min))
    (while (re-search-forward (rx (group "<p>") (group (*? anychar)) (group "</p>")) nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'display
                         (pcase (match-string 0) ("&lt;" "<") ("&gt;" ">") ("&nbsp;" " "))))))

;;; ---------------------------------------------------------------------------
;;; Cursor Tracking & Auto-Hide (PERFORMANCE HARDENED)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--follow-cursor ()
  "Update or hide the childframe based on cursor movement.
CRITICAL FIX: Only recalculates geometry if the window actually scrolled.
Prevents main-thread blocking on every single command."
  (if (eq (point) eldoc-childframe--last-point)
      (when (eldoc-childframe--frame-visible-p)
        (let ((current-start (window-start)))
          (unless (eq current-start eldoc-childframe--last-window-start)
            (setq eldoc-childframe--last-window-start current-start)
            (ignore-errors
              (eldoc-childframe--update-geometry
               eldoc-childframe--frame
               (frame-selected-window eldoc-childframe--frame))))))
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe--hide-on-buffer-change ()
  (when (and (eldoc-childframe--frame-visible-p)
             eldoc-childframe--last-buffer
             (not (eq (current-buffer) eldoc-childframe--last-buffer))
             (not (eq (current-buffer) (get-buffer eldoc-childframe--buffer))))
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe--cleanup-on-buffer-kill ()
  (when (and (eldoc-childframe--frame-visible-p)
             (eq (current-buffer) eldoc-childframe--last-buffer))
    (eldoc-childframe-quit-frame)))

;;; ---------------------------------------------------------------------------
;;; Interactive Commands & Minor Mode
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun eldoc-childframe-help-at-point ()
  (interactive)
  (if (eldoc-childframe--frame-visible-p)
      (eldoc-childframe-quit-frame)
    (eldoc-print-current-symbol-info t)
    (setq eldoc-childframe--last-point (point))))

;;;###autoload
(defun eldoc-childframe-glance ()
  (interactive)
  (eldoc-childframe-help-at-point)
  (add-hook 'post-command-hook #'eldoc-childframe--glance-cleanup))

(defun eldoc-childframe--glance-cleanup ()
  (unless (memq this-command '(eldoc-childframe-glance eldoc-childframe-help-at-point))
    (eldoc-childframe-quit-frame)
    (remove-hook 'post-command-hook #'eldoc-childframe--glance-cleanup)))

(defun eldoc-childframe--enable ()
  (setq-local eldoc-childframe--old-display-functions eldoc-display-functions)
  (setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
  (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
  (add-hook 'post-command-hook #'eldoc-childframe--follow-cursor nil t)
  (add-hook 'post-command-hook #'eldoc-childframe--hide-on-buffer-change nil t)
  (add-hook 'kill-buffer-hook #'eldoc-childframe--cleanup-on-buffer-kill nil t)
  (when eldoc-childframe-clear-with-C-g
    (advice-add #'keyboard-quit :before #'eldoc-childframe--quit-frame-not-in-childframe)))

(defun eldoc-childframe--disable ()
  (setq-local eldoc-display-functions eldoc-childframe--old-display-functions)
  (setq-local eldoc-childframe--old-display-functions nil)
  (add-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area nil t)
  (remove-hook 'post-command-hook #'eldoc-childframe--follow-cursor t)
  (remove-hook 'post-command-hook #'eldoc-childframe--hide-on-buffer-change t)
  (remove-hook 'kill-buffer-hook #'eldoc-childframe--cleanup-on-buffer-kill t)
  (advice-remove #'keyboard-quit #'eldoc-childframe--quit-frame-not-in-childframe)
  (when (and eldoc-childframe--frame (frame-live-p eldoc-childframe--frame))
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

;;;###autoload
(define-minor-mode eldoc-childframe-hover-at-point-mode
  "Display Eldoc documentation in a childframe on demand (M-h)."
  :lighter " eldoc-cf" :global nil
  (if eldoc-childframe-hover-at-point-mode (eldoc-childframe--enable) (eldoc-childframe--disable)))

;;; ---------------------------------------------------------------------------
;;; Tab-bar / Tab-line Compatibility
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe-reset-frame ()
  (interactive)
  (when eldoc-childframe--frame
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

(with-eval-after-load 'tab-bar (add-hook 'tab-bar-mode-hook #'eldoc-childframe-reset-frame))
(with-eval-after-load 'tab-line (add-hook 'tab-line-mode-hook #'eldoc-childframe-reset-frame))

(provide 'eldoc-childframe)
;;; eldoc-childframe.el ends here