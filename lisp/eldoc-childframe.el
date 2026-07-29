;;; eldoc-childframe.el --- On-demand childframe documentation viewer -*- lexical-binding: t; -*-

;; Author: Ahsanur Rahman (Forked from eldoc-box by Yuan Fu / casouri)
;; Version: 2.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: docs, convenience, tools

;;; Commentary:

;; A modernized, stripped-down fork of `eldoc-box' optimized for Emacs 31.
;;
;; Design Principles:
;;
;; - Keybinding-Only Activation: The childframe spawns exclusively on demand
;;   via `eldoc-childframe-help-at-point' (bound to M-h).  Idle hover does
;;   NOT trigger the childframe; the minor mode silences native Eldoc
;;   echo-area and doc-buffer popups in prog-mode so the echo area stays
;;   clean.
;;
;; - Flymake & Breadcrumb Firewall: Inspects the `:origin' plist key on
;;   each Eldoc doc item and drops payloads from `flymake-eldoc-function'
;;   and breadcrumb injectors before rendering.
;;
;; - TTY Degradation: On non-graphical frames without `tty-child-frames',
;;   falls back to truncated echo-area display via
;;   `eldoc-display-in-echo-area'.
;;
;; - Evil Motion Fix: Spatial debouncing (point comparison) replaces the
;;   legacy 0.5s `eldoc-idle-delay' inhibition trap.
;;
;; - Corfu Collision Avoidance: Shifts the childframe rightward when a
;;   `corfu--frame' is visible to prevent overlap.
;;
;; - Buffer-Change Auto-Hide: Instantly vanishes when the user switches
;;   buffers or windows.
;;
;; - Minimal Border: 1px internal-border-width provides subtle visual
;;   separation without heavy padding.
;;
;; - Cohesive Rendering: Compact spacing, clickable URLs, prettified
;;   Markdown separators, and half-height empty lines for a clean,
;;   lsp-ui-doc-inspired aesthetic.
;;
;; - global-tab-line-mode Suppression: Prevents tab-line bleeding into
;;   the childframe window.
;;
;; Designed for use with `eglot-documentation-renderer' set to
;; `gfm-view-mode' and the doom-tokyo-night theme.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'face-remap)

;;; ---------------------------------------------------------------------------
;;; Customization Group
;;; ---------------------------------------------------------------------------

(defgroup eldoc-childframe nil
  "Display Eldoc documentation in a floating childframe."
  :prefix "eldoc-childframe-"
  :group 'eldoc)

;;; ---------------------------------------------------------------------------
;;; Faces
;;; ---------------------------------------------------------------------------

(defface eldoc-childframe-border
  '((((background dark))  :background "#292e42")
    (((background light)) :background "#a6accd"))
  "Border face for the documentation childframe.
Only the `:background' attribute is used to paint the 1px internal
border surrounding the documentation content."
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
;;; User Options
;;; ---------------------------------------------------------------------------

(defcustom eldoc-childframe-clear-with-C-g t
  "If non-nil, \\[keyboard-quit] hides the documentation childframe."
  :type 'boolean
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-width 900
  "Maximum width of the documentation childframe in pixels.
Can be a number or a function returning a number."
  :type '(choice integer function)
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-height 800
  "Maximum height of the documentation childframe in pixels.
Can be a number or a function returning a number."
  :type '(choice integer function)
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-offset '(16 16 16)
  "Pixel offsets for the childframe: (LEFT RIGHT TOP).
LEFT and RIGHT are used by the upper-corner position function.
TOP is the vertical offset from the frame edge."
  :type '(list (integer :tag "Left")
               (integer :tag "Right")
               (integer :tag "Top"))
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-prefer-above nil
  "If non-nil, prefer placing the childframe above point.
When nil (default), the frame appears below point unless there is
insufficient space below, in which case it flips above."
  :type 'boolean
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-show-header t
  "If non-nil, show the symbol name as a header line in the childframe."
  :type 'boolean
  :group 'eldoc-childframe)

;;; ---------------------------------------------------------------------------
;;; Internal State
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe--frame nil
  "The childframe used to display documentation.")

(defvar eldoc-childframe--buffer " *eldoc-childframe*"
  "Name of the buffer rendered inside the childframe.")

(defvar eldoc-childframe--last-point 0
  "Buffer position where the childframe was last summoned.")

(defvar eldoc-childframe--last-buffer nil
  "Buffer from which the childframe was last summoned.")

(defvar eldoc-childframe--main-frame nil
  "The main frame to return focus to after childframe interaction.")

(defvar-local eldoc-childframe--old-display-functions nil
  "Saved value of `eldoc-display-functions' before mode activation.")

;;; ---------------------------------------------------------------------------
;;; Frame Parameters
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
    (tab-bar-lines-keep-state . 0)
    (line-spacing        . 0.25)
    (unsplittable        . t)
    (undecorated         . t)
    (visibility          . nil)
    (mouse-wheel-frame   . nil)
    (no-other-frame      . t)
    (cursor-type         . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs   . t)
    (desktop-dont-save   . t))
  "Frame parameters for the documentation childframe.
Modeled after eldoc-box and lsp-ui-doc with Emacs 31 best practices.
The 1px `internal-border-width' provides minimal but clear visual
separation from the parent frame.  Fringes are set to 12px to provide
breathing room between text and border without excessive padding.")

;;; ---------------------------------------------------------------------------
;;; Hooks
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe-frame-hook nil
  "Hook run after the childframe is set up, before it is made visible.
Each function receives the parent frame as its sole argument.")

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
  "Hook run after the documentation buffer content is inserted.
Each function operates on the current buffer (the doc buffer).")

;;; ---------------------------------------------------------------------------
;;; Frame Visibility Predicates
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--frame-visible-p ()
  "Return non-nil if the documentation childframe is currently visible."
  (and eldoc-childframe--frame
       (frame-live-p eldoc-childframe--frame)
       (frame-visible-p eldoc-childframe--frame)))

(defun eldoc-childframe--childframe-supported-p ()
  "Return non-nil if childframes can be displayed on the current frame."
  (or (display-graphic-p)
      (featurep 'tty-child-frames)))

;;; ---------------------------------------------------------------------------
;;; Frame Lifecycle Commands
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun eldoc-childframe-quit-frame ()
  "Hide the documentation childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (make-frame-invisible eldoc-childframe--frame t)))

(defun eldoc-childframe--quit-frame-not-in-childframe ()
  "Hide the childframe unless point is inside it.
Used as `:before' advice on `keyboard-quit'."
  (when (and eldoc-childframe-clear-with-C-g
             (not (eq (selected-frame) eldoc-childframe--frame)))
    (eldoc-childframe-quit-frame)))

;;;###autoload
(defun eldoc-childframe-scroll-up (arg)
  "Scroll up ARG lines in the documentation childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-up arg))))

;;;###autoload
(defun eldoc-childframe-scroll-down (arg)
  "Scroll down ARG lines in the documentation childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-down arg))))

;;;###autoload
(defun eldoc-childframe-focus-frame ()
  "Move input focus to the documentation childframe for reading."
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
  "Return focus to the main frame and restore childframe parameters."
  (interactive)
  (when (eq (selected-frame) eldoc-childframe--frame)
    (setq-local cursor-type nil)
    ;; Restore no-focus parameters to prevent stale state on next spawn.
    (set-frame-parameter eldoc-childframe--frame 'no-accept-focus t)
    (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map t)
    (when (and eldoc-childframe--main-frame
               (frame-live-p eldoc-childframe--main-frame))
      (select-frame-set-input-focus eldoc-childframe--main-frame))))

;;; ---------------------------------------------------------------------------
;;; Position Engine (4-Quadrant)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--point-position-relative-to-frame (&optional point window)
  "Return (X . Y) pixel coordinate of POINT in WINDOW relative to the frame.
WINDOW nil means selected window.  POINT nil means `window-point'."
  (unless point
    (setq point (window-point window)))
  (let* ((pos (pos-visible-in-window-p point window t))
         (x (car pos))
         (en (frame-char-width))
         (y (cadr pos))
         (edges (window-edges window nil nil t)))
    ;; Add en to x for cursor width offset (eldoc-box HACK).
    (cons (+ x (car edges) en)
          (+ y (cadr edges)))))

(defun eldoc-childframe--calc-position (width height)
  "Calculate (X . Y) for a childframe of WIDTH and HEIGHT pixels.
Implements 4-quadrant logic:
- Horizontal: if not enough space on right, shift left.
- Vertical: if `eldoc-childframe-prefer-above', try above first;
  otherwise try below first.  Flip if insufficient space."
  (let* ((point-pos (eldoc-childframe--point-position-relative-to-frame))
         (x (car point-pos))
         (y (cdr point-pos))
         (em (frame-char-height))
         (frame-w (frame-inner-width))
         (frame-h (frame-inner-height))
         ;; Horizontal: clamp to frame boundaries.
         (final-x (if (< (- frame-w width) x)
                      (max 0 (- frame-w width 16))
                    x))
         ;; Vertical: 4-quadrant flip logic.
         (final-y
          (if eldoc-childframe-prefer-above
              ;; Prefer above: if not enough space above, go below.
              (if (< y height)
                  (min (- frame-h height) (+ y em))
                (- y height))
            ;; Prefer below: if not enough space below, go above.
            (if (< (- frame-h height) y)
                (max 0 (- y height))
              (+ y em)))))
    ;; Corfu collision avoidance: shift right of the Corfu popup.
    (when (and (boundp 'corfu--frame)
               corfu--frame
               (frame-live-p corfu--frame)
               (frame-visible-p corfu--frame))
      (setq final-x (+ (car (frame-position corfu--frame))
                       (frame-pixel-width corfu--frame)
                       4)))
    (cons final-x final-y)))

;;; ---------------------------------------------------------------------------
;;; Frame Construction & Geometry
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--resolve-max (val)
  "Resolve VAL which can be a number or function returning a number."
  (if (functionp val) (funcall val) val))

(defun eldoc-childframe--update-geometry (frame window)
  "Resize and reposition FRAME's WINDOW to fit its content."
  (let* ((parent-frame (frame-parent frame))
         (max-w (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-width))
         (max-h (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-height))
         (size (window-text-pixel-size window nil nil max-w max-h t))
         (width (+ (car size) (frame-char-width frame)))
         (height (cdr size))
         ;; Clamp to parent frame bounds.
         (width (min width (- (frame-pixel-width parent-frame) 32)))
         (height (min height (- (frame-pixel-height parent-frame) 32)))
         (frame-resize-pixelwise t)
         (pos (eldoc-childframe--calc-position width height)))
    (set-frame-size frame width height t)
    (set-frame-position frame (car pos) (cdr pos))))

(defun eldoc-childframe--get-frame (buffer)
  "Return a childframe displaying BUFFER, creating one if needed."
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-childframe-frame-parameters
                            `((minibuffer . ,(minibuffer-window))
                              (background-color .
                                                ,(face-background 'eldoc-childframe-body nil t)))))
         window frame
         (main-frame (selected-frame)))
    (if (and eldoc-childframe--frame
             (frame-live-p eldoc-childframe--frame))
        (progn
          (setq frame eldoc-childframe--frame)
          (setq window (frame-selected-window frame))
          (set-frame-parameter frame 'parent-frame main-frame)
          ;; FIX: Restore no-focus parameters in case focus-frame was used.
          (set-frame-parameter frame 'no-accept-focus t)
          (set-frame-parameter frame 'no-focus-on-map t))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter)
                      (no-other-window . t)
                      (no-delete-other-windows . t))))
      (setq frame (window-frame window)))
    ;; Theme the fringe to match body (prevents parent fringe bleed).
    (set-face-attribute 'fringe frame
                        :background 'unspecified
                        :inherit 'eldoc-childframe-body)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    ;; Theme the internal border (1px) to the border face.
    (set-face-attribute 'internal-border frame
                        :inherit 'eldoc-childframe-border)
    ;; Emacs 28+ child-frame-border face.
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border
                           (face-attribute 'eldoc-childframe-border
                                           :background nil t)
                           frame))
    ;; Geometry
    (eldoc-childframe--update-geometry frame window)
    (set-window-margins window nil nil)
    (setq eldoc-childframe--frame frame)
    ;; Frame-local setup
    (with-selected-frame frame
      (set-frame-parameter nil 'left-fringe 12)
      (set-frame-parameter nil 'right-fringe 12)
      (run-hook-with-args 'eldoc-childframe-frame-hook main-frame))
    (make-frame-visible frame)))

;;; ---------------------------------------------------------------------------
;;; Buffer Setup
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--buffer-setup ()
  "Set up the documentation buffer for clean rendering."
  ;; Suppress all chrome for a clean floating tooltip.
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  ;; Suppress global-tab-line-mode bleeding into childframe.
  (when (bound-and-true-p global-tab-line-mode)
    (setq-local tab-line-format nil))
  ;; Suppress trailing whitespace highlighting from prog-mode hooks.
  (setq-local show-trailing-whitespace nil)
  ;; WORKAROUND: cursor-type nil sometimes still shows (eldoc-box issue #66).
  (setq-local cursor-type nil)
  ;; Apply body face to entire buffer.
  (buffer-face-set 'eldoc-childframe-body)
  ;; Enable visual-line-mode for wrapping.
  (visual-line-mode 1)
  ;; Add comfortable line spacing for readability.
  (setq-local line-spacing 0.25)
  ;; Set wrap/line prefixes for consistent indentation of wrapped lines.
  (setq-local wrap-prefix '(space :width 1))
  (setq-local line-prefix '(space :width 1))
  ;; Suppress window-state noise.
  (setq-local window-configuration-change-hook nil)
  (when (boundp 'window-state-change-functions)
    (setq-local window-state-change-functions nil))
  (setq-local window-size-change-functions nil))

;;; ---------------------------------------------------------------------------
;;; Display Entry Point
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--display (str)
  "Display STR in the documentation childframe."
  (let ((doc-buffer (get-buffer-create eldoc-childframe--buffer)))
    (with-current-buffer doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (eldoc-childframe--buffer-setup)
        ;; Run the prettify hook chain.
        (run-hooks 'eldoc-childframe-buffer-hook)))
    (let ((frame (eldoc-childframe--get-frame doc-buffer)))
      (setq eldoc-childframe--last-point (point))
      (setq eldoc-childframe--last-buffer (current-buffer))
      (make-frame-visible frame))))

;;; ---------------------------------------------------------------------------
;;; Payload Filtering & Routing
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--filter-flymake (docs)
  "Remove Flymake diagnostic payloads from DOCS.
Each element of DOCS is (STRING . PLIST); we inspect the :origin key."
  (cl-remove-if
   (lambda (doc)
     (eq (plist-get (cdr doc) :origin) 'flymake-eldoc-function))
   docs))

(defun eldoc-childframe--filter-breadcrumb (docs)
  "Remove breadcrumb payloads from DOCS.
Breadcrumbs carry :origin `eldoc-breadcrumb' or contain the separator."
  (cl-remove-if
   (lambda (doc)
     (or (eq (plist-get (cdr doc) :origin) 'eldoc-breadcrumb)
         (string-match-p " › " (car doc))))
   docs))

(defun eldoc-childframe--compose-doc (doc)
  "Compose a single DOC item (STRING . PLIST) into a display string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face  (plist-get (cdr doc) :face)))
    (concat (when (and eldoc-childframe-show-header thing)
              (concat (propertize (format "%s" thing)
                                  'face (or face 'eldoc-childframe-header))
                      "\n"))
            (car doc))))

(defun eldoc-childframe--route-display (docs interactive)
  "Route DOCS to the childframe only when INTERACTIVE is non-nil.
This ensures the childframe spawns exclusively on explicit keybinding
invocation, while silencing native Eldoc hover popups in prog-mode."
  (when interactive
    (let* ((filtered (eldoc-childframe--filter-breadcrumb
                      (eldoc-childframe--filter-flymake docs)))
           (composed (string-join
                      (mapcar #'eldoc-childframe--compose-doc filtered)
                      "\n"))
           (doc (string-trim composed)))
      (if (eldoc-childframe--childframe-supported-p)
          (eldoc-childframe--display
           (if (string-empty-p doc)
               "No documentation available at point."
             doc))
        ;; TTY fallback: truncated echo-area display.
        (eldoc-display-in-echo-area filtered interactive)))))

;;; ---------------------------------------------------------------------------
;;; Buffer Prettifiers (eldoc-childframe-buffer-hook)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--strip-breadcrumb ()
  "Remove breadcrumb lines (containing \" › \") from the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.* › .*$" nil t)
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max))))))

(defun eldoc-childframe--strip-invisible-props ()
  "Remove invisible text properties from markdown-ts-view-mode rendering.
markdown-ts-view-mode sets `invisible' on fence delimiters and markup;
these would hide content in the childframe buffer."
  (remove-text-properties (point-min) (point-max) '(invisible nil)))

(defun eldoc-childframe--prettify-markdown-separator ()
  "Prettify Markdown horizontal rules to span the childframe width.
Handles both `markdown-hr' text property (markdown-mode) and
`markdown-ts-thematic-break' face (markdown-ts-mode/view-mode)."
  (save-excursion
    (goto-char (point-min))
    ;; Case 1: Traditional markdown-mode sets `markdown-hr' text property.
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (let* ((beg (prop-match-beginning prop))
               (end (prop-match-end prop))
               (end+nl (save-excursion
                         (goto-char end)
                         (min (1+ (line-end-position)) (point-max)))))
          (add-text-properties beg end '(display " "))
          (add-text-properties beg end+nl
                               '(face eldoc-childframe-markdown-separator)))))
    ;; Case 2: markdown-ts-mode raw thematic breaks (---, ***, ___).
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:---\\|\\*\\*\\*\\|___\\)[ \t]*$" nil t)
      (let ((beg (line-beginning-position))
            (end (min (1+ (line-end-position)) (point-max))))
        (add-text-properties beg (line-end-position) '(display " "))
        (add-text-properties beg end
                             '(face eldoc-childframe-markdown-separator))))))

(defun eldoc-childframe--replace-en-space ()
  "Display en-spaces and line-break faces as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (when (facep 'markdown-line-break-face)
    (face-remap-set-base 'markdown-line-break-face '(:inherit default))))

(defun eldoc-childframe--make-smaller-empty-lines ()
  "Make empty lines half-height for compact rendering (lsp-ui-doc parity)."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^$" nil t)
      (add-text-properties (line-beginning-position) (line-end-position)
                           '(face (:height 0.5))))))

(defun eldoc-childframe--make-clickable-links ()
  "Find URLs in the buffer and make them clickable."
  (save-excursion
    (goto-char (point-min))
    (when (boundp 'goto-address-url-regexp)
      (let (case-fold-search)
        (while (re-search-forward goto-address-url-regexp nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (add-face-text-property beg end 'eldoc-childframe-url)
            (put-text-property beg end 'mouse-face 'highlight)
            (let ((map (make-sparse-keymap)))
              (define-key map [down-mouse-1] #'browse-url-at-mouse)
              (put-text-property beg end 'keymap map))))))))

(defun eldoc-childframe--condense-large-newline-gaps ()
  "Condense runs of 2+ consecutive blank lines into a single thin gap."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx (>= 2 (or "\n"
                          (seq bol "```" (* (syntax word)) "\n")
                          (seq (+ "<br>") "\n")
                          (seq bol (+ (or " " "\t" "\u00A0")) "\n"))))
            nil t)
      (if (or (eq (match-beginning 0) (point-min))
              (eq (match-end 0) (point-max)))
          (replace-match "")
        (replace-match "\n")
        (add-text-properties (1- (point)) (point)
                             '(font-lock-face (:height 0.4)
                                              face (:height 0.4)))))))

(defun eldoc-childframe--remove-linked-images ()
  "Remove embedded Markdown image links from documentation."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]"
                "(" (+? anychar) ")")
            nil t)
      (replace-match ""))))

(defun eldoc-childframe--remove-noise-chars ()
  "Remove carriage returns and other noise characters."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun eldoc-childframe--fontify-html ()
  "Fontify HTML heading tags and special entities."
  (save-excursion
    (goto-char (point-min))
    ;; Headings: <h1>...</h1> through <h6>...</h6>
    (while (re-search-forward
            (rx bol
                (group "<h" digit ">")
                (group (*? anychar))
                (group "</h" digit ">")
                eol)
            nil t)
      (add-text-properties (match-beginning 2) (match-end 2)
                           '(face (:weight bold)
                                  font-lock-face (:weight bold)))
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    ;; Paragraphs: <p>...</p>
    (goto-char (point-min))
    (while (re-search-forward
            (rx (group "<p>") (group (*? anychar)) (group "</p>"))
            nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    ;; Entities: &lt; &gt; &nbsp;
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (pcase (match-string 0)
                           ("&lt;" "<")
                           ("&gt;" ">")
                           ("&nbsp;" " "))))))

;;; ---------------------------------------------------------------------------
;;; Cursor Tracking & Auto-Hide
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--follow-cursor ()
  "Update or hide the childframe based on cursor movement.
If point has not moved, reposition the frame (handles scrolling).
If point has moved, hide the frame instantly (no 0.5s penalty)."
  (if (eq (point) eldoc-childframe--last-point)
      (when (eldoc-childframe--frame-visible-p)
        (eldoc-childframe--update-geometry
         eldoc-childframe--frame
         (frame-selected-window eldoc-childframe--frame)))
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe--hide-on-buffer-change ()
  "Hide the childframe when the user switches to a different buffer."
  (when (and (eldoc-childframe--frame-visible-p)
             eldoc-childframe--last-buffer
             (not (eq (current-buffer) eldoc-childframe--last-buffer))
             (not (eq (current-buffer)
                      (get-buffer eldoc-childframe--buffer))))
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe--cleanup-on-buffer-kill ()
  "Hide the childframe when the source buffer is killed."
  (when (and (eldoc-childframe--frame-visible-p)
             (eq (current-buffer) eldoc-childframe--last-buffer))
    (eldoc-childframe-quit-frame)))

;;; ---------------------------------------------------------------------------
;;; Interactive Commands
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun eldoc-childframe-help-at-point ()
  "Display documentation for the symbol at point in a childframe.
If the childframe is already visible, hide it (toggle behavior)."
  (interactive)
  (if (eldoc-childframe--frame-visible-p)
      (eldoc-childframe-quit-frame)
    ;; Trigger Eldoc's native engine with INTERACTIVE=t.
    (eldoc-print-current-symbol-info t)
    (setq eldoc-childframe--last-point (point))))

;;;###autoload
(defun eldoc-childframe-glance ()
  "Show the documentation childframe until the next command executes."
  (interactive)
  (eldoc-childframe-help-at-point)
  (add-hook 'post-command-hook #'eldoc-childframe--glance-cleanup))

(defun eldoc-childframe--glance-cleanup ()
  "Hide the childframe and remove this function from `post-command-hook'."
  (unless (memq this-command
                '(eldoc-childframe-glance eldoc-childframe-help-at-point))
    (eldoc-childframe-quit-frame)
    (remove-hook 'post-command-hook #'eldoc-childframe--glance-cleanup)))

;;; ---------------------------------------------------------------------------
;;; Minor Mode
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--enable ()
  "Enable eldoc-childframe with strict keybinding-only routing."
  (setq-local eldoc-childframe--old-display-functions
              eldoc-display-functions)
  ;; Replace display functions entirely: hover does NOTHING in prog-mode.
  (setq-local eldoc-display-functions
              (list #'eldoc-childframe--route-display))
  ;; Remove the native echo-area refresh to prevent flicker.
  (remove-hook 'pre-command-hook
               #'eldoc-pre-command-refresh-echo-area t)
  ;; Spatial debouncing: reposition or hide on cursor movement.
  (add-hook 'post-command-hook
            #'eldoc-childframe--follow-cursor nil t)
  ;; Auto-hide on buffer switch.
  (add-hook 'post-command-hook
            #'eldoc-childframe--hide-on-buffer-change nil t)
  ;; Cleanup on buffer kill.
  (add-hook 'kill-buffer-hook
            #'eldoc-childframe--cleanup-on-buffer-kill nil t)
  ;; C-g integration.
  (when eldoc-childframe-clear-with-C-g
    (advice-add #'keyboard-quit :before
                #'eldoc-childframe--quit-frame-not-in-childframe)))

(defun eldoc-childframe--disable ()
  "Disable eldoc-childframe and restore original Eldoc behavior."
  (setq-local eldoc-display-functions
              eldoc-childframe--old-display-functions)
  (setq-local eldoc-childframe--old-display-functions nil)
  (add-hook 'pre-command-hook
            #'eldoc-pre-command-refresh-echo-area nil t)
  (remove-hook 'post-command-hook
               #'eldoc-childframe--follow-cursor t)
  (remove-hook 'post-command-hook
               #'eldoc-childframe--hide-on-buffer-change t)
  (remove-hook 'kill-buffer-hook
               #'eldoc-childframe--cleanup-on-buffer-kill t)
  (advice-remove #'keyboard-quit
                 #'eldoc-childframe--quit-frame-not-in-childframe)
  ;; Destroy the frame.
  (when (and eldoc-childframe--frame
             (frame-live-p eldoc-childframe--frame))
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

;;;###autoload
(define-minor-mode eldoc-childframe-hover-at-point-mode
  "Display Eldoc documentation in a childframe on demand (M-h).
When enabled, native Eldoc echo-area and doc-buffer popups are
silenced in the current buffer.  Documentation is shown exclusively
via \\[eldoc-childframe-help-at-point]."
  :lighter " eldoc-cf"
  :global nil
  (if eldoc-childframe-hover-at-point-mode
      (eldoc-childframe--enable)
    (eldoc-childframe--disable)))

;;; ---------------------------------------------------------------------------
;;; Tab-bar / Tab-line Compatibility
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe-reset-frame ()
  "Discard the current childframe so parameter changes take effect."
  (interactive)
  (when eldoc-childframe--frame
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-mode-hook #'eldoc-childframe-reset-frame))

(with-eval-after-load 'tab-line
  (add-hook 'tab-line-mode-hook #'eldoc-childframe-reset-frame))

(provide 'eldoc-childframe)

;;; eldoc-childframe.el ends here
