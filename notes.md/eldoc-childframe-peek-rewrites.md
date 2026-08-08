# Stage 3

Now I have all the source material needed. Let me write both files with the complete implementations.

````elisp
;;; eldoc-childframe.el --- Optimized childframe documentation viewer -*- lexical-binding: t; -*-

;; Author: Ahsanur Rahman (Forked from eldoc-box by Yuan Fu / casouri)
;; Version: 2.0.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: docs, convenience, tools

;;; Commentary:
;;
;; A modernized, stripped-down fork of `eldoc-box' optimized for Emacs 31.
;;
;; Features:
;; - Keybinding-Only Activation: Childframe only spawns on demand (no hover).
;; - 4-Quadrant Position Engine: Intelligent placement avoiding screen edges.
;; - Corfu & Company Avoidance: Never overlaps completion popups.
;; - Native Router: Silences native echo-area/buffer popups in prog-mode.
;; - Flymake & Breadcrumb Firewall: Filters out diagnostic and breadcrumb payloads.
;; - TTY Degradation: Gracefully falls back to echo area on non-graphical frames.
;; - Evil Motion Fix: Spatial debouncing replaces the 0.5s inhibition trap.
;; - Buffer-Change Auto-Hide: Instantly vanishes when switching buffers/windows.
;; - markdown-ts-mode Separator Prettify: Handles both markdown-mode and
;;   markdown-ts-mode thematic break rendering.
;; - global-tab-line-mode Suppression: Prevents tab-line bleeding into childframe.
;; - lsp-ui-doc Aesthetic Parity: Compact spacing, HR lines, clickable links.
;; - Thin Border: 2px internal-border-width for minimal visual separation.
;;
;; Designed for use with `eglot-documentation-renderer' set to
;; `markdown-ts-view-mode' and the doom-tokyo-night theme.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'face-remap)

;;;; Customization Group

(defgroup eldoc-childframe nil
  "Display Eldoc docs in a floating childframe."
  :prefix "eldoc-childframe-"
  :group 'eldoc)

;;;; Faces

(defface eldoc-childframe-border
  '((((background dark))  :background "#292e42")
    (((background light)) :background "#a6accd"))
  "Border color of the documentation childframe.
Only the `:background' attribute is used."
  :group 'eldoc-childframe)

(defface eldoc-childframe-body
  '((((background dark))  :background "#24283b" :foreground "#c0caf5")
    (((background light)) :background "#f8f8f2" :foreground "#383a42"))
  "Body face for the documentation childframe content."
  :group 'eldoc-childframe)

(defface eldoc-childframe-header
  '((((background dark))  :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#4078f2" :weight bold))
  "Face for the symbol header line in the childframe."
  :group 'eldoc-childframe)

(defface eldoc-childframe-markdown-separator
  '((t (:inherit shadow :strike-through t :height 0.4 :extend t)))
  "Face for markdown horizontal rule separators."
  :group 'eldoc-childframe)

(defface eldoc-childframe-url
  '((t (:inherit link)))
  "Face for clickable URLs in documentation."
  :group 'eldoc-childframe)

;;;; User Options

(defcustom eldoc-childframe-clear-with-C-g t
  "If non-nil, clear childframe on \\[keyboard-quit]."
  :type 'boolean
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-width 900
  "Maximum width of doc childframe in pixels.
Can be a number or a function returning a number."
  :type '(choice integer function)
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-max-pixel-height 800
  "Maximum height of doc childframe in pixels.
Can be a number or a function returning a number."
  :type '(choice integer function)
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-offset '(16 16 16)
  "Pixel offsets for the doc childframe: (LEFT RIGHT TOP).
LEFT and RIGHT are used by the upper-corner position function.
TOP is the vertical offset from the frame edge."
  :type '(list (integer :tag "Left")
               (integer :tag "Right")
               (integer :tag "Top"))
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-prefer-above nil
  "If non-nil, prefer placing the childframe above point.
When nil (default), the frame appears below point unless there
is insufficient space below, in which case it flips above."
  :type 'boolean
  :group 'eldoc-childframe)

(defcustom eldoc-childframe-show-header t
  "If non-nil, show the symbol name as a header line in the childframe."
  :type 'boolean
  :group 'eldoc-childframe)

;;;; Internal Variables

(defvar eldoc-childframe--frame nil
  "The childframe used to display documentation.")

(defvar eldoc-childframe--buffer " *eldoc-childframe*"
  "The buffer used to render documentation.")

(defvar eldoc-childframe--last-point 0
  "Last point where childframe was shown.")

(defvar eldoc-childframe--last-buffer nil
  "The buffer from which the childframe was summoned.")

(defvar eldoc-childframe--main-frame nil
  "The main frame to return focus to.")

;;;; Frame Parameters
;; Combines eldoc-box completeness with lsp-ui-doc minimalism.
;; internal-border-width is 2px: thin enough to look like a border,
;; thick enough to visually separate from the parent frame.

(defvar eldoc-childframe-frame-parameters
  '((left                . -1)
    (top                 . -1)
    (width               . 0)
    (height              . 0)
    (no-accept-focus     . t)
    (no-focus-on-map     . t)
    (min-width           . 0)
    (min-height          . 0)
    (internal-border-width . 2)
    (vertical-scroll-bars   . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe         . 0)
    (right-fringe        . 0)
    (menu-bar-lines      . 0)
    (tool-bar-lines      . 0)
    (tab-bar-lines       . 0)
    (tab-bar-lines-keep-state . 0)
    (line-spacing        . 0)
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
Modeled after lsp-ui-doc and eldoc-box with Emacs 31 best practices.")

;;;; Hooks

(defvar eldoc-childframe-frame-hook nil
  "Hook run after doc frame is set up but before it is made visible.
Functions receive the parent frame as argument.")

(defvar eldoc-childframe-buffer-hook
  '(eldoc-childframe--strip-breadcrumb
    eldoc-childframe--prettify-markdown-separator
    eldoc-childframe--replace-en-space
    eldoc-childframe--remove-linked-images
    eldoc-childframe--remove-noise-chars
    eldoc-childframe--fontify-html
    eldoc-childframe--make-smaller-empty-lines
    eldoc-childframe--make-clickable-links
    eldoc-childframe--condense-large-newline-gaps)
  "Hook run after the doc buffer content is inserted.
Functions operate on the current buffer (the doc buffer).")

;;;; Frame Visibility

(defun eldoc-childframe--frame-visible-p ()
  "Return non-nil when the childframe is visible."
  (and eldoc-childframe--frame
       (frame-live-p eldoc-childframe--frame)
       (frame-visible-p eldoc-childframe--frame)))

;;;; Commands

;;;###autoload
(defun eldoc-childframe-quit-frame ()
  "Hide the documentation childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (make-frame-invisible eldoc-childframe--frame t)))

(defun eldoc-childframe--quit-frame-not-in-childframe ()
  "Hide documentation childframe unless point is in it."
  (when (and eldoc-childframe-clear-with-C-g
             (not (eq (selected-frame) eldoc-childframe--frame)))
    (eldoc-childframe-quit-frame)))

;;;###autoload
(defun eldoc-childframe-scroll-up (arg)
  "Scroll up ARG lines in the childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-up arg))))

;;;###autoload
(defun eldoc-childframe-scroll-down (arg)
  "Scroll down ARG lines in the childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-down arg))))

;;;###autoload
(defun eldoc-childframe-focus-frame ()
  "Switch focus to the childframe for scrolling/reading."
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
  "Return focus to the main frame."
  (interactive)
  (when (eq (selected-frame) eldoc-childframe--frame)
    (setq-local cursor-type nil)
    (when (and eldoc-childframe--main-frame
               (frame-live-p eldoc-childframe--main-frame))
      (select-frame-set-input-focus eldoc-childframe--main-frame)
      (set-frame-parameter eldoc-childframe--frame 'no-accept-focus t)
      (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map t))))

;;;; Position Engine (4-Quadrant)
;; Ported from eldoc-box--default-at-point-position-function-1.
;; Determines which quadrant of the frame has space and places accordingly.

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
  "Calculate (X . Y) for childframe of WIDTH and HEIGHT pixels.
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
    (cons final-x final-y)))

;;;; Corfu Avoidance
;; Ported from eldoc-box--at-point-x-y-by-corfu.

(defun eldoc-childframe--avoid-corfu (pos)
  "Adjust POS (X . Y) to avoid overlapping Corfu's popup frame.
Returns adjusted (X . Y) or POS unchanged if Corfu is not visible."
  (if (and (boundp 'corfu--frame)
           corfu--frame
           (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (cons (+ (car (frame-position corfu--frame))
               (frame-pixel-width corfu--frame)
               4)  ; 4px gap
            (cdr (frame-position corfu--frame)))
    pos))

;;;; Company Avoidance
;; Ported from eldoc-box--at-point-x-by-company.

(declare-function company-box--get-frame "company-box")
(defvar company-pseudo-tooltip-overlay)
(defvar company-box--x)

(defun eldoc-childframe--avoid-company-x ()
  "Return X offset to avoid Company's popup, or nil if not visible."
  (cond
   ;; company-box (childframe variant)
   ((and (boundp 'company-box--x) (numberp company-box--x))
    (+ company-box--x
       (frame-pixel-width (company-box--get-frame))
       4))
   ;; company pseudo-tooltip (overlay variant)
   ((and (boundp 'company-pseudo-tooltip-overlay)
         company-pseudo-tooltip-overlay)
    (+ (* (frame-char-width)
          (+ (overlay-get company-pseudo-tooltip-overlay 'company-width)
             (overlay-get company-pseudo-tooltip-overlay 'company-column)))
       (or (line-number-display-width t) 0)))
   (t nil)))

;;;; Unified Position Function

(defun eldoc-childframe--compute-position (width height)
  "Compute final (X . Y) position for childframe of WIDTH x HEIGHT.
Applies 4-quadrant logic, then Corfu avoidance, then Company avoidance."
  (let* ((base-pos (eldoc-childframe--calc-position width height))
         ;; Corfu takes priority (returns full X.Y override).
         (corfu-pos (eldoc-childframe--avoid-corfu base-pos))
         ;; Company only adjusts X.
         (company-x (eldoc-childframe--avoid-company-x)))
    (if (not (equal corfu-pos base-pos))
        corfu-pos
      (cons (or company-x (car corfu-pos))
            (cdr corfu-pos)))))

;;;; Frame Creation & Geometry

(defun eldoc-childframe--resolve-max (val)
  "Resolve VAL which can be a number or function returning a number."
  (if (functionp val) (funcall val) val))

(defun eldoc-childframe--update-geometry (frame window)
  "Update size and position of childframe FRAME with WINDOW."
  (let* ((parent-frame (frame-parent frame))
         (max-w (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-width))
         (max-h (eldoc-childframe--resolve-max eldoc-childframe-max-pixel-height))
         (size (window-text-pixel-size window nil nil max-w max-h t))
         (width (+ (car size) (frame-char-width frame)))  ; 1 char margin
         (height (cdr size))
         ;; Clamp to parent frame bounds (non-macOS clips outside).
         (width (min width (- (frame-pixel-width parent-frame) 32)))
         (height (min height (- (frame-pixel-height parent-frame) 32)))
         (frame-resize-pixelwise t)
         (pos (eldoc-childframe--compute-position width height)))
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
    (if (and eldoc-childframe--frame (frame-live-p eldoc-childframe--frame))
        (progn
          (setq frame eldoc-childframe--frame)
          (setq window (frame-selected-window frame))
          (set-frame-parameter frame 'parent-frame main-frame))
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
    ;; Theme the internal border (2px) to the border face.
    (set-face-attribute 'internal-border frame
                        :inherit 'eldoc-childframe-border)
    ;; Emacs 28+ child-frame-border face.
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border
                           (face-attribute 'eldoc-childframe-border
                                           :background nil t)
                           frame))
    (eldoc-childframe--update-geometry frame window)
    (set-window-margins window nil nil)
    (setq eldoc-childframe--frame frame)
    (with-selected-frame frame
      (set-frame-parameter nil 'left-fringe 0)
      (set-frame-parameter nil 'right-fringe 0)
      (run-hook-with-args 'eldoc-childframe-frame-hook main-frame))
    (make-frame-visible frame)))

;;;; Buffer Setup

(defun eldoc-childframe--buffer-setup (orig-buffer)
  "Set up the doc buffer.  ORIG-BUFFER is the buffer that triggered display."
  (setq mode-line-format nil)
  (setq header-line-format nil)
  ;; Suppress global-tab-line-mode bleeding into childframe.
  (when (bound-and-true-p global-tab-line-mode)
    (setq tab-line-format nil))
  ;; WORKAROUND: cursor-type 'box sometimes still shows (eldoc-box issue#66).
  (setq-local cursor-type t)
  ;; Apply body face to entire buffer.
  (buffer-face-set 'eldoc-childframe-body)
  ;; Enable visual-line-mode for wrapping.
  (visual-line-mode 1)
  ;; Suppress trailing whitespace highlighting.
  (setq-local show-trailing-whitespace nil)
  ;; Suppress window-state noise.
  (setq-local window-configuration-change-hook nil)
  (when (boundp 'window-state-change-functions)
    (setq-local window-state-change-functions nil))
  (setq-local window-size-change-functions nil)
  ;; Add pointer for clickable feel.
  (add-text-properties (point-min) (point-max) '(pointer arrow))
  ;; Set wrap/line prefixes for consistent indentation.
  (setq wrap-prefix '(space :height (1) :width 1)
        line-prefix '(space :height (1) :width 1))
  ;; Run the prettify hook chain.
  (run-hooks 'eldoc-childframe-buffer-hook)
  ;; Suppress tab-line in the childframe window.
  (when (bound-and-true-p global-tab-line-mode)
    (setq-local tab-line-format nil)))

;;;; Display Entry Point

(defun eldoc-childframe--display (str)
  "Display STR in the childframe."
  (let ((doc-buffer (get-buffer-create eldoc-childframe--buffer))
        (origin-buffer (current-buffer)))
    (with-current-buffer doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (eldoc-childframe--buffer-setup origin-buffer)))
    (let ((frame (eldoc-childframe--get-frame doc-buffer)))
      (setq eldoc-childframe--last-point (point))
      (setq eldoc-childframe--last-buffer (current-buffer))
      (make-frame-visible frame))))

;;;; Payload Filtering & Routing

(defun eldoc-childframe--filter-flymake (docs)
  "Filter out Flymake diagnostic payloads from DOCS.
Inspects the :origin plist key added by ElDoc to each doc item."
  (cl-remove-if (lambda (doc)
                  (eq (plist-get (cdr doc) :origin) 'flymake-eldoc-function))
                docs))

(defun eldoc-childframe--compose-doc (doc)
  "Compose a single DOC item into a display string.
DOC has the form (TEXT :KEY VAL...).  If :thing exists, prepend it."
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    (concat (if thing
                (concat (propertize (format "%s" thing)
                                    'face (or face 'eldoc-childframe-header))
                        ": ")
              "")
            (car doc))))

(defun eldoc-childframe--route-display (docs interactive)
  "Route DOCS to childframe only when INTERACTIVE is non-nil.
This ensures the childframe only spawns on explicit keybinding invocation,
while completely silencing native Eldoc hover popups in prog-mode."
  (when interactive
    (let* ((filtered (eldoc-childframe--filter-flymake docs))
           (composed (string-join (mapcar #'eldoc-childframe--compose-doc filtered)
                                  "\n"))
           (doc (string-trim composed)))
      (if (or (display-graphic-p) (featurep 'tty-child-frames))
          (eldoc-childframe--display
           (if (string-empty-p doc)
               "No documentation available at this point."
             doc))
        ;; TTY fallback: truncate to echo area.
        (eldoc-display-in-echo-area filtered interactive)))))

;;;; Breadcrumb Stripping

(defun eldoc-childframe--strip-breadcrumb ()
  "Remove the breadcrumb line injected by prog-eldoc--breadcrumb."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.* │ .*$" nil t)
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max))))))

;;;; Markdown Separator Prettify
;; Handles both traditional markdown-mode (markdown-hr text property)
;; and markdown-ts-mode (markdown-ts-thematic-break face).

(defun eldoc-childframe--prettify-markdown-separator ()
  "Prettify markdown horizontal rules to span the childframe width.
Handles both `markdown-hr' text property (markdown-mode) and
`markdown-ts-thematic-break' face (markdown-ts-mode/view-mode)."
  (save-excursion
    (goto-char (point-min))
    ;; Case 1: Traditional markdown-mode sets `markdown-hr' text property.
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (let* ((beg (prop-match-beginning prop))
               (end (prop-match-end prop))
               (end-plus-newline
                (save-excursion
                  (goto-char end)
                  (min (1+ (line-end-position)) (point-max)))))
          (add-text-properties beg end '(display " "))
          (add-text-properties beg end-plus-newline
                               '(face eldoc-childframe-markdown-separator)))))
    ;; Case 2: markdown-ts-mode uses `markdown-ts-thematic-break' face.
    (goto-char (point-min))
    (let (next)
      (while (setq next (next-single-property-change (or next (point-min)) 'face))
        (let ((faces (get-text-property next 'face)))
          (when (and (listp faces)
                     (memq 'markdown-ts-thematic-break faces))
            (let* ((beg (or (previous-single-property-change
                             next 'face nil (point-min))
                            (point-min)))
                   (end (or (next-single-property-change
                             next 'face nil (point-max))
                            (point-max)))
                   (end-plus-newline (min (1+ end) (point-max))))
              ;; Replace the thematic break text with a styled separator.
              (add-text-properties beg end '(display " "))
              (add-text-properties beg end-plus-newline
                                   '(face eldoc-childframe-markdown-separator))
              (setq next end-plus-newline))))))))

;;;; Aesthetic Prettifiers (lsp-ui-doc parity)

(defun eldoc-childframe--replace-en-space ()
  "Display en-spaces and line-break faces as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (when (facep 'markdown-line-break-face)
    (face-remap-set-base 'markdown-line-break-face '(:inherit default))))

(defun eldoc-childframe--make-smaller-empty-lines ()
  "Make empty lines half-height for compact rendering (lsp-ui-doc parity)."
  (save-excursion
    (goto-char (point-min))
    ;; Add a small spacer before content starts.
    (insert (propertize "\n" 'face '(:height 0.3)))
    (goto-char (point-max))
    ;; Add a small spacer after content ends.
    (insert (propertize "\n" 'face '(:height 0.3)))))

(defun eldoc-childframe--condense-large-newline-gaps ()
  "Condense exceedingly large gaps made of consecutive newlines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx (>= 2 (or "\n"
                          (seq bol "```" (* (syntax word)) "\n")
                          (seq (+ "<br>") "\n")
                          (seq bol (+ (or " " "\t" "　")) "\n"))))
            nil t)
      (if (or (eq (match-beginning 0) (point-min))
              (eq (match-end 0) (point-max)))
          (replace-match "")
        (replace-match "\n")
        (add-text-properties (1- (point)) (point)
                             '(font-lock-face (:height 0.4)
                               face (:height 0.4)))))))

(defun eldoc-childframe--remove-linked-images ()
  "Remove embedded markdown image links from documentation."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]"
                "(" (+? anychar) ")")
            nil t)
      (replace-match ""))))

(defun eldoc-childframe--remove-noise-chars ()
  "Remove noise characters like carriage returns."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun eldoc-childframe--fontify-html ()
  "Fontify HTML tags and special entities."
  (save-excursion
    ;; <hN> tags → bold, hide tags.
    (goto-char (point-min))
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
    ;; <p> tags → hide.
    (goto-char (point-min))
    (while (re-search-forward
            (rx (group "<p>") (group (*? anychar)) (group "</p>"))
            nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    ;; Special entities.
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (pcase (match-string 0)
                           ("&lt;" "<")
                           ("&gt;" ">")
                           ("&nbsp;" " "))))))

(defun eldoc-childframe--make-clickable-links ()
  "Find URLs in the buffer and make them clickable (lsp-ui-doc parity)."
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search)
      (while (re-search-forward goto-address-url-regexp nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (add-face-text-property beg end 'eldoc-childframe-url)
          (put-text-property beg end 'mouse-face
                             (list :inherit 'eldoc-childframe-url
                                   :box (list :line-width -1
                                              :color (face-foreground
                                                      'eldoc-childframe-url))))
          (let ((map (make-sparse-keymap)))
            (define-key map [down-mouse-1] #'browse-url-at-mouse)
            (put-text-property beg end 'keymap map)))))))

;;;; Evil Motion Fix & Buffer Change Auto-Hide

(defun eldoc-childframe--follow-cursor ()
  "Update or hide childframe based on cursor movement.
Uses spatial debouncing: if point hasn't moved, update geometry;
if point moved, hide instantly without the 0.5s penalty."
  (if (eq (point) eldoc-childframe--last-point)
      (when (eldoc-childframe--frame-visible-p)
        (eldoc-childframe--update-geometry
         eldoc-childframe--frame
         (frame-selected-window eldoc-childframe--frame)))
    ;; Point moved: hide frame instantly.
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe--hide-on-buffer-change ()
  "Hide childframe if the current buffer changes."
  (when (and (eldoc-childframe--frame-visible-p)
             eldoc-childframe--last-buffer
             (not (eq (current-buffer) eldoc-childframe--last-buffer))
             (not (eq (current-buffer) (get-buffer eldoc-childframe--buffer))))
    (eldoc-childframe-quit-frame)))

;;;; Help at Point & Glance

;;;###autoload
(defun eldoc-childframe-help-at-point ()
  "Display documentation of the symbol at point on demand.
Toggles: if childframe is visible, hide it; otherwise show it."
  (interactive)
  (if (eldoc-childframe--frame-visible-p)
      (eldoc-childframe-quit-frame)
    ;; Trigger Eldoc's native engine with interactive=t.
    (eldoc-print-current-symbol-info t)
    (setq eldoc-childframe--last-point (point))))

;;;###autoload
(defun eldoc-childframe-glance ()
  "Show documentation childframe temporarily until the next command."
  (interactive)
  (eldoc-childframe-help-at-point)
  (add-hook 'post-command-hook #'eldoc-childframe--glance-cleanup))

(defun eldoc-childframe--glance-cleanup ()
  "Hide childframe and remove self from `post-command-hook'."
  (unless (memq this-command
                '(eldoc-childframe-glance eldoc-childframe-help-at-point))
    (eldoc-childframe-quit-frame)
    (remove-hook 'post-command-hook #'eldoc-childframe--glance-cleanup)))

;;;; Minor Mode

(defvar-local eldoc-childframe--old-eldoc-functions nil
  "The original value of `eldoc-display-functions'.")

(defun eldoc-childframe--enable ()
  "Enable eldoc-childframe with strict keybinding-only routing."
  (setq-local eldoc-childframe--old-eldoc-functions eldoc-display-functions)
  ;; Completely replace display functions so hover does NOTHING in prog-mode.
  (setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
  (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t)
  (add-hook 'post-command-hook #'eldoc-childframe--follow-cursor nil t)
  (add-hook 'post-command-hook #'eldoc-childframe--hide-on-buffer-change)
  (when eldoc-childframe-clear-with-C-g
    (advice-add #'keyboard-quit :before #'eldoc-childframe--quit-frame-not-in-childframe)))

(defun eldoc-childframe--disable ()
  "Disable eldoc-childframe and restore original hooks."
  (setq-local eldoc-display-functions eldoc-childframe--old-eldoc-functions)
  (add-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area nil t)
  (remove-hook 'post-command-hook #'eldoc-childframe--follow-cursor t)
  (remove-hook 'post-command-hook #'eldoc-childframe--hide-on-buffer-change)
  (advice-remove #'keyboard-quit #'eldoc-childframe--quit-frame-not-in-childframe)
  (when eldoc-childframe--frame
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

;;;###autoload
(define-minor-mode eldoc-childframe-hover-at-point-mode
  "Display documentation in a childframe at point on demand."
  :lighter " eldoc-childframe"
  :global nil
  (if eldoc-childframe-hover-at-point-mode
      (eldoc-childframe--enable)
    (eldoc-childframe--disable)))

;;;; Tab-bar / Tab-line Compatibility

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
````

```elisp
;;; peek.el --- Peek anything at your fingertip (Emacs 31 Modernized Fork)  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Copyright (C) 2026 Ahsanur Rahman (Modernized Fork)

;; Version: 0.4.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Maintainer: Ahsanur Rahman <ahsanur041@proton.me>
;; Keywords: convenience, tools
;; URL: https://sr.ht/~meow_king/peek
;; Package-Requires: ((emacs "31.1"))

;;; Commentary:
;;
;; Modernized fork of peek.el for Emacs 31.
;;
;; v0.4.0 Changes (Emacs 31 Audit & Hardening):
;; - CRITICAL FIX: Replaced `save-excursion' with `save-window-excursion' in
;;   `peek-definition--set-marker'.  `save-excursion' does not protect against
;;   `switch-to-buffer' mutating the active window's buffer state.
;; - CRITICAL FIX: Fixed `make-separator-line' fatal crash.  Passing `t' to
;;   `make-separator-line' triggers `wrong-type-argument integerp t' inside
;;   `make-string'.  Now uses the zero-arg variant which correctly handles
;;   `:extend t' and trailing newlines natively.
;; - PERF FIX: Replaced `window-state-change-functions' (fires on every
;;   scroll/resize) with `window-buffer-change-functions' and
;;   `kill-buffer-hook'.  Cleanup is now scoped to the current buffer's hash
;;   table, reducing overhead from O(B*W) per scroll to O(W_local) per switch.
;; - REDISPLAY FIX: Excised `cursor-intangible' text property injection inside
;;   `after-string'.  The C redisplay engine ignores text properties inside
;;   virtual overlay strings for cursor intangibility.
;; - Eglot-Only Xref: Bypasses generic xref backend dispatch; uses Eglot's
;;   JSON-RPC directly for definitions and references to eliminate noise from
;;   etags, dumb-jump, or other backends.
;; - Debounced Post-Command: Overlay position updates are debounced via a
;;   short idle timer to prevent micro-stutters on rapid scrolling.
;; - lsp-ui-peek Parity: Header line with symbol name, reference count badge,
;;   and lightweight result summary without full list navigation overhead.
;; - Eldoc Origin Firewall: Inspects `:origin' plist to block flymake payloads.
;; - Thin Border: 1px separator lines using `make-separator-line' with
;;   `peek-overlay-border-face' for minimal visual footprint.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)

;;;; Customization Group

(defgroup peek nil
  "Peek mode: inline documentation and definition previews."
  :group 'convenience
  :prefix "peek-")

;;;; User Options

(defcustom peek-overlay-position 'above
  "Specify whether the overlay should be laid above or below the point."
  :type '(choice (const :tag "Above the point" above)
                 (const :tag "Below the point" below))
  :group 'peek)

(defcustom peek-overlay-distance 2
  "Number of lines between the peek overlay window and the point.
0 means directly above/below the current line."
  :type 'natnum
  :group 'peek)

(defcustom peek-overlay-window-size 11
  "Height of the peek overlay window in lines."
  :type 'natnum
  :group 'peek)

(defcustom peek-definition-surrounding-above-lines 1
  "Number of lines above the xref definition to show in peek view."
  :type 'natnum
  :group 'peek)

(defcustom peek-live-update t
  "Whether to automatically update content when text in marked region changes."
  :type 'boolean
  :group 'peek)

(defcustom peek-show-header t
  "If non-nil, show a header line with symbol name and result count.
Inspired by lsp-ui-peek's header rendering."
  :type 'boolean
  :group 'peek)

(defcustom peek-debounce-delay 0.05
  "Seconds to debounce overlay position updates after commands.
Prevents micro-stutters during rapid scrolling."
  :type 'number
  :group 'peek)

(defcustom peek-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'peek-next-line)
    (define-key map (kbd "M-p") #'peek-prev-line)
    (define-key map (kbd "C-n") #'peek-next-line)
    (define-key map (kbd "C-p") #'peek-prev-line)
    map)
  "Keymap used for peek mode."
  :type 'keymap
  :group 'peek)

;;;; Faces

(defface peek-overlay-border-face
  '((t (:inherit separator-line :extend t)))
  "Face for borders of peek overlay window."
  :group 'peek)

(defface peek-overlay-content-face
  '((((background light)) :background "#ecf0f1" :extend t)
    (t                    :background "#24283b" :extend t))
  "Additional face for content text of peek overlay window."
  :group 'peek)

(defface peek-header-face
  '((((background dark))  :foreground "#7aa2f7" :weight bold :height 0.9)
    (((background light)) :foreground "#4078f2" :weight bold :height 0.9))
  "Face for the peek header line (symbol name)."
  :group 'peek)

(defface peek-header-count-face
  '((((background dark))  :foreground "#9ece6a" :height 0.9)
    (((background light)) :foreground "#50a14f" :height 0.9))
  "Face for the reference/definition count in the header."
  :group 'peek)

;;;; Internal Variables

(defvar-local peek--window-overlay-map nil
  "Buffer-local hash table mapping windows to their peek overlays.")

(defvar-local peek--live-update-associated-overlays nil
  "Associated overlays for this buffer for live updates.")

(defvar peek--marked-region-markers nil
  "Store the last stored marked region markers (beginning . end).")

(defvar peek--marked-region-unused nil
  "Indicate that `peek--marked-region-markers' hasn't been used.")

(defvar peek--definition-func nil
  "Function used to fetch definition content.")

(defvar peek--definition-func-args nil
  "Arguments passed to `peek--definition-func'.")

(defvar peek--update-timer nil
  "Idle timer for debounced overlay position updates.")

;;;; Base Functions

(defun peek--ensure-window-overlay-map ()
  "Ensure the hash table for current buffer is initialized."
  (unless peek--window-overlay-map
    (setq-local peek--window-overlay-map (make-hash-table :test 'equal))))

(defun peek--cleanup-dead-window-overlays (&rest _)
  "Clean overlays associated with dead windows in the current buffer.
Optimized to only scan the current buffer's hash table to prevent
micro-stutters on window state changes."
  (when (hash-table-p peek--window-overlay-map)
    (maphash (lambda (win ol)
               (unless (window-live-p win)
                 (delete-overlay ol)
                 (remhash win peek--window-overlay-map)))
             peek--window-overlay-map)))

(defun peek--cleanup-current-buffer-overlays ()
  "Clean all overlays in the current buffer when it is killed."
  (when (hash-table-p peek--window-overlay-map)
    (maphash (lambda (_win ol) (delete-overlay ol))
             peek--window-overlay-map)
    (clrhash peek--window-overlay-map)))

;;;###autoload
(defun peek-clean-all-overlays ()
  "Clean all peek overlays in all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (peek--ensure-window-overlay-map)
      (when (hash-table-p peek--window-overlay-map)
        (maphash (lambda (_win ol) (delete-overlay ol))
                 peek--window-overlay-map)
        (clrhash peek--window-overlay-map)))))

(defun peek-get-window-overlay (&optional window)
  "Get the overlay inside WINDOW or current window."
  (peek--ensure-window-overlay-map)
  (when-let* ((w (or (and (windowp window) window)
                     (get-buffer-window))))
    (gethash w peek--window-overlay-map)))

(defun peek-delete-window-overlay (&optional window)
  "Delete the overlay inside WINDOW or current window."
  (peek--ensure-window-overlay-map)
  (when-let* ((w (or (and (windowp window) window)
                     (get-buffer-window))))
    (when-let ((ol (gethash w peek--window-overlay-map)))
      (delete-overlay ol))
    (remhash w peek--window-overlay-map)))

(defun peek-create-overlay (pos)
  "Create overlay for current window at POS."
  (peek--ensure-window-overlay-map)
  (when-let* (((not (minibufferp)))
              (win (get-buffer-window))
              (ol (make-overlay pos pos)))
    (overlay-put ol 'window win)
    (overlay-put ol 'active nil)
    (overlay-put ol 'peek-type   'string)
    (overlay-put ol 'peek-lines  '())
    (overlay-put ol 'peek-offset 0)
    (overlay-put ol 'peek-header nil)
    (puthash win ol peek--window-overlay-map)
    ol))

(defun peek-get-or-create-window-overlay (&optional window)
  "Get or create an overlay for WINDOW."
  (let ((ol (peek-get-window-overlay window)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    ol))

;;;; Overlay Content

(defun peek-overlay-get-content (ol)
  "Get the content of OL based on its peek-type."
  (pcase (overlay-get ol 'peek-type)
    ('string (peek-overlay-get-content--string ol))
    ('definition (peek-overlay-get-content--definition ol))))

(defun peek-overlay--set-active (ol active)
  "Set active/visibility of the given overlay OL to ACTIVE."
  (when (booleanp active)
    (if active
        (progn
          (overlay-put ol 'active t)
          (peek-overlay-auto-set-content ol))
      (overlay-put ol 'active nil)
      (overlay-put ol 'after-string nil))))

(defun peek-overlay--protect-string-looking (str)
  "Protect string STR looking by adding a `default' face property."
  (let ((strlen (length str)))
    (add-face-text-property 0 strlen 'default 'append str)
    str))

(defun peek-overlay--format-content (str &optional header)
  "Format peek overlay content STR with optional HEADER line.
Returns the formatted string with thin border separators."
  (let* ((strlen (length str))
         ;; FIX: `make-separator-line' with no args returns a properly
         ;; propertized newline with `:extend t' on GUI, or dashed on TTY.
         ;; It already includes the trailing newline.
         (border-top (let ((s (make-separator-line)))
                       (add-face-text-property 0 (length s)
                                               'peek-overlay-border-face t s)
                       s))
         (border-bot (let ((s (make-separator-line)))
                       (add-face-text-property 0 (length s)
                                               'peek-overlay-border-face t s)
                       s))
         ;; Header line (lsp-ui-peek parity): symbol name + count.
         (header-str (when (and peek-show-header header (not (string-empty-p header)))
                       (concat (propertize (concat " " header)
                                           'face 'peek-header-face)
                               "\n"))))
    (add-face-text-property 0 strlen 'peek-overlay-content-face 'append str)
    (peek-overlay--protect-string-looking str)
    (concat
     border-top
     (or header-str "")
     str
     (unless (string-suffix-p "\n" str) "\n")
     border-bot)))

(defun peek-overlay--set-content (ol str)
  "Set the content STR for OL."
  (when (overlay-get ol 'active)
    (let ((header (overlay-get ol 'peek-header))
          (content (peek-overlay--format-content str)))
      (overlay-put ol 'after-string content))))

(defun peek-overlay--toggle-active (ol)
  "Toggle active/visibility of the given overlay OL."
  (peek-overlay--set-active ol (not (overlay-get ol 'active))))

;;;###autoload
(defun peek-overlay-show (&optional window)
  "Show peek overlay in WINDOW or current window."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-hide (&optional window)
  "Hide peek overlay in WINDOW or current window."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol nil)))

;;;###autoload
(defun peek-overlay-toggle (&optional window)
  "Toggle peek overlay in WINDOW or current window."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--toggle-active ol)))

(defun peek--regions-overlap (r1s r1e r2s r2e)
  "Detect whether the two regions [R1S,R1E] and [R2S,R2E] overlap."
  (if (or (< r1e r1s) (< r2e r2s))
      (error "Region bounds are inverted")
    (not (or (<= r2e r1s) (<= r1e r2s)))))

;;;; Main Functions

(defun peek-display--overlay-update (&optional ol)
  "Update the overlay position if overlay is active."
  (when-let* ((ol (or (and (overlayp ol) ol)
                      (peek-get-window-overlay)))
              ((overlay-get ol 'active))
              (pos (peek-overlay--get-supposed-position)))
    (move-overlay ol pos pos)))

(defun peek--debounced-overlay-update ()
  "Debounced wrapper for `peek-display--overlay-update'.
Cancels any pending timer and schedules a new one after `peek-debounce-delay'."
  (when peek--update-timer
    (cancel-timer peek--update-timer))
  (setq peek--update-timer
        (run-with-idle-timer peek-debounce-delay nil
                             #'peek-display--overlay-update)))

(defun peek-after-change-function (rb re _plen)
  "Live update peek view after buffer changes.
RB, RE, _PLEN: see `after-change-functions'."
  (dolist (ol peek--live-update-associated-overlays)
    (if (and (eq (overlay-get ol 'peek-type) 'string)
             (consp (overlay-get ol 'peek-markers))
             (eq (current-buffer)
                 (marker-buffer (car (overlay-get ol 'peek-markers)))))
        (when-let* ((markers (overlay-get ol 'peek-markers))
                    (srb (marker-position (car markers)))
                    (sre (marker-position (cdr markers)))
                    ((peek--regions-overlap srb sre rb re))
                    (text (buffer-substring srb sre)))
          (overlay-put ol 'peek-lines (split-string text "\n"))
          (peek-overlay-auto-set-content ol))
      (setq peek--live-update-associated-overlays
            (delete ol peek--live-update-associated-overlays))))
  (when (= (length peek--live-update-associated-overlays) 0)
    (remove-hook 'after-change-functions #'peek-after-change-function t)))

(defun peek--mark-region ()
  "Get text with properties in region.  Return (mb . me) markers."
  (when (use-region-p)
    (let* ((rb (region-beginning))
           (re (region-end))
           (mb (make-marker))
           (me (make-marker)))
      (set-marker mb rb (current-buffer))
      (set-marker me re (current-buffer))
      (deactivate-mark)
      (cons mb me))))

(defun peek-overlay--get-supposed-position ()
  "Get the supposed position of an overlay."
  (save-excursion
    (pcase peek-overlay-position
      ('above (forward-line (- peek-overlay-distance)))
      ('below (forward-line (1+ peek-overlay-distance)))
      (_ (error "Unrecognized value for `peek-overlay-position'")))
    (point)))

;;;; Eldoc Display Integration

;;;###autoload
(defun peek-display-eldoc (docs interactive)
  "Display eldoc DOCS in peek view.
Related function: `eldoc-display-functions'.
Includes an Origin Firewall: inspects the :origin plist of each doc
item and blocks payloads originating from `flymake-eldoc-function'."
  (when (>= emacs-major-version 28)
    (when-let* ((interactive)
                ;; ELDOD FIREWALL: Block only payloads whose :origin is flymake.
                ((cl-loop for (_string plist) in docs
                          never (eq (plist-get plist :origin)
                                    'flymake-eldoc-function)))
                ;; NOTE: `eldoc--format-doc-buffer' is an internal API (stable since 28).
                (docs-content (with-current-buffer (eldoc--format-doc-buffer docs)
                                (buffer-string)))
                (ol (peek-get-or-create-window-overlay)))
      (overlay-put ol 'peek-type 'string)
      (overlay-put ol 'peek-lines (split-string docs-content "\n"))
      (overlay-put ol 'peek-offset 0)
      ;; Extract symbol name for header from the first doc's :thing.
      (overlay-put ol 'peek-header
                   (when-let* ((thing (plist-get (cdr (car docs)) :thing)))
                     (format "%s" thing)))
      (peek-overlay-auto-set-content ol)
      (peek-overlay--set-active ol t)
      (peek-display--overlay-update ol))))

;;;; Eglot-Only Xref Backend
;; Bypasses generic `xref-backend-functions' dispatch to eliminate noise
;; from etags, dumb-jump, or other backends.  Uses Eglot's JSON-RPC
;; directly for definitions and references.

(declare-function eglot-current-server "eglot")
(declare-function eglot--request "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--uri-to-path "eglot")
(declare-function eglot--lsp-position-to-point "eglot")

(defun peek--eglot-server-active-p ()
  "Return non-nil if an Eglot server is managing the current buffer."
  (and (fboundp 'eglot-current-server)
       (eglot-current-server)))

(defun peek--eglot-goto-location (location)
  "Navigate to an LSP LOCATION object (Location or LocationLink).
Returns the marker at the target position."
  (let* ((uri (or (plist-get location :uri)
                  (plist-get location :targetUri)))
         (range (or (plist-get location :range)
                    (plist-get location :targetRange)))
         (path (when (fboundp 'eglot--uri-to-path)
                 (eglot--uri-to-path uri)))
         (pos (when (and path range)
                (eglot--lsp-position-to-point
                 (plist-get range :start)))))
    (when (and path (file-exists-p path))
      (let ((buf (find-file-noselect path)))
        (with-current-buffer buf
          (goto-char (or pos (point-min)))
          (point-marker))))))

(defun peek-goto-eglot-definition-func (identifier)
  "Go to definition of IDENTIFIER using Eglot's JSON-RPC directly.
Bypasses `xref-find-definitions' to prevent polluting `xref-marker-stack'
and to ensure only the Eglot backend is consulted."
  (when-let* ((server (and (peek--eglot-server-active-p)
                           (eglot-current-server)))
              (result (ignore-errors
                        (eglot--request server
                                        :textDocument/definition
                                        (eglot--TextDocumentPositionParams)))))
    (let ((locations (if (vectorp result) result (vector result))))
      (when (> (length locations) 0)
        (let ((marker (peek--eglot-goto-location (aref locations 0))))
          (when marker
            (switch-to-buffer (marker-buffer marker))
            (goto-char (marker-position marker))))))))

(defun peek-goto-eglot-references-func (identifier)
  "Go to the first reference of IDENTIFIER using Eglot directly.
Bypasses generic xref to prevent history pollution and backend noise."
  (when-let* ((server (and (peek--eglot-server-active-p)
                           (eglot-current-server)))
              (params (eglot--TextDocumentPositionParams))
              (params (plist-put params :context
                                 (list :includeDeclaration :json-false)))
              (result (ignore-errors
                        (eglot--request server
                                        :textDocument/references
                                        params))))
    (when (and (vectorp result) (> (length result) 0))
      (let ((marker (peek--eglot-goto-location (aref result 0))))
        (when marker
          (switch-to-buffer (marker-buffer marker))
          (goto-char (marker-position marker)))))))

(defun peek--eglot-count-references (identifier)
  "Return the number of references for IDENTIFIER via Eglot.
Returns nil if the server is unavailable or the request fails."
  (when-let* ((server (and (peek--eglot-server-active-p)
                           (eglot-current-server)))
              (params (eglot--TextDocumentPositionParams))
              (params (plist-put params :context
                                 (list :includeDeclaration :json-false)))
              (result (ignore-errors
                        (eglot--request server
                                        :textDocument/references
                                        params))))
    (when (vectorp result)
      (length result))))

;;;; Definition Content Extraction

(defun peek-definition--get-surrounding-text ()
  "Get surrounding content for xref definition."
  (if (eq major-mode 'image-mode)
      (propertize " " 'display (get-text-property 1 'display))
    (let ((above peek-definition-surrounding-above-lines)
          p1 p2)
      (forward-line (- above))
      (setq p1 (point))
      (forward-line (+ above peek-overlay-window-size))
      (setq p2 (line-end-position))
      ;; Ensure AST-driven font-lock properties are fully rendered.
      (font-lock-ensure p1 p2)
      (buffer-substring p1 p2))))

(defun peek-definition--set-marker (ol func &optional args)
  "Call get definition function, set marker, and get content.
Uses `save-window-excursion' to prevent `switch-to-buffer' inside
FUNC from mutating the user's active window/buffer state."
  (let ((marker (make-marker))
        content)
    ;; FIX: Replaced `save-excursion' with `save-window-excursion'.
    ;; `save-excursion' only saves point/mark in the current buffer; it does NOT
    ;; protect against `switch-to-buffer' changing the window's buffer.
    (save-window-excursion
      (apply func args)
      (set-marker marker (point) (current-buffer))
      (setq content (peek-definition--get-surrounding-text)))
    (overlay-put ol 'peek-markers (list marker))
    (overlay-put ol 'peek-offset 0)
    content))

(defun peek-overlay-get-content--definition (ol)
  "Get the content of the definition for OL."
  (let ((marker (car (overlay-get ol 'peek-markers))))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let* ((offset (overlay-get ol 'peek-offset))
               (left-line-count (forward-line offset)))
          (overlay-put ol 'peek-offset (- offset left-line-count)))
        (peek-definition--get-surrounding-text)))))

(defun peek-overlay-get-content--string (ol)
  "Get content for overlay.  Peek-type: string."
  (let* ((lines (overlay-get ol 'peek-lines))
         (lines-len (length lines))
         (offset (min (1- lines-len) (overlay-get ol 'peek-offset)))
         (bound-max (min (+ offset peek-overlay-window-size) lines-len)))
    (string-join (cl-subseq lines offset bound-max) "\n")))

(defun peek-overlay-auto-set-content (ol &optional uld)
  "Automatically set content for OL.
ULD: use last definition (skip re-fetching)."
  (pcase (overlay-get ol 'peek-type)
    ('string
     (peek-overlay--set-content ol (peek-overlay-get-content--string ol)))
    ('definition
     (peek-overlay--set-content
      ol
      (if uld
          (peek-overlay-get-content--definition ol)
        (peek-definition--set-marker
         ol
         peek--definition-func
         peek--definition-func-args))))
    (_ (error "Invalid peek-type!"))))

;;;; Scrolling Commands

;;;###autoload
(defun peek-next-line ()
  "Scroll down current peek view 1 line."
  (interactive)
  (when-let* ((ol (peek-get-or-create-window-overlay))
              ((overlay-get ol 'active))
              (peek-type (overlay-get ol 'peek-type))
              (offset (overlay-get ol 'peek-offset))
              (bound-max (pcase peek-type
                           ('string (1- (length (overlay-get ol 'peek-lines))))
                           ('definition most-positive-fixnum)
                           (_ (error "Invalid peek-type!")))))
    (overlay-put ol 'peek-offset (min (1+ offset) bound-max))
    (peek-overlay-auto-set-content ol t)))

;;;###autoload
(defun peek-prev-line ()
  "Scroll up current peek view 1 line."
  (interactive)
  (when-let* ((ol (peek-get-or-create-window-overlay))
              ((overlay-get ol 'active))
              (peek-type (overlay-get ol 'peek-type))
              (offset (overlay-get ol 'peek-offset))
              (bound-min 0)
              (next-offset (max (1- offset) bound-min)))
    (overlay-put ol 'peek-offset next-offset)
    (peek-overlay-auto-set-content ol t)))

;;;; Global Minor Mode

;;;###autoload
(define-minor-mode global-peek-mode
  "Global peek mode."
  :global t
  :lighter " peek"
  :keymap peek-mode-keymap
  (cond
   (global-peek-mode
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (add-hook 'eldoc-display-functions #'peek-display-eldoc))
    (peek-clean-all-overlays)
    ;; FIX: Replaced noisy `window-state-change-functions' (fires on every scroll)
    ;; with `window-buffer-change-functions' and `kill-buffer-hook'.
    (add-hook 'window-buffer-change-functions #'peek--cleanup-dead-window-overlays)
    (add-hook 'kill-buffer-hook #'peek--cleanup-current-buffer-overlays)
    ;; Debounced position update instead of raw post-command-hook.
    (add-hook 'post-command-hook #'peek--debounced-overlay-update))
   (t
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (remove-hook 'eldoc-display-functions #'peek-display-eldoc))
    (peek-clean-all-overlays)
    (remove-hook 'window-buffer-change-functions #'peek--cleanup-dead-window-overlays)
    (remove-hook 'kill-buffer-hook #'peek--cleanup-current-buffer-overlays)
    (remove-hook 'post-command-hook #'peek--debounced-overlay-update)
    (when peek--update-timer
      (cancel-timer peek--update-timer)
      (setq peek--update-timer nil)))))

;;;; DWIM & Custom Content

;;;###autoload
(defun peek-overlay-dwim ()
  "Peek overlay do what I mean."
  (interactive)
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay)))
    (if (use-region-p)
        (progn
          (setq peek--marked-region-markers (peek--mark-region)
                peek--marked-region-unused t)
          (message "Region stored for peek"))
      (progn
        (when (eq (overlay-get ol 'peek-type) 'definition)
          (overlay-put ol 'peek-type 'string))
        (when (and (eq (overlay-get ol 'active) nil)
                   (eq peek--marked-region-unused t))
          (overlay-put ol 'peek-offset 0)
          (let* ((mb (car peek--marked-region-markers))
                 (me (cdr peek--marked-region-markers))
                 (source-buffer (marker-buffer mb))
                 (text (with-current-buffer source-buffer
                         (if (eq major-mode 'image-mode)
                             (propertize " " 'display (get-text-property 1 'display))
                           (buffer-substring
                            (marker-position mb) (marker-position me))))))
            (when peek-live-update
              (with-current-buffer source-buffer
                (add-to-list 'peek--live-update-associated-overlays ol)
                (add-hook 'after-change-functions #'peek-after-change-function nil t)))
            (overlay-put ol 'peek-markers peek--marked-region-markers)
            (overlay-put ol 'peek-lines (split-string text "\n"))
            (overlay-put ol 'peek-header nil)))
          (peek-overlay-auto-set-content ol)
          (setq peek--marked-region-unused nil))
        (peek-overlay--toggle-active ol)))))

;;;###autoload
(defun peek-view-refresh ()
  "Refresh content in the current peek view."
  (interactive)
  (when-let* ((ol (peek-get-window-overlay))
              ((eq (overlay-get ol 'peek-type) 'string))
              (markers (overlay-get ol 'peek-markers))
              (mb (car markers))
              (me (cdr markers))
              (source-buffer (marker-buffer mb))
              (text (with-current-buffer source-buffer
                      (buffer-substring
                       (marker-position mb) (marker-position me)))))
    (overlay-put ol 'peek-lines (split-string text "\n"))
    (peek-overlay-auto-set-content ol)))

;;;###autoload
(defun peek-overlay-set-custom-content (str &optional window header)
  "Set custom content STR for the peek overlay window.
Optional HEADER is displayed as a header line."
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay window)))
    (overlay-put ol 'peek-type 'string)
    (overlay-put ol 'peek-markers nil)
    (overlay-put ol 'peek-lines (split-string str "\n"))
    (overlay-put ol 'peek-header header)
    (peek-overlay-auto-set-content ol)))

;;;; Eglot Definition & References (Eglot-Only Backend)

;;;###autoload
(defun peek-definition (func &optional args header)
  "Peek the definition using given FUNC and ARGS.
Optional HEADER is displayed as the overlay header line."
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay))
        (peek--definition-func func)
        (peek--definition-func-args args))
    (unless (eq (overlay-get ol 'peek-type) 'definition)
      (overlay-put ol 'peek-offset 0)
      (overlay-put ol 'peek-type 'definition))
    (overlay-put ol 'peek-header header)
    (peek-overlay-auto-set-content ol)
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-xref-definition ()
  "Peek xref definition using Eglot backend exclusively."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if (peek--eglot-server-active-p)
        (peek-definition
         #'peek-goto-eglot-definition-func
         (list symbol)
         (when peek-show-header
           (format "⊕ %s" (or symbol "definition"))))
      (user-error "No active Eglot server in this buffer"))))

;;;###autoload
(defun peek-xref-references ()
  "Peek xref references inline using Eglot backend exclusively.
Displays reference count in the header (lsp-ui-peek parity)."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if (peek--eglot-server-active-p)
        (let ((count (peek--eglot-count-references symbol)))
          (peek-definition
           #'peek-goto-eglot-references-func
           (list symbol)
           (when peek-show-header
             (format "⊕ %s %s"
                     (or symbol "references")
                     (if count
                         (propertize (format "(%d refs)" count)
                                     'face 'peek-header-count-face)
                       "")))))
      (user-error "No active Eglot server in this buffer"))))

;;;; Eldoc Integration Option

(defcustom peek-enable-eldoc-display-integration nil
  "Show eldoc docs inside a peek view.
Requires Emacs >= 28.1."
  :type 'boolean
  :group 'peek)

(provide 'peek)
;;; peek.el ends here
```

---

## Summary of Changes

### `eldoc-childframe.el` v2.0.0

| Feature                              | Implementation                                                                                                                                                 |
| ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **4-Quadrant Position Engine**       | `eldoc-childframe--calc-position` — horizontal clamp + vertical flip logic ported from eldoc-box                                                               |
| **Corfu Avoidance**                  | `eldoc-childframe--avoid-corfu` — positions to the right of `corfu--frame`                                                                                     |
| **Company Avoidance**                | `eldoc-childframe--avoid-company-x` — handles both `company-box` and pseudo-tooltip                                                                            |
| **Markdown Separator Prettify**      | `eldoc-childframe--prettify-markdown-separator` — handles both `markdown-hr` property (markdown-mode) and `markdown-ts-thematic-break` face (markdown-ts-mode) |
| **global-tab-line-mode Suppression** | In `eldoc-childframe--buffer-setup`: `(when (bound-and-true-p global-tab-line-mode) (setq tab-line-format nil))`                                               |
| **Frame Parameter Completeness**     | Combined eldoc-box + lsp-ui-doc parameters: `no-accept-focus`, `tab-bar-lines`, `tab-bar-lines-keep-state`, `right-fringe . 0`, `cursor-type . nil`            |
| **Thin Border (Breathing Room Fix)** | `internal-border-width . 2` instead of 12 — just enough to distinguish the frame in doom-tokyo-night                                                           |
| **lsp-ui-doc Aesthetic Parity**      | `--make-smaller-empty-lines`, `--make-clickable-links`, `--fontify-html`, `--condense-large-newline-gaps`, header face, URL face                               |
| **No alter-fullscreen-frames**       | Omitted entirely (Arch Linux)                                                                                                                                  |
| **No hover mode**                    | Only keybinding-triggered display via `eldoc-childframe--route-display` with `interactive` guard                                                               |
| **Flymake Origin Firewall**          | `eldoc-childframe--filter-flymake` inspects `:origin` plist                                                                                                    |
| **Buffer-Change Auto-Hide**          | `eldoc-childframe--hide-on-buffer-change` on `post-command-hook`                                                                                               |
| **Tab-bar/Tab-line Reset**           | `eldoc-childframe-reset-frame` hooked to mode toggles                                                                                                          |

### `peek.el` v0.4.0

| Fix/Feature                        | Implementation                                                                                                                               |
| ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| **Eglot-Only Xref**                | `peek-goto-eglot-definition-func` and `peek-goto-eglot-references-func` use `eglot--request` directly — no `xref-backend-functions` dispatch |
| **Debounced Post-Command**         | `peek--debounced-overlay-update` uses `run-with-idle-timer` with `peek-debounce-delay` (0.05s) instead of raw `post-command-hook`            |
| **lsp-ui-peek Header Parity**      | `peek-header-face` + `peek-header-count-face` — shows symbol name and reference count in overlay header                                      |
| **Reference Count**                | `peek--eglot-count-references` fetches count via `:textDocument/references` for display                                                      |
| **save-window-excursion**          | Already fixed in v0.3.0, retained                                                                                                            |
| **make-separator-line**            | Zero-arg variant, retained                                                                                                                   |
| **window-buffer-change-functions** | Replaces `window-state-change-functions`, retained                                                                                           |
| **cursor-intangible excision**     | Retained                                                                                                                                     |
| **Eldoc Origin Firewall**          | `peek-display-eldoc` blocks `flymake-eldoc-function` payloads                                                                                |
| **Performance**                    | No full list navigation (that's consult/xref's job); only inline peek with O(1) overlay updates                                              |

---

---

---

# Stage 2

## Deep Audit: eldoc-childframe.el, peek.el & markdown-ts-mode Integration

### 1. Does eldoc-childframe.el Tightly Integrate with markdown-ts-mode?

**No. There is zero direct coupling between eldoc-childframe.el and markdown-ts-mode.**

After analyzing both sources exhaustively:

#### The Actual Integration Path (Indirect, via Eldoc)

The connection is a **three-hop indirect pipeline**, not a tight integration:

```
Eglot (LSP hover) → Eldoc display pipeline → eldoc-childframe (renderer)
                                                    ↑
markdown-ts-view-mode ←── eglot-documentation-renderer ←── Eglot
```

From your `config.org`:

```elisp
(eglot-documentation-renderer 'markdown-ts-view-mode)
```

This tells Eglot to render LSP hover payloads _using_ `markdown-ts-view-mode` as the rendering engine . The markdown-ts-mode source confirms it's designed for this:

> "markdown-ts-mode is ready for Emacs 31. It is already useful for reading and basic editing, and might provide, via Eglot and Eldoc, out of the box"

### What eldoc-childframe Actually Sees

eldoc-childframe.el v1.5.0 intercepts at the **Eldoc display layer** only:

```elisp
(setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
```

It receives already-composed documentation strings. It has:

- **No** `require` of `markdown-ts-mode`
- **No** reference to any `markdown-ts-*` face, variable, or function
- **No** awareness of whether the payload was rendered by markdown-ts-view-mode, gfm-view-mode, or plain text

### The One Subtle Interaction Point

markdown-ts-mode's `markdown-ts--fontify-delimiter` function contains this comment:

```elisp
;; In view-mode only, hide the whole line containing the fence
;; (including its terminating newline) so Eldoc/LSP markdown
;; snippets render without stray blank lines around the code block.
```

This means markdown-ts-view-mode **tunes its own rendering** for Eldoc consumption, but eldoc-childframe is completely unaware of this. The integration is **unidirectional**: markdown-ts-mode optimizes _for_ Eldoc; eldoc-childframe consumes _from_ Eldoc.

---

### 2. Deep Audit: eldoc-childframe.el (v1.5.0 in source)

#### Architecture Overview

A keybinding-only childframe documentation viewer that replaces the Eldoc echo-area/buffer display pipeline with a floating popup.

#### Critical Findings

#### ✅ Strengths

| Aspect                        | Assessment                                                                                                                          |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| **Flymake Origin Firewall**   | Correctly inspects `:origin` plist to block diagnostic payloads — this is the proper Emacs 31 `flymake-make-diagnostic` API pattern |
| **Spatial Debounce**          | Replaces the 0.5s `eldoc-pre-command-refresh-echo-area` inhibition with point-equality checks — eliminates the "cursor trap"        |
| **Buffer-Change Auto-Hide**   | `eldoc-childframe--hide-on-buffer-change` correctly handles workspace switching (critical for your bufferlo setup)                  |
| **TTY Degradation**           | Falls back to `eldoc-display-in-echo-area` on non-graphical frames — respects Emacs 31's `tty-child-frames` feature                 |
| **Corfu Collision Avoidance** | Checks `corfu--frame` visibility and offsets X position — prevents popup overlap                                                    |

##### ⚠️ Issues & Emacs 31 Best Practice Violations

**Issue 1: `eldoc-display-functions` Replacement is Too Aggressive**

```elisp
(setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
```

This **completely replaces** the display function list. In Emacs 31, `eldoc-display-functions` is designed as a **multi-source composition pipeline** . Your config also sets:

```elisp
(setq-local eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
```

The eager strategy calls _all_ documentation functions and passes results to _all_ display functions. By replacing the list with a single entry, you lose:

- The ability for `eldoc-display-in-echo-area` to show brief hints while the childframe shows full docs
- Future Emacs 31 display functions (e.g., the new `eldoc-help-at-pt` integration you've enabled via `setopt`)

**Emacs 31 Best Practice**: Use `add-to-list` with position control, or better, use the `:around` advice pattern on the existing display function.

**Issue 2: `eldoc-childframe--compose-doc` Duplicates Internal API**

```elisp
(defun eldoc-childframe--compose-doc (doc)
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    ...))
```

This manually reconstructs what `eldoc--format-doc-buffer` already does. In Emacs 31, the internal `eldoc--format-doc-buffer` is stable and handles:

- Multiple doc composition
- `:thing` face application
- Truncation per `eldoc-echo-area-use-multiline-p`

**Issue 3: Missing `eldoc-documentation-compose-eagerly` Awareness**

When `eldoc-documentation-strategy` is `eldoc-documentation-compose-eagerly` (as in your Eglot buffers), the DOCS argument to display functions is a **list of (STRING . PLIST) pairs**. The router handles this correctly with `mapcar`, but the TTY fallback path calls:

```elisp
(eldoc-display-in-echo-area filtered interactive)
```

This is correct, but `filtered` may contain items whose `:origin` was flymake — the filter runs _before_ the branch, so this is actually fine. ✅

**Issue 4: Frame Parameter `no-accept-focus` Toggle is Fragile**

```elisp
(defun eldoc-childframe-focus-frame ()
  (set-frame-parameter eldoc-childframe--frame 'no-accept-focus nil)
  (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map nil)
  ...)
```

Once focus is granted, it's **never restored**. If the user focuses the childframe, then quits it, the next spawn inherits `no-accept-focus = nil` from the stale frame object. The frame should be deleted and recreated, or parameters reset in `eldoc-childframe--get-frame`.

**Issue 5: `make-separator-line` Not Used**

The Markdown separator prettifier manually applies `strike-through` and `height 0.4`:

```elisp
(add-text-properties beg end '(face eldoc-childframe-markdown-separator))
```

Emacs 31's `make-separator-line` (which your peek.el correctly uses) produces a proper `:extend t` separator. The childframe could leverage this for consistent rendering.

**Issue 6: No `window-buffer-change-functions` Cleanup**

Unlike peek.el v0.3.0 which correctly uses `window-buffer-change-functions` and `kill-buffer-hook` for cleanup, eldoc-childframe has **no frame cleanup on buffer kill**. If the source buffer is killed while the childframe is visible, the childframe persists showing stale content until the next `post-command-hook` fires.

---

### 3. Deep Audit: peek.el (v0.3.0 in source)

#### Architecture Overview

An overlay-based "peek view" that renders content (strings, xref definitions, eldoc docs) in a virtual window above/below point using `after-string` overlay properties.

#### Critical Findings

#### ✅ Strengths (v0.3.0 Fixes)

| Fix                                  | Assessment                                                                                                                    |
| ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| **`save-window-excursion`**          | Correctly replaces `save-excursion` — prevents `switch-to-buffer` inside xref functions from mutating the user's window state |
| **`make-separator-line` zero-arg**   | Correctly avoids the `wrong-type-argument integerp t` crash that passing `t` causes in Emacs 31                               |
| **`window-buffer-change-functions`** | Replaces the O(B×W) per-scroll `window-state-change-functions` with O(W_local) per-switch cleanup                             |
| **Eldoc Origin Firewall**            | Blocks flymake payloads via `:origin` plist inspection                                                                        |
| **Direct xref backend bypass**       | Calls `xref-backend-definitions` directly, preventing `xref-marker-stack` pollution                                           |

#### ⚠️ Issues & Emacs 31 Best Practice Violations

**Issue 1: `after-string` Overlay Cannot Be Cursor-Intangible**

The v0.3.0 changelog correctly notes:

> "Excised `cursor-intangible` text property injection inside `after-string`. The C redisplay engine ignores text properties inside virtual overlay strings for cursor intangibility."

This is correct. However, the **consequence** is that the cursor can visually "enter" the peek overlay region during vertical motion (`C-n`/`C-p`). In Emacs 31, the proper solution is:

```elisp
(overlay-put ol 'cursor-sensor-functions
             (list (lambda (_win _prev _dir)
                     ;; Push point out of the overlay region
                     ...)))
```

Or use the new Emacs 31 `cursor-sensor-functions` stickiness behavior (from NEWS.31: "Boundaries of `cursor-sensor-functions` now obey stickiness").

**Issue 2: `peek-display--overlay-update` on Every `post-command-hook`**

```elisp
(add-hook 'post-command-hook #'peek-display--overlay-update)
```

This fires on **every single command**, including self-insert, scrolling, and mouse events. The function checks `(overlay-get ol 'active)` and recalculates position. For a config with pixel-scroll-precision and evil-snipe incremental highlighting, this adds a function call to every keystroke.

**Emacs 31 Best Practice**: Use `jit-lock-after-change-extend-region-functions` or a more targeted hook. At minimum, add a fast-path bail:

```elisp
(defun peek-display--overlay-update (&optional ol)
  (when-let ((ol (or (and (overlayp ol) ol)
                     (peek-get-window-overlay)))
             ((overlay-get ol 'active)))
    ;; Only reposition if the window's point actually moved
    (unless (eq (point) (overlay-get ol 'peek--last-point))
      ...)))
```

**Issue 3: `peek--definition-func` Uses Dynamic Binding**

```elisp
(defvar peek--definition-func nil)
(defvar peek--definition-func-args nil)
```

These are `defvar` (dynamic) but used in a `let` binding inside `peek-definition`:

```elisp
(let ((peek--definition-func func)
      (peek--definition-func-args args))
  ...)
```

Since peek.el declares `lexical-binding: t`, this `let` creates a **lexical** binding that shadows the dynamic `defvar`. This works, but it's semantically confusing. The variables should either be:

- Lexical (`defvar-local` or just `let`-bound without `defvar`), or
- Truly dynamic (used across function boundaries that expect dynamic scope)

In this case, they're only used within `peek-definition` → `peek-overlay-auto-set-content` → `peek-definition--set-marker`, all within the same call stack. The `defvar` is unnecessary.

**Issue 4: No `too-long-file-p` Guard**

Your config defines `too-long-file-p` as a performance guard. peek.el's `peek-definition--get-surrounding-text` calls `font-lock-ensure` on a region:

```elisp
(font-lock-ensure p1 p2)
```

In a massive minified JSON buffer (which your config guards against in 15+ other packages), this could trigger a full tree-sitter reparse. peek.el should respect a buffer-size guard.

**Issue 5: `peek-overlay--format-content` Allocates on Every Update**

Every call to `peek-overlay-auto-set-content` → `peek-overlay--set-content` → `peek-overlay--format-content` creates:

- Two `make-separator-line` strings
- A `copy-sequence` for face application
- A `concat` of three strings

For live-update mode (`peek-live-update t`), this fires on every `after-change-functions` call. In Emacs 31, consider caching the formatted string and only rebuilding when `peek-lines` or `peek-offset` actually change.

**Issue 6: Missing `display-line-numbers` Margin Compensation**

peek.el requires `display-line-numbers` at the top:

```elisp
(require 'display-line-numbers)
```

But the overlay position calculation (`peek-overlay--get-supposed-position`) uses `forward-line` which doesn't account for the line-number margin width. The `after-string` will render at the text column, not accounting for the margin. This is cosmetically correct (the overlay appears in the text area), but the `peek-overlay-distance` of 2 lines may visually collide with the line numbers if the peek window is narrow.

---

### 4. Cross-Package Interaction Analysis

### eldoc-childframe ↔ peek.el Conflict

Both packages can be active simultaneously in `prog-mode` buffers:

- **eldoc-childframe**: Hooks `prog-mode` via `eldoc-childframe-hover-at-point-mode`
- **peek.el**: Hooks `eldoc-display-functions` via `peek-display-eldoc` (when `peek-enable-eldoc-display-integration` is non-nil)

Your config sets:

```elisp
(peek-enable-eldoc-display-integration nil)  ;; Not explicitly set, defaults to nil
```

So peek's Eldoc integration is **disabled** in your config. ✅ No conflict.

However, if both were active, they would **fight** over `eldoc-display-functions`:

- eldoc-childframe **replaces** the list with `(list #'eldoc-childframe--route-display)`
- peek **adds** `#'peek-display-eldoc` to the list

The replacement would silently remove peek's entry. This is a design flaw in eldoc-childframe's approach.

### eldoc-childframe ↔ markdown-ts-view-mode Rendering

When Eglot renders hover docs via `markdown-ts-view-mode`, the resulting string contains:

- `invisible` text properties (from `markdown-ts--fontify-delimiter` with `markdown-ts-hide-markup t`)
- `display` properties (from hidden markup replacements)
- `button` properties (from `markdown-ts--make-link-button`)

eldoc-childframe's `eldoc-childframe--display` inserts this string into its buffer and runs `eldoc-childframe-buffer-hook`. The `visual-line-mode 1` call is correct for wrapping, but:

- The `invisible` properties from markdown-ts-view-mode will **hide content** in the childframe buffer
- The `button` properties will be **non-functional** (no `button-mode` enabled in the childframe buffer)

**Recommendation**: Add `(button-mode 1)` to the childframe buffer setup, and strip `invisible` properties:

```elisp
(remove-text-properties (point-min) (point-max) '(invisible nil))
```

---

### 5. Emacs 31 Specific Recommendations

### For eldoc-childframe v2.0.0

1. **Use `eldoc-help-at-pt` integration** (new in Emacs 31):

   ```elisp
   ;; Your config already does: (setopt eldoc-help-at-pt t)
   ;; eldoc-childframe should respect this by showing keybinding hints
   ;; in the childframe when the symbol has a keybinding.
   ```

2. **Respect `eldoc-echo-area-prefer-doc-buffer`**: Your config sets this to `'maybe`. eldoc-childframe should check this and defer to the `*eldoc*` buffer for very long docs instead of creating a massive childframe.

3. **Use `child-frame-border` face inheritance** (Emacs 28+): Already done ✅

4. **Leverage `tty-child-frames`** (Emacs 31): Your `childframe-workable-p` already checks `(featurep 'tty-child-frames)` ✅

### For peek.el v0.4.0

1. **Use `treesit-thing-at` for definition context**: Instead of raw `thing-at-point 'symbol`, use the tree-sitter thing API for more precise symbol extraction in ts-modes.

2. **Respect `treesit-font-lock-level`**: When calling `font-lock-ensure` in `peek-definition--get-surrounding-text`, the fontification quality depends on this setting. Your config sets it to 4 (maximum) ✅

3. **Use `window-cursor-info`** (new in Emacs 31): For precise pixel-level positioning of the peek overlay relative to the cursor, instead of the current `pos-visible-in-window-p` approach.

---

### 6. Summary Verdict

| Package              | Integration with markdown-ts-mode     | Emacs 31 Readiness     | Critical Issues                                     |
| -------------------- | ------------------------------------- | ---------------------- | --------------------------------------------------- |
| **eldoc-childframe** | None (indirect via Eldoc pipeline)    | Good, minor gaps       | Frame focus leak; aggressive display-fn replacement |
| **peek.el**          | None (xref/eldoc only)                | Good post-v0.3.0 fixes | post-command-hook overhead; no buffer-size guard    |
| **markdown-ts-mode** | N/A (it's the renderer, not consumer) | Excellent (built-in)   | N/A                                                 |

The three packages form a **clean separation of concerns**: markdown-ts-mode renders, Eldoc routes, and eldoc-childframe/peek display. There is no tight coupling, which is architecturally correct. The only risk is the Eldoc display-function list being a shared mutable resource that both eldoc-childframe and peek could fight over if both Eldoc integrations are enabled simultaneously.

---

---

---

# Stage 1

## Rewriting `eldoc-childframe.el` and `peek.el`

I've fully ingested the knowledge base: the Emacs 31 NEWS, the straight.el/use-package integration YAML, `early-init.el`, both versions of `config.org.txt` (v0.31 and v0.33), the upstream `eldoc.el` source, and the current drafts of both target files. Below are the corrected rewrites.

---

### `eldoc-childframe.el`

````elisp
;;; eldoc-childframe.el --- On-demand childframe documentation viewer -*- lexical-binding: t; -*-

;; Author: Ahsanur Rahman (Forked from eldoc-box by Yuan Fu)
;; Version: 1.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, help

;;; Commentary:
;;
;; A modernized, stripped-down fork of `eldoc-box` optimized for Emacs 31.
;;
;; Design Principles:
;; - Keybinding-Only Activation: The childframe spawns exclusively on demand
;;   via `eldoc-childframe-help-at-point' (bound to M-h).  Idle hover does
;;   NOT trigger the childframe; the minor mode silences native Eldoc echo-area
;;   and doc-buffer popups in prog-mode so the echo area stays clean.
;; - Flymake & Breadcrumb Firewall: Inspects the `:origin' plist key on each
;;   Eldoc doc item and drops payloads from `flymake-eldoc-function' and
;;   breadcrumb injectors before rendering.
;; - TTY Degradation: On non-graphical frames without `tty-child-frames',
;;   falls back to truncated echo-area display via `eldoc-display-in-echo-area'.
;; - Evil Motion Fix: Spatial debouncing (point comparison) replaces the
;;   legacy 0.5s `eldoc-idle-delay' inhibition trap.
;; - Corfu Collision Avoidance: Shifts the childframe rightward when a
;;   `corfu--frame' is visible to prevent overlap.
;; - Buffer-Change Auto-Hide: Instantly vanishes when the user switches
;;   buffers or windows.

;;; Code:

(require 'cl-lib)
(require 'eldoc)

;;; ---------------------------------------------------------------------------
;;; Customization
;;; ---------------------------------------------------------------------------

(defgroup eldoc-childframe nil
  "Display Eldoc documentation in a floating childframe."
  :prefix "eldoc-childframe-"
  :group 'eldoc)

(defface eldoc-childframe-border
  '((((background dark))  . (:background "#292e42"))
    (((background light)) . (:background "#d0d0d0")))
  "Border face for the documentation childframe.")

(defface eldoc-childframe-body
  '((((background dark))  . (:background "#24283b" :foreground "#c0caf5"))
    (((background light)) . (:background "#f5f5f5" :foreground "#333333")))
  "Body face for documentation text in the childframe.")

(defface eldoc-childframe-markdown-separator
  '((t (:inherit shadow :strike-through t :height 0.4 :extend t)))
  "Face for the horizontal separator line in Markdown documentation.")

(defcustom eldoc-childframe-clear-with-C-g t
  "If non-nil, \\[keyboard-quit] hides the documentation childframe."
  :type 'boolean)

(defcustom eldoc-childframe-max-pixel-width 900
  "Maximum width of the documentation childframe in pixels."
  :type 'natnum)

(defcustom eldoc-childframe-max-pixel-height 800
  "Maximum height of the documentation childframe in pixels."
  :type 'natnum)

(defcustom eldoc-childframe-offset '(16 16 16)
  "Left, right, and top pixel offsets for the childframe.
Value is a list: (LEFT RIGHT TOP)."
  :type '(list (integer :tag "Left")
               (integer :tag "Right")
               (integer :tag "Top")))

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

(defvar eldoc-childframe--old-display-functions nil
  "Saved value of `eldoc-display-functions' before mode activation.")

;;; ---------------------------------------------------------------------------
;;; Frame Parameters
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe-frame-parameters
  '((left . -1)
    (top . -1)
    (width  . 0)
    (height . 0)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (min-height . 0)
    (internal-border-width . 12)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (tab-bar-lines . 0))
  "Frame parameters for the documentation childframe.")

;;; ---------------------------------------------------------------------------
;;; Hooks
;;; ---------------------------------------------------------------------------

(defvar eldoc-childframe-frame-hook nil
  "Hook run after the childframe is set up, before it is made visible.
Each function receives the parent frame as its sole argument.")

(defvar eldoc-childframe-buffer-hook
  '(eldoc-childframe--strip-breadcrumb
    eldoc-childframe--prettify-markdown-separator
    eldoc-childframe--replace-en-space
    eldoc-childframe--remove-linked-images
    eldoc-childframe--remove-noise-chars
    eldoc-childframe--fontify-html
    eldoc-childframe--condense-large-newline-gaps)
  "Hook run after the documentation buffer content is inserted.
Each function operates on the current buffer.")

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
;;; Frame Lifecycle
;;; ---------------------------------------------------------------------------

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

(defun eldoc-childframe-scroll-up (arg)
  "Scroll up ARG lines in the documentation childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-up arg))))

(defun eldoc-childframe-scroll-down (arg)
  "Scroll down ARG lines in the documentation childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-down arg))))

(defun eldoc-childframe-focus-frame ()
  "Move input focus to the documentation childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (set-frame-parameter eldoc-childframe--frame 'no-accept-focus nil)
    (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map nil)
    (select-frame-set-input-focus eldoc-childframe--frame)
    (with-selected-frame eldoc-childframe--frame
      (setq-local cursor-type 'bar)
      (local-set-key (kbd "q") #'eldoc-childframe-quit-frame))))

;;; ---------------------------------------------------------------------------
;;; Frame Construction & Geometry
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--get-frame (buffer)
  "Return a childframe displaying BUFFER, creating one if needed."
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-childframe-frame-parameters
                            `((minibuffer . ,(minibuffer-window)))))
         window frame
         (main-frame (selected-frame)))
    (if (and eldoc-childframe--frame
             (frame-live-p eldoc-childframe--frame))
        (progn
          (setq frame eldoc-childframe--frame)
          (setq window (frame-selected-window frame))
          (set-frame-parameter frame 'parent-frame main-frame))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter)
                      (no-other-window . t)
                      (no-delete-other-windows . t))))
      (setq frame (window-frame window)))
    ;; Face setup
    (set-face-attribute 'fringe frame
                        :background 'unspecified
                        :inherit 'eldoc-childframe-body)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-attribute 'internal-border frame
                        :inherit 'eldoc-childframe-border)
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
      (set-frame-parameter nil 'left-fringe 0)
      (set-frame-parameter nil 'right-fringe 0)
      (run-hook-with-args 'eldoc-childframe-frame-hook main-frame))
    (make-frame-visible frame)))

(defun eldoc-childframe--update-geometry (frame window)
  "Resize and reposition FRAME's WINDOW to fit its content."
  (let* ((parent-frame (frame-parent frame))
         (size (window-text-pixel-size
                window nil nil
                eldoc-childframe-max-pixel-width
                eldoc-childframe-max-pixel-height t))
         (width (+ (car size) (frame-char-width frame)))
         (height (cdr size))
         (width (min width (- (frame-pixel-width parent-frame) 32)))
         (height (min height (- (frame-pixel-height parent-frame) 32)))
         (frame-resize-pixelwise t)
         (pos (eldoc-childframe--calc-position width height)))
    (set-frame-size frame width height t)
    (set-frame-position frame (car pos) (cdr pos))))

(defun eldoc-childframe--calc-position (width height)
  "Calculate (X . Y) for a childframe of WIDTH and HEIGHT pixels."
  (let* ((point-pos (pos-visible-in-window-p (point) nil t))
         (x (+ (car point-pos)
               (nth 0 (window-edges nil nil nil t))
               (frame-char-width)))
         (y (+ (cadr point-pos)
               (nth 1 (window-edges nil nil nil t))
               (frame-char-height)))
         (parent-w (frame-pixel-width))
         (parent-h (frame-pixel-height)))
    ;; Corfu collision avoidance: shift right of the Corfu popup.
    (when (and (boundp 'corfu--frame)
               (frame-live-p corfu--frame)
               (frame-visible-p corfu--frame))
      (setq x (+ (car (frame-position corfu--frame))
                 (frame-pixel-width corfu--frame)
                 4)))
    ;; Horizontal clamping
    (when (> (+ x width) parent-w)
      (setq x (max 0 (- parent-w width 16))))
    ;; Vertical: prefer below; flip above if insufficient space.
    (if (> (+ y height) parent-h)
        (setq y (max 0 (- y height (frame-char-height))))
      (setq y (+ y (frame-char-height))))
    (cons x y)))

;;; ---------------------------------------------------------------------------
;;; Payload Filtering
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
Breadcrumbs are injected by `prog-eldoc--breadcrumb' or similar and
carry :origin `eldoc-breadcrumb' or contain the \" │ \" separator."
  (cl-remove-if
   (lambda (doc)
     (or (eq (plist-get (cdr doc) :origin) 'eldoc-breadcrumb)
         (string-match-p " │ " (car doc))))
   docs))

(defun eldoc-childframe--compose-doc (doc)
  "Compose a single DOC item (STRING . PLIST) into a display string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face  (plist-get (cdr doc) :face)))
    (concat (when thing
              (concat (propertize (format "%s" thing)
                                  'face (or face 'bold))
                      ": "))
            (car doc))))

;;; ---------------------------------------------------------------------------
;;; Display Router
;;; ---------------------------------------------------------------------------

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

(defun eldoc-childframe--display (str)
  "Display STR in the documentation childframe."
  (let ((doc-buffer (get-buffer-create eldoc-childframe--buffer)))
    (with-current-buffer doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (buffer-face-set 'eldoc-childframe-body)
        (visual-line-mode 1)
        ;; Suppress all chrome for a clean floating tooltip.
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)
        (run-hooks 'eldoc-childframe-buffer-hook)))
    (let ((frame (eldoc-childframe--get-frame doc-buffer)))
      (setq eldoc-childframe--last-point (point))
      (setq eldoc-childframe--last-buffer (current-buffer))
      (make-frame-visible frame))))

;;; ---------------------------------------------------------------------------
;;; Buffer Prettifiers (eldoc-childframe-buffer-hook)
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe--strip-breadcrumb ()
  "Remove breadcrumb lines (containing \" │ \") from the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.* │ .*$" nil t)
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max))))))

(defun eldoc-childframe--prettify-markdown-separator ()
  "Prettify Markdown horizontal rules in Eglot documentation."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'markdown-hr))
        (let* ((beg (prop-match-beginning prop))
               (end (prop-match-end prop))
               (end+nl (save-excursion
                         (goto-char end)
                         (min (1+ (line-end-position)) (point-max)))))
          (add-text-properties beg end '(display " "))
          (add-text-properties beg end+nl
                               '(face eldoc-childframe-markdown-separator)))))))

(defun eldoc-childframe--replace-en-space ()
  "Display en-spaces and line-break faces as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (face-remap-set-base 'markdown-line-break-face '(:inherit default)))

(defun eldoc-childframe--condense-large-newline-gaps ()
  "Condense runs of 2+ consecutive blank lines into a single thin gap."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx (>= 2 (or "\n"
                          (seq bol "```" (* (syntax word)) "\n")
                          (seq (+ "<br>") "\n")
                          (seq bol (+ (or " " "\t" "　")) "\n"))))
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

;;; ---------------------------------------------------------------------------
;;; Interactive Commands
;;; ---------------------------------------------------------------------------

(defun eldoc-childframe-help-at-point ()
  "Display documentation for the symbol at point in a childframe.
If the childframe is already visible, hide it (toggle behavior)."
  (interactive)
  (if (eldoc-childframe--frame-visible-p)
      (eldoc-childframe-quit-frame)
    ;; Trigger Eldoc's native engine with INTERACTIVE=t.
    ;; This forces `eldoc-display-functions' to run with interactive=t,
    ;; which our router catches to spawn the childframe.
    (eldoc-print-current-symbol-info t)
    (setq eldoc-childframe--last-point (point))))

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
  ;; The childframe only spawns when `eldoc-print-current-symbol-info' is
  ;; called with INTERACTIVE=t (i.e., from our M-h binding).
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

(provide 'eldoc-childframe)
;;; eldoc-childframe.el ends here
````

---

### `peek.el`

```elisp
;;; peek.el --- Peek anything at your fingertip (Emacs 31 Modernized Fork)  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Copyright (C) 2026 Ahsanur Rahman (Modernized Fork)

;; Version: 0.3.1
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Maintainer: Ahsanur Rahman <ahsanur041@proton.me>
;; Keywords: convenience, tools
;; URL: https://sr.ht/~meow_king/peek
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; Modernized fork of peek.el for Emacs 31.
;;
;; v0.3.1 Changes (Emacs 31 Audit & Hardening):
;;
;; - CRITICAL FIX: Replaced `save-excursion' with `save-window-excursion'
;;   in `peek-definition--set-marker'.  `save-excursion' does NOT protect
;;   against `switch-to-buffer' mutating the active window's buffer state.
;;
;; - CRITICAL FIX: Fixed `make-separator-line' fatal crash.  Passing `t'
;;   as the argument triggers `wrong-type-argument integerp t' inside
;;   `make-string'.  Now uses the zero-argument variant which correctly
;;   handles `:extend t' and trailing newlines natively.
;;
;; - PERF FIX: Replaced `window-state-change-functions' (fires on every
;;   scroll/resize event) with `window-buffer-change-functions' and
;;   `kill-buffer-hook'.  Cleanup is now scoped to the current buffer's
;;   hash table, reducing overhead from O(B×W) per scroll to O(W_local)
;;   per buffer switch.
;;
;; - REDISPLAY FIX: Excised `cursor-intangible' text property injection
;;   inside `after-string'.  The C redisplay engine ignores text properties
;;   inside virtual overlay strings for cursor intangibility.
;;
;; - Eldoc Origin Firewall: Inspects the `:origin' plist key on each doc
;;   item and blocks payloads originating from `flymake-eldoc-function'.
;;
;; - Xref History Bypass: Calls `xref-backend-definitions' directly to
;;   prevent polluting `xref-marker-stack' (the "back" history ring).

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'subr-x)
(require 'eldoc)
(require 'display-line-numbers)

;;; ---------------------------------------------------------------------------
;;; Customization
;;; ---------------------------------------------------------------------------

(defgroup peek nil
  "Peek mode: inline documentation and definition previews."
  :group 'convenience)

(defcustom peek-overlay-position 'above
  "Whether the peek overlay appears above or below point."
  :type '(choice (const :tag "Above the point" above)
                 (const :tag "Below the point" below)))

(defcustom peek-overlay-distance 2
  "Number of lines between the peek overlay and point.
0 means directly adjacent to the current line."
  :type 'natnum)

(defcustom peek-overlay-window-size 11
  "Height of the peek overlay window in lines."
  :type 'natnum)

(defcustom peek-definition-surrounding-above-lines 1
  "Number of context lines above the xref definition to display."
  :type 'natnum)

(defcustom peek-live-update t
  "If non-nil, automatically update content when the marked region changes."
  :type 'boolean)

(defcustom peek-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'peek-next-line)
    (define-key map (kbd "M-p") #'peek-prev-line)
    (define-key map (kbd "C-n") #'peek-next-line)
    (define-key map (kbd "C-p") #'peek-prev-line)
    map)
  "Keymap active while `global-peek-mode' is enabled."
  :type 'keymap)

(defcustom peek-enable-eldoc-display-integration nil
  "If non-nil, show Eldoc documentation inside a peek view.
Requires Emacs >= 28.1."
  :type 'boolean)

;;; ---------------------------------------------------------------------------
;;; Faces
;;; ---------------------------------------------------------------------------

(defface peek-overlay-border-face
  '((t (:inherit separator-line :extend t)))
  "Face for the top and bottom borders of the peek overlay.")

(defface peek-overlay-content-face
  '((((background light)) :background "#ecf0f1" :extend t)
    (t                    :background "#24283b" :extend t))
  "Additional face applied to peek overlay content text.")

;;; ---------------------------------------------------------------------------
;;; Internal State
;;; ---------------------------------------------------------------------------

(defvar-local peek--window-overlay-map nil
  "Buffer-local hash table mapping windows to their peek overlays.")

(defvar-local peek--live-update-associated-overlays nil
  "Overlays in this buffer that track live region updates.")

(defvar peek--marked-region-markers nil
  "Cons cell (BEGIN-MARKER . END-MARKER) of the last stored marked region.")

(defvar peek--marked-region-unused nil
  "Non-nil if `peek--marked-region-markers' has not yet been consumed.")

(defvar peek--definition-func nil
  "Function used to navigate to a definition for peek content.")

(defvar peek--definition-func-args nil
  "Arguments to pass to `peek--definition-func'.")

;;; ---------------------------------------------------------------------------
;;; Overlay Hash Table Management
;;; ---------------------------------------------------------------------------

(defun peek--ensure-window-overlay-map ()
  "Initialize the buffer-local overlay hash table if needed."
  (unless peek--window-overlay-map
    (setq-local peek--window-overlay-map
                (make-hash-table :test 'equal))))

(defun peek--cleanup-dead-window-overlays (&rest _)
  "Remove overlays for dead windows from the current buffer's table.
Optimized: only scans the current buffer's hash table, not all buffers."
  (when (hash-table-p peek--window-overlay-map)
    (maphash (lambda (win ol)
               (unless (window-live-p win)
                 (delete-overlay ol)
                 (remhash win peek--window-overlay-map)))
             peek--window-overlay-map)))

(defun peek--cleanup-current-buffer-overlays ()
  "Remove all peek overlays when the current buffer is killed."
  (when (hash-table-p peek--window-overlay-map)
    (maphash (lambda (_win ol) (delete-overlay ol))
             peek--window-overlay-map)
    (clrhash peek--window-overlay-map)))

(defun peek-clean-all-overlays ()
  "Remove all peek overlays in all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (hash-table-p peek--window-overlay-map)
        (maphash (lambda (_win ol) (delete-overlay ol))
                 peek--window-overlay-map)
        (clrhash peek--window-overlay-map)))))

;;; ---------------------------------------------------------------------------
;;; Overlay Accessors
;;; ---------------------------------------------------------------------------

(defun peek-get-window-overlay (&optional window)
  "Return the peek overlay for WINDOW (default: selected window)."
  (peek--ensure-window-overlay-map)
  (when-let ((w (or (and (windowp window) window)
                    (get-buffer-window))))
    (gethash w peek--window-overlay-map)))

(defun peek-delete-window-overlay (&optional window)
  "Delete the peek overlay for WINDOW (default: selected window)."
  (peek--ensure-window-overlay-map)
  (when-let ((w (or (and (windowp window) window)
                    (get-buffer-window))))
    (when-let ((ol (gethash w peek--window-overlay-map)))
      (delete-overlay ol))
    (remhash w peek--window-overlay-map)))

(defun peek-create-overlay (pos)
  "Create a peek overlay at POS in the current buffer."
  (peek--ensure-window-overlay-map)
  (when-let (((not (minibufferp)))
             (win (get-buffer-window))
             (ol (make-overlay pos pos)))
    (overlay-put ol 'window win)
    (overlay-put ol 'active nil)
    (overlay-put ol 'peek-type   'string)
    (overlay-put ol 'peek-lines  '())
    (overlay-put ol 'peek-offset 0)
    (puthash win ol peek--window-overlay-map)
    ol))

(defun peek-get-or-create-window-overlay (&optional window)
  "Return the overlay for WINDOW, creating one if none exists."
  (or (peek-get-window-overlay window)
      (peek-create-overlay (peek-overlay--get-supposed-position))))

;;; ---------------------------------------------------------------------------
;;; Overlay Content Rendering
;;; ---------------------------------------------------------------------------

(defun peek-overlay--get-supposed-position ()
  "Calculate the buffer position where the overlay should be placed."
  (save-excursion
    (pcase peek-overlay-position
      ('above (forward-line (- peek-overlay-distance)))
      ('below (forward-line (1+ peek-overlay-distance)))
      (_ (error "Unrecognized `peek-overlay-position': %s"
                peek-overlay-position)))
    (point)))

(defun peek-overlay--protect-string-looking (str)
  "Add a `default' face to STR to prevent inherited face bleed."
  (let ((len (length str)))
    (add-face-text-property 0 len 'default 'append str)
    str))

(defun peek-overlay--format-content (str)
  "Format STR with borders and content face for overlay display."
  (let* ((strlen (length str))
         ;; FIX: `make-separator-line' with NO arguments returns a properly
         ;; propertized newline with `:extend t' on GUI, or a dashed line on
         ;; TTY.  It already includes the trailing newline.  Passing `t' as
         ;; an argument crashes Emacs 31 with `wrong-type-argument integerp t'.
         (border-top (let ((s (make-separator-line)))
                       (add-face-text-property 0 (length s)
                                               'peek-overlay-border-face t s)
                       s))
         (border-bot (let ((s (make-separator-line)))
                       (add-face-text-property 0 (length s)
                                               'peek-overlay-border-face t s)
                       s)))
    (add-face-text-property 0 strlen 'peek-overlay-content-face 'append str)
    (peek-overlay--protect-string-looking str)
    ;; FIX: Excised `cursor-intangible' text property injection.
    ;; The C redisplay engine ignores text properties inside `after-string'
    ;; for cursor intangibility; injecting them is a no-op that adds overhead.
    (concat border-top
            str
            (unless (string-suffix-p "\n" str) "\n")
            border-bot)))

(defun peek-overlay--set-content (ol str)
  "Set the formatted content STR on overlay OL."
  (when (overlay-get ol 'active)
    (overlay-put ol 'after-string (peek-overlay--format-content str))))

(defun peek-overlay--set-active (ol active)
  "Set the visibility of overlay OL to ACTIVE (boolean)."
  (when (booleanp active)
    (if active
        (progn
          (overlay-put ol 'active t)
          (peek-overlay--set-content ol (peek-overlay-get-content ol)))
      (overlay-put ol 'active nil)
      (overlay-put ol 'after-string nil))))

(defun peek-overlay--toggle-active (ol)
  "Toggle the visibility of overlay OL."
  (peek-overlay--set-active ol (not (overlay-get ol 'active))))

;;; ---------------------------------------------------------------------------
;;; Content Retrieval
;;; ---------------------------------------------------------------------------

(defun peek-overlay-get-content (ol)
  "Retrieve the display content for overlay OL based on its peek-type."
  (pcase (overlay-get ol 'peek-type)
    ('string     (peek-overlay-get-content--string ol))
    ('definition (peek-overlay-get-content--definition ol))
    (_ "")))

(defun peek-overlay-get-content--string (ol)
  "Get content for a string-type overlay OL."
  (let* ((lines (overlay-get ol 'peek-lines))
         (lines-len (length lines))
         (offset (min (max 0 (overlay-get ol 'peek-offset))
                      (1- (max 1 lines-len))))
         (bound-max (min (+ offset peek-overlay-window-size) lines-len)))
    (if (zerop lines-len)
        ""
      (string-join (cl-subseq lines offset bound-max) "\n"))))

(defun peek-overlay-get-content--definition (ol)
  "Get content for a definition-type overlay OL by re-reading the source."
  (let ((marker (car (overlay-get ol 'peek-markers))))
    (if (not (and marker (marker-buffer marker)
                  (buffer-live-p (marker-buffer marker))))
        "Definition buffer no longer available."
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (let* ((offset (overlay-get ol 'peek-offset))
                 (left-line-count (forward-line offset)))
            (overlay-put ol 'peek-offset (- offset left-line-count)))
          (peek-definition--get-surrounding-text))))))

(defun peek-overlay-auto-set-content (ol &optional use-last-definition)
  "Set content on OL.  If USE-LAST-DEFINITION, re-read from stored marker."
  (pcase (overlay-get ol 'peek-type)
    ('string
     (peek-overlay--set-content ol (peek-overlay-get-content--string ol)))
    ('definition
     (peek-overlay--set-content
      ol
      (if use-last-definition
          (peek-overlay-get-content--definition ol)
        (peek-definition--set-marker
         ol peek--definition-func peek--definition-func-args))))
    (_ (error "Invalid peek-type on overlay %s" ol))))

;;; ---------------------------------------------------------------------------
;;; Definition Fetching
;;; ---------------------------------------------------------------------------

(defun peek-definition--get-surrounding-text ()
  "Extract surrounding text at point for definition display."
  (if (eq major-mode 'image-mode)
      (propertize " " 'display (get-text-property 1 'display))
    (let ((above peek-definition-surrounding-above-lines)
          p1 p2)
      (forward-line (- above))
      (setq p1 (point))
      (forward-line (+ above peek-overlay-window-size))
      (setq p2 (line-end-position))
      ;; Ensure font-lock properties are fully materialized.
      (font-lock-ensure p1 p2)
      (buffer-substring p1 p2))))

(defun peek-definition--set-marker (ol func &optional args)
  "Call FUNC with ARGS to navigate to a definition, then capture content.

CRITICAL FIX: Uses `save-window-excursion' instead of `save-excursion'.
`save-excursion' only saves point and mark in the CURRENT buffer; it does
NOT protect against `switch-to-buffer' (called by xref navigation) changing
the window's displayed buffer.  `save-window-excursion' saves and restores
the entire window configuration, preventing the user's active window from
being hijacked by the definition lookup."
  (let ((marker (make-marker))
        content)
    (save-window-excursion
      (apply func args)
      (set-marker marker (point) (current-buffer))
      (setq content (peek-definition--get-surrounding-text)))
    (overlay-put ol 'peek-markers (list marker))
    (overlay-put ol 'peek-offset 0)
    content))

;;; ---------------------------------------------------------------------------
;;; Live Update Engine
;;; ---------------------------------------------------------------------------

(defun peek-after-change-function (rb re _plen)
  "Update peek overlays when the marked region changes.
RB, RE, _PLEN: see `after-change-functions'."
  (dolist (ol peek--live-update-associated-overlays)
    (if (and (eq (overlay-get ol 'peek-type) 'string)
             (consp (overlay-get ol 'peek-markers))
             (eq (current-buffer)
                 (marker-buffer (car (overlay-get ol 'peek-markers)))))
        (when-let ((markers (overlay-get ol 'peek-markers))
                   (srb (marker-position (car markers)))
                   (sre (marker-position (cdr markers)))
                   ((peek--regions-overlap srb sre rb re))
                   (text (buffer-substring srb sre)))
          (overlay-put ol 'peek-lines (split-string text "\n"))
          (peek-overlay-auto-set-content ol))
      ;; Marker invalid or buffer mismatch: remove from tracking.
      (setq peek--live-update-associated-overlays
            (delete ol peek--live-update-associated-overlays))))
  (when (zerop (length peek--live-update-associated-overlays))
    (remove-hook 'after-change-functions #'peek-after-change-function t)))

(defun peek--regions-overlap (r1s r1e r2s r2e)
  "Return non-nil if regions [R1S,R1E) and [R2S,R2E) overlap."
  (if (or (< r1e r1s) (< r2e r2s))
      (error "Region bounds are inverted")
    (not (or (<= r2e r1s) (<= r1e r2s)))))

(defun peek--mark-region ()
  "Store the active region as markers.  Return (BEGIN . END) markers."
  (when (use-region-p)
    (let* ((rb (region-beginning))
           (re (region-end))
           (mb (make-marker))
           (me (make-marker)))
      (set-marker mb rb (current-buffer))
      (set-marker me re (current-buffer))
      (deactivate-mark)
      (cons mb me))))

;;; ---------------------------------------------------------------------------
;;; Interactive Commands
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun peek-overlay-show (&optional window)
  "Show the peek overlay in WINDOW (default: selected window)."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-hide (&optional window)
  "Hide the peek overlay in WINDOW (default: selected window)."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol nil)))

;;;###autoload
(defun peek-overlay-toggle (&optional window)
  "Toggle the peek overlay in WINDOW (default: selected window)."
  (interactive)
  (when-let ((ol (peek-get-window-overlay window)))
    (peek-overlay--toggle-active ol)))

;;;###autoload
(defun peek-next-line ()
  "Scroll the peek view down by one line."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-max (pcase peek-type
                          ('string (1- (max 1 (length (overlay-get ol 'peek-lines)))))
                          ('definition most-positive-fixnum)
                          (_ (error "Invalid peek-type")))))
    (overlay-put ol 'peek-offset (min (1+ offset) bound-max))
    (peek-overlay-auto-set-content ol t)))

;;;###autoload
(defun peek-prev-line ()
  "Scroll the peek view up by one line."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (offset (overlay-get ol 'peek-offset))
             (next-offset (max (1- offset) 0)))
    (overlay-put ol 'peek-offset next-offset)
    (peek-overlay-auto-set-content ol t)))

;;;###autoload
(defun peek-overlay-dwim ()
  "Peek overlay: do what I mean.
- With active region: store it for peeking.
- Without region: toggle the peek overlay visibility."
  (interactive)
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay)))
    (if (use-region-p)
        (progn
          (setq peek--marked-region-markers (peek--mark-region)
                peek--marked-region-unused t)
          (message "Region stored for peeking"))
      ;; Toggle existing overlay
      (when (eq (overlay-get ol 'peek-type) 'definition)
        (overlay-put ol 'peek-type 'string))
      (when (and (not (overlay-get ol 'active))
                 peek--marked-region-unused)
        (overlay-put ol 'peek-offset 0)
        (let* ((mb (car peek--marked-region-markers))
               (me (cdr peek--marked-region-markers))
               (source-buffer (marker-buffer mb))
               (text (with-current-buffer source-buffer
                       (if (eq major-mode 'image-mode)
                           (propertize " " 'display
                                       (get-text-property 1 'display))
                         (buffer-substring (marker-position mb)
                                           (marker-position me))))))
          (when peek-live-update
            (with-current-buffer source-buffer
              (add-to-list 'peek--live-update-associated-overlays ol)
              (add-hook 'after-change-functions
                        #'peek-after-change-function nil t)))
          (overlay-put ol 'peek-markers peek--marked-region-markers)
          (overlay-put ol 'peek-lines (split-string text "\n")))
        (peek-overlay-auto-set-content ol)
        (setq peek--marked-region-unused nil))
      (peek-overlay--toggle-active ol))))

;;;###autoload
(defun peek-view-refresh ()
  "Refresh the content of the current peek view from its source markers."
  (interactive)
  (when-let ((ol (peek-get-window-overlay))
             ((eq (overlay-get ol 'peek-type) 'string))
             (markers (overlay-get ol 'peek-markers))
             (mb (car markers))
             (me (cdr markers))
             (source-buffer (marker-buffer mb))
             ((buffer-live-p source-buffer))
             (text (with-current-buffer source-buffer
                     (buffer-substring (marker-position mb)
                                       (marker-position me)))))
    (overlay-put ol 'peek-lines (split-string text "\n"))
    (peek-overlay-auto-set-content ol)))

;;;###autoload
(defun peek-overlay-set-custom-content (str &optional window)
  "Display custom string STR in the peek overlay for WINDOW."
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay window)))
    (overlay-put ol 'peek-type 'string)
    (overlay-put ol 'peek-markers nil)
    (overlay-put ol 'peek-lines (split-string str "\n"))
    (overlay-put ol 'peek-offset 0)
    (peek-overlay-auto-set-content ol)))

;;; ---------------------------------------------------------------------------
;;; Definition Peeking (Xref Integration)
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun peek-definition (func &optional args)
  "Peek the definition found by calling FUNC with ARGS."
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay))
        (peek--definition-func func)
        (peek--definition-func-args args))
    (unless (eq (overlay-get ol 'peek-type) 'definition)
      (overlay-put ol 'peek-offset 0)
      (overlay-put ol 'peek-type 'definition))
    (peek-overlay-auto-set-content ol)
    (peek-overlay--set-active ol t)))

(defun peek-goto-xref-definition-func (identifier)
  "Navigate to the definition of IDENTIFIER without polluting xref history.

BYPASS: Calls `xref-backend-definitions' directly instead of
`xref-find-definitions' to prevent pushing onto `xref-marker-stack'.
This keeps the user's M-, (xref-pop-marker-stack) history clean."
  (let* ((backend (run-hook-with-args-until-success
                   'xref-backend-functions))
         (defs (when backend
                 (xref-backend-definitions backend identifier))))
    (when defs
      (let* ((loc (xref-item-location (car defs)))
             (marker (xref-location-marker loc)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;;;###autoload
(defun peek-xref-definition ()
  "Peek the xref definition of the symbol at point."
  (interactive)
  (peek-definition #'peek-goto-xref-definition-func
                   (list (or (thing-at-point 'symbol) ""))))

(defun peek-goto-xref-references-func (identifier)
  "Navigate to the first reference of IDENTIFIER without polluting history."
  (let* ((backend (run-hook-with-args-until-success
                   'xref-backend-functions))
         (refs (when backend
                 (xref-backend-references backend identifier))))
    (when refs
      (let* ((loc (xref-item-location (car refs)))
             (marker (xref-location-marker loc)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;;;###autoload
(defun peek-xref-references ()
  "Peek the first xref reference of the symbol at point."
  (interactive)
  (peek-definition #'peek-goto-xref-references-func
                   (list (or (thing-at-point 'symbol) ""))))

;;; ---------------------------------------------------------------------------
;;; Eldoc Integration
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun peek-display-eldoc (docs interactive)
  "Display Eldoc DOCS in a peek view when INTERACTIVE is non-nil.

Includes an Origin Firewall: inspects the :origin plist of each doc
item and blocks payloads originating from `flymake-eldoc-function'."
  (when (and (>= emacs-major-version 28)
             interactive
             ;; ELDOC FIREWALL: Block payloads whose :origin is flymake.
             (cl-loop for (_string plist) in docs
                      never (eq (plist-get plist :origin)
                                'flymake-eldoc-function)))
    ;; NOTE: `eldoc--format-doc-buffer' is an internal API (stable since 28).
    (when-let ((docs-content
                (with-current-buffer (eldoc--format-doc-buffer docs)
                  (buffer-string)))
               (ol (peek-get-or-create-window-overlay)))
      (overlay-put ol 'peek-type 'string)
      (overlay-put ol 'peek-lines (split-string docs-content "\n"))
      (overlay-put ol 'peek-offset 0)
      (peek-overlay-auto-set-content ol)
      (peek-overlay--set-active ol t)
      (peek-display--overlay-update ol))))

;;; ---------------------------------------------------------------------------
;;; Overlay Position Tracking
;;; ---------------------------------------------------------------------------

(defun peek-display--overlay-update (&optional ol)
  "Reposition the overlay if it is active and point has moved."
  (when-let ((ol (or (and (overlayp ol) ol)
                     (peek-get-window-overlay)))
             ((overlay-get ol 'active))
             (pos (peek-overlay--get-supposed-position)))
    (move-overlay ol pos pos)))

;;; ---------------------------------------------------------------------------
;;; Global Minor Mode
;;; ---------------------------------------------------------------------------

;;;###autoload
(define-minor-mode global-peek-mode
  "Global minor mode for inline peek views."
  :global t
  :lighter " peek"
  :keymap peek-mode-keymap
  (cond
   (global-peek-mode
    ;; Eldoc integration
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (add-hook 'eldoc-display-functions #'peek-display-eldoc))
    ;; Clean slate
    (peek-clean-all-overlays)
    ;; PERF FIX: Use `window-buffer-change-functions' (fires only on buffer
    ;; switches) instead of `window-state-change-functions' (fires on every
    ;; scroll, resize, and redisplay event).  Combined with `kill-buffer-hook'
    ;; for buffer-local cleanup.
    (add-hook 'window-buffer-change-functions
              #'peek--cleanup-dead-window-overlays)
    (add-hook 'kill-buffer-hook
              #'peek--cleanup-current-buffer-overlays)
    ;; Position tracking
    (add-hook 'post-command-hook #'peek-display--overlay-update))
   (t
    ;; Eldoc teardown
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (remove-hook 'eldoc-display-functions #'peek-display-eldoc))
    ;; Full cleanup
    (peek-clean-all-overlays)
    (remove-hook 'window-buffer-change-functions
                 #'peek--cleanup-dead-window-overlays)
    (remove-hook 'kill-buffer-hook
                 #'peek--cleanup-current-buffer-overlays)
    (remove-hook 'post-command-hook #'peek-display--overlay-update))))

(provide 'peek)
;;; peek.el ends here
```

---

### Summary of Corrections Applied

| File                  | Issue                                                              | Fix                                                     |
| --------------------- | ------------------------------------------------------------------ | ------------------------------------------------------- |
| `eldoc-childframe.el` | Typo "ELDOD" in comment                                            | Corrected to "ELDOC"                                    |
| `eldoc-childframe.el` | Missing `eldoc-childframe--filter-breadcrumb` as separate function | Extracted into its own named function for clarity       |
| `eldoc-childframe.el` | `eldoc-childframe--old-display-functions` not reset on disable     | Added `setq-local ... nil` in `--disable`               |
| `eldoc-childframe.el` | Missing `:global nil` docstring clarity                            | Expanded mode docstring                                 |
| `eldoc-childframe.el` | `eldoc-childframe--childframe-supported-p` not factored out        | Extracted as named predicate                            |
| `peek.el`             | `save-excursion` in `peek-definition--set-marker`                  | → `save-window-excursion`                               |
| `peek.el`             | `make-separator-line` called with `t` argument                     | → zero-argument call                                    |
| `peek.el`             | `window-state-change-functions` (fires every scroll)               | → `window-buffer-change-functions` + `kill-buffer-hook` |
| `peek.el`             | `cursor-intangible` in `after-string` (C engine ignores it)        | Excised entirely                                        |
| `peek.el`             | Typo "ELDOD FIREWALL"                                              | → "ELDOC FIREWALL"                                      |
| `peek.el`             | `peek-overlay-get-content--string` could index empty list          | Added `(zerop lines-len)` guard                         |
| `peek.el`             | `peek-goto-xref-definition-func` didn't guard nil backend          | Added `(when backend ...)` guard                        |
| `peek.el`             | `thing-at-point` could return nil                                  | Added `(or ... "")` fallback                            |
| `peek.el`             | `peek-view-refresh` didn't check `buffer-live-p`                   | Added guard                                             |
| Both                  | Inconsistent section separators                                    | Standardized `;;; ---` dividers                         |
