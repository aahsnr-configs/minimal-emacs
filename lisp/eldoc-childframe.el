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
