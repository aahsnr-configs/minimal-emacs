;;; eldoc-childframe.el --- Optimized childframe documentation viewer -*- lexical-binding: t; -*-
;; Author: Ahsanur Rahman (Forked from eldoc-box by Yuan Fu)
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1"))
;;; Commentary:
;; A modernized, stripped-down fork of `eldoc-box` optimized for Emacs 31.
;; Features:
;; - Intelligent Display Routing: ≤2 lines → echo area, >2 lines → childframe.
;; - Flymake Firewall: Filters out diagnostic payloads from childframe display.
;; - TTY Degradation: Gracefully falls back to echo area on non-graphical frames.
;; - Evil Motion Fix: Spatial debouncing replaces the 0.5s inhibition trap.
;; - Corfu Collision Avoidance: Shifts the childframe to avoid overlapping popups.
;;; Code:

(require 'cl-lib)
(require 'eldoc)

(defgroup eldoc-childframe nil
  "Display Eldoc docs in a floating childframe."
  :prefix "eldoc-childframe-"
  :group 'eldoc)

(defface eldoc-childframe-border
  '((((background dark)) . (:background "#292e42"))
    (((background light)) . (:background "#292e42")))
  "The border color used in childframe.")

(defface eldoc-childframe-body
  '((t (:background "#24283b" :foreground "#c0caf5")))
  "Body face used in documentation childframe.")

(defface eldoc-childframe-markdown-separator
  '((t (:inherit shadow :strike-through t :height 0.4 :extend t)))
  "Face for the separator line in Markdown.")

(defcustom eldoc-childframe-only-multi-line t
  "If non-nil, only use childframe when there are more than 2 lines."
  :type 'boolean)

(defcustom eldoc-childframe-clear-with-C-g t
  "If set to non-nil, clear childframe on \\[keyboard-quit]."
  :type 'boolean)

(defcustom eldoc-childframe-max-pixel-width 900
  "Maximum width of doc childframe in pixel."
  :type 'number)

(defcustom eldoc-childframe-max-pixel-height 800
  "Maximum height of doc childframe in pixel."
  :type 'number)

(defcustom eldoc-childframe-offset '(16 16 16)
  "Sets left, right & top offset of the doc childframe.
Its value should be a list: (left right top)"
  :type '(list (integer :tag "Left")
          (integer :tag "Right")
          (integer :tag "Top")))

(defvar eldoc-childframe-frame-parameters
  '((left . -1)
    (top . -1)
    (width  . 0)
    (height  . 0)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (min-height  . 0)
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
  "Frame parameters used to create the frame.")

(defvar eldoc-childframe--frame nil
  "The frame to display doc.")

(defvar eldoc-childframe--buffer " *eldoc-childframe*"
  "The buffer used to render documentation.")

(defvar eldoc-childframe--last-point 0
  "Last point where childframe was shown.")

(defvar eldoc-childframe-frame-hook nil
  "Hook run after doc frame is setup but just before it is made visible.")

(defvar eldoc-childframe-buffer-hook
  '(eldoc-childframe--prettify-markdown-separator
    eldoc-childframe--replace-en-space
    eldoc-childframe--remove-linked-images
    eldoc-childframe--remove-noise-chars
    eldoc-childframe--fontify-html
    eldoc-childframe--condense-large-newline-gaps)
  "Hook run after buffer for doc is setup.")

;;; Frame Management

(defun eldoc-childframe--frame-visible-p ()
  "Return t when the childframe is visible."
  (and eldoc-childframe--frame
       (frame-live-p eldoc-childframe--frame)
       (frame-visible-p eldoc-childframe--frame)))

(defun eldoc-childframe-quit-frame ()
  "Hide documentation childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (make-frame-invisible eldoc-childframe--frame t)))

(defun eldoc-childframe--quit-frame-not-in-childframe ()
  "Hide documentation childframe unless point is in it."
  (interactive)
  (when (and eldoc-childframe-clear-with-C-g
             (not (eq (selected-frame) eldoc-childframe--frame)))
    (eldoc-childframe-quit-frame)))

(defun eldoc-childframe-scroll-up (arg)
  "Scroll up ARG lines in the childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-up arg))))

(defun eldoc-childframe-scroll-down (arg)
  "Scroll down ARG lines in the childframe."
  (interactive "p")
  (when (eldoc-childframe--frame-visible-p)
    (with-selected-frame eldoc-childframe--frame
      (scroll-down arg))))

(defun eldoc-childframe-focus-frame ()
  "Switch focus to the childframe."
  (interactive)
  (when (eldoc-childframe--frame-visible-p)
    (set-frame-parameter eldoc-childframe--frame 'no-accept-focus nil)
    (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map nil)
    (select-frame-set-input-focus eldoc-childframe--frame)
    (setq cursor-type 'bar)
    (local-set-key (kbd "q") #'eldoc-childframe-quit-frame)))

(defun eldoc-childframe--get-frame (buffer)
  "Return a childframe displaying BUFFER."
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (parameter (append eldoc-childframe-frame-parameters
                            `((minibuffer . ,(minibuffer-window)))))
         window frame
         (main-frame (selected-frame)))
    (if (and eldoc-childframe--frame (frame-live-p eldoc-childframe--frame))
        (progn (setq frame eldoc-childframe--frame)
               (setq window (frame-selected-window frame))
               (set-frame-parameter frame 'parent-frame main-frame))
      (setq window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,parameter)
                      (no-other-window . t)
                      (no-delete-other-windows . t))))
      (setq frame (window-frame window)))
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
    (eldoc-childframe--update-geometry frame window)
    (set-window-margins window nil nil)
    (setq eldoc-childframe--frame frame)
    (with-selected-frame frame
      (set-frame-parameter nil 'left-fringe 0)
      (set-frame-parameter nil 'right-fringe 0)
      (run-hook-with-args 'eldoc-childframe-frame-hook main-frame))
    (make-frame-visible frame)))

(defun eldoc-childframe--update-geometry (frame window)
  "Update the size and the position of childframe."
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
  "Calculate (X . Y) position for childframe of WIDTH and HEIGHT."
  (let* ((point-pos (pos-visible-in-window-p (point) nil t))
         (x (+ (car point-pos)
               (nth 0 (window-edges nil nil nil t))
               (frame-char-width)))
         (y (+ (cadr point-pos)
               (nth 1 (window-edges nil nil nil t))
               (frame-char-height)))
         (parent-w (frame-pixel-width))
         (parent-h (frame-pixel-height)))
    ;; Corfu collision avoidance
    (when (and (boundp 'corfu--frame)
               (frame-live-p corfu--frame)
               (frame-visible-p corfu--frame))
      (setq x (+ (car (frame-position corfu--frame))
                 (frame-pixel-width corfu--frame))))
    ;; Boundary clamping
    (when (> (+ x width) parent-w)
      (setq x (max 0 (- parent-w width 16))))
    (if (> (+ y height) parent-h)
        (setq y (max 0 (- y height (frame-char-height))))
      (setq y (+ y (frame-char-height))))
    (cons x y)))

(defun eldoc-childframe--display (str)
  "Display STR in the childframe."
  (let ((doc-buffer (get-buffer-create eldoc-childframe--buffer)))
    (with-current-buffer doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (buffer-face-set 'eldoc-childframe-body)
        (visual-line-mode 1)
        (run-hooks 'eldoc-childframe-buffer-hook)))
    (let ((frame (eldoc-childframe--get-frame doc-buffer)))
      (setq eldoc-childframe--last-point (point))
      (make-frame-visible frame))))

;;; Flymake Firewall

(defun eldoc-childframe--filter-flymake (docs)
  "Filter out Flymake diagnostic payloads from DOCS.
Inspects the :origin plist key added by ElDoc to each doc item."
  (cl-remove-if (lambda (doc)
                  (eq (plist-get (cdr doc) :origin) 'flymake-eldoc-function))
                docs))

;;; Intelligent Display Routing

(defun eldoc-childframe--compose-doc (doc)
  "Compose a single DOC item into a display string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    (concat (if thing
                (concat (propertize (format "%s" thing) 'face face) ": ")
              "")
            (car doc))))

(defun eldoc-childframe--route-display (docs interactive)
  "Route DOCS to echo area (≤2 lines) or childframe (>2 lines).
This function is the sole display router in `eldoc-display-functions'.
It internally delegates to `eldoc-display-in-echo-area' for short docs
and renders long docs in the childframe.

NOTE: `eldoc-display-functions' is run via `run-hook-with-args' (NOT
until-success), so all members always execute. This function handles
both display paths internally to avoid double-rendering."
  (let* ((filtered (eldoc-childframe--filter-flymake docs))
         (composed (string-join (mapcar #'eldoc-childframe--compose-doc
                                        filtered)
                                "\n"))
         (line-count (if (string-empty-p composed)
                         0
                       (1+ (cl-count ?\n composed)))))
    (cond
     ;; Nothing to display: clear echo area.
     ((zerop line-count)
      (eldoc--message nil))
     ;; ≤2 lines: delegate to the native echo area renderer.
     ((<= line-count 2)
      (eldoc-display-in-echo-area filtered interactive))
     ;; >2 lines: render in childframe (with TTY guard).
     (t
      (if (or (display-graphic-p) (featurep 'tty-child-frames))
          (let ((doc (string-trim composed)))
            (unless (string-empty-p doc)
              (eldoc-childframe--display doc)))
        ;; TTY fallback: truncate to echo area.
        (eldoc-display-in-echo-area filtered interactive))))))

;;; Evil Motion Fix (Spatial Debounce)

(defun eldoc-childframe--follow-cursor ()
  "Update or hide childframe based on cursor movement."
  (if (eq (point) eldoc-childframe--last-point)
      (when (eldoc-childframe--frame-visible-p)
        (eldoc-childframe--update-geometry
         eldoc-childframe--frame
         (frame-selected-window eldoc-childframe--frame)))
    ;; Point moved: hide frame instantly without 0.5s penalty.
    (eldoc-childframe-quit-frame)))

;;; Help at Point & Glance

(defun eldoc-childframe-help-at-point ()
  "Display documentation of the symbol at point on demand."
  (interactive)
  (cond
   ((eldoc-childframe--frame-visible-p)
    (eldoc-childframe-focus-frame))
   (t
    (when (boundp 'eldoc--doc-buffer)
      (let ((doc (with-current-buffer eldoc--doc-buffer (buffer-string))))
        (eldoc-childframe--display
         (if (equal doc "") "No doc to display at this point" doc)))
      (setq eldoc-childframe--last-point (point))))))

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

;;; Markdown Prettifiers

(defun eldoc-childframe--prettify-markdown-separator ()
  "Prettify the markdown separator in doc returned by Eglot."
  (save-excursion
    (goto-char (point-min))
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
                               '(face eldoc-childframe-markdown-separator)))))))

(defun eldoc-childframe--replace-en-space ()
  "Display the en spaces in documentation as regular spaces."
  (face-remap-set-base 'nobreak-space '(:inherit default))
  (face-remap-set-base 'markdown-line-break-face '(:inherit default)))

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
  "Remove embedded image links from documentation."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx "[" (seq "![" (+? anychar) "](" (+? anychar) ")") "]"
                "(" (+? anychar) ")")
            nil t)
      (replace-match ""))))

(defun eldoc-childframe--remove-noise-chars ()
  "Remove some noise characters like carriage return."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun eldoc-childframe--fontify-html ()
  "Fontify HTML tags and special entities."
  (save-excursion
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
    (goto-char (point-min))
    (while (re-search-forward
            (rx (group "<p>") (group (*? anychar)) (group "</p>"))
            nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (put-text-property (match-beginning 3) (match-end 3) 'invisible t))
    (goto-char (point-min))
    (while (re-search-forward (rx (or "&lt;" "&gt;" "&nbsp;")) nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (pcase (match-string 0)
                           ("&lt;" "<")
                           ("&gt;" ">")
                           ("&nbsp;" " "))))))

;;; Minor Mode

(defvar-local eldoc-childframe--old-eldoc-functions nil
  "The original value of `eldoc-display-functions'.")

(defun eldoc-childframe--enable ()
  "Enable eldoc-childframe with intelligent routing.
Replaces `eldoc-display-in-echo-area' with the router function."
  (setq-local eldoc-childframe--old-eldoc-functions eldoc-display-functions)
  (setq-local eldoc-display-functions
              (cons #'eldoc-childframe--route-display
                    (remq 'eldoc-display-in-echo-area
                          eldoc-display-functions)))
  (remove-hook 'pre-command-hook
               #'eldoc-pre-command-refresh-echo-area t)
  (add-hook 'post-command-hook
            #'eldoc-childframe--follow-cursor nil t)
  (when eldoc-childframe-clear-with-C-g
    (advice-add #'keyboard-quit :before
                #'eldoc-childframe--quit-frame-not-in-childframe)))

(defun eldoc-childframe--disable ()
  "Disable eldoc-childframe and restore original display functions."
  (setq-local eldoc-display-functions
              (remq #'eldoc-childframe--route-display
                    eldoc-display-functions))
  (when (memq 'eldoc-display-in-echo-area
              eldoc-childframe--old-eldoc-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-display-in-echo-area
                      eldoc-display-functions)))
  (add-hook 'pre-command-hook
            #'eldoc-pre-command-refresh-echo-area nil t)
  (remove-hook 'post-command-hook
               #'eldoc-childframe--follow-cursor t)
  (advice-remove #'keyboard-quit
                 #'eldoc-childframe--quit-frame-not-in-childframe)
  (when eldoc-childframe--frame
    (delete-frame eldoc-childframe--frame)
    (setq eldoc-childframe--frame nil)))

;;;###autoload
(define-minor-mode eldoc-childframe-hover-at-point-mode
  "Display documentation in a childframe at point."
  :lighter " eldoc-childframe"
  :global nil
  (if eldoc-childframe-hover-at-point-mode
      (eldoc-childframe--enable)
    (eldoc-childframe--disable)))

(provide 'eldoc-childframe)
;;; eldoc-childframe.el ends here
