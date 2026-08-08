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
