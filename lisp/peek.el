;;; peek.el --- Peek anything at your fingertip (Emacs 31 Modernized Fork)  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Copyright (C) 2026 Ahsanur Rahman (Modernized Fork)

;; Version: 0.3.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Maintainer: Ahsanur Rahman <ahsanur041@proton.me>
;; Keywords: convenience, tools
;; URL: https://sr.ht/~meow_king/peek
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Modernized fork of peek.el for Emacs 31.
;;
;; v0.3.0 Changes (Emacs 31 Audit & Hardening):
;; - CRITICAL FIX: Replaced `save-excursion' with `save-window-excursion' in
;;   `peek-definition--set-marker'. `save-excursion' does not protect against
;;   `switch-to-buffer' mutating the active window's buffer state.
;; - CRITICAL FIX: Fixed `make-separator-line' fatal crash. Passing `t' to
;;   `make-separator-line' triggers `wrong-type-argument integerp t' inside
;;   `make-string'. Now uses the zero-arg variant which correctly handles
;;   `:extend t' and trailing newlines natively.
;; - PERF FIX: Replaced `window-state-change-functions' (which fires on every
;;   single scroll/resize event) with `window-buffer-change-functions' and
;;   `kill-buffer-hook'. Cleanup is now scoped to the current buffer's hash
;;   table, reducing overhead from O(B*W) per scroll to O(W_local) per switch.
;; - REDISPLAY FIX: Excised `cursor-intangible' text property injection inside
;;   `after-string'. The C redisplay engine ignores text properties inside
;;   virtual overlay strings for cursor intangibility.
;; - Kept the Eldoc Origin Firewall (inspects `:origin' plist to block flymake).
;; - Kept the direct `xref-backend-definitions' bypass to prevent history pollution.

;;; Code:

(require 'display-line-numbers)
(require 'cl-lib)
(require 'xref)
(require 'subr-x)
(require 'eldoc)

(defgroup peek nil
  "Peek mode."
  :group 'convenience)

(defcustom peek-overlay-position 'above
  "Specify whether the overlay should be laid above or below the point."
  :type '(choice (const :tag "above the point" above)
                 (const :tag "below the point" below)))

(defcustom peek-overlay-distance 2
  "Number of lines between the peek overlay window and the point.
0 means directly above/below the current line."
  :type 'natnum)

(defcustom peek-overlay-window-size 11
  "Height of the peek overlay window."
  :type 'natnum)

(defcustom peek-definition-surrounding-above-lines 1
  "Number of lines above the xref definition to be shown in peek view."
  :type 'natnum)

(defcustom peek-live-update t
  "Whether to automatically update content when text in marked region changes."
  :type 'boolean)

(defcustom peek-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'peek-next-line)
    (define-key map (kbd "M-p") #'peek-prev-line)
    (define-key map (kbd "C-n") #'peek-next-line)
    (define-key map (kbd "C-p") #'peek-prev-line)
    map)
  "Keymap used for peek mode."
  :type 'keymap)

(defface peek-overlay-border-face
  '((t (:inherit separator-line :extend t)))
  "Face for borders of peek overlay window.")

(defface peek-overlay-content-face
  '((((background light))
     :background "#ecf0f1" :extend t)
    (t
     :background "#24283b" :extend t))
  "Additional face for content text of peek overlay window.")

(defcustom peek-enable-eldoc-display-integration nil
  "Show eldoc docs inside a peek view.
Requires Emacs >= 28.1."
  :type 'boolean)

(defvar-local peek--window-overlay-map nil
  "Buffer-local window-overlay map.")

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

;;; =============================================================================
;;; Base Functions
;;; =============================================================================

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

(defun peek-clean-all-overlays ()
  "Clean all overlays in all buffers."
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
  (let ((w (or (and (windowp window) window)
               (get-buffer-window))))
    (when w
      (gethash w peek--window-overlay-map))))

(defun peek-delete-window-overlay (&optional window)
  "Delete the overlay inside WINDOW or current window."
  (peek--ensure-window-overlay-map)
  (let ((w (or (and (windowp window) window)
               (get-buffer-window))))
    (when w
      (when-let ((ol (gethash w peek--window-overlay-map)))
        (delete-overlay ol))
      (remhash w peek--window-overlay-map))))

(defun peek-create-overlay (pos)
  "Create overlay for current window at POS."
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
  "Get or create an overlay for WINDOW."
  (let ((ol (peek-get-window-overlay window)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    ol))

(defun peek-overlay-get-content (ol)
  "Get the content of OL based on its peek-type."
  (let ((peek-type (overlay-get ol 'peek-type)))
    (pcase peek-type
      ('string (peek-overlay-get-content--string ol))
      ('definition (peek-overlay-get-content--definition ol)))))

(defun peek-overlay--set-active (ol active)
  "Set active/visibility of the given overlay OL to ACTIVE."
  (when (booleanp active)
    (if active
        (progn
          (overlay-put ol 'active t)
          (peek-overlay--set-content ol (peek-overlay-get-content ol)))
      (overlay-put ol 'active nil)
      (overlay-put ol 'after-string nil))))

(defun peek-overlay--protect-string-looking (str)
  "Protect string STR looking by adding a `default' face property."
  (let ((strlen (length str)))
    (add-face-text-property 0 strlen 'default 'append str)
    str))

(defun peek-overlay--format-content (str)
  "Format peek overlay content STR and return the formatted string."
  (let* ((strlen (length str))
         ;; FIX: `make-separator-line' with no args returns a properly propertized
         ;; newline with `:extend t' on GUI, or a dashed line on TTY. It already
         ;; includes the trailing newline. Passing `t' crashes Emacs 31.
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
    ;; FIX: Excised `cursor-intangible' text property injection. The C redisplay
    ;; engine ignores text properties inside `after-string' for cursor intangibility.
    (concat
     border-top
     str
     (unless (string-suffix-p "\n" str) "\n")
     border-bot)))

(defun peek-overlay--set-content (ol str)
  "Set the content STR for OL."
  (when (overlay-get ol 'active)
    (let ((content (peek-overlay--format-content str)))
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
  "Detect whether the two regions overlap."
  (if (or (< r1e r1s) (< r2e r2s))
      (error "Region bounds are inverted")
    (not (or (<= r2e r1s) (<= r1e r2s)))))

;;; =============================================================================
;;; Main Functions
;;; =============================================================================

(defun peek-display--overlay-update (&optional ol)
  "Update the overlay position if overlay is active."
  (when-let ((ol (or (and (overlayp ol) ol)
                     (peek-get-window-overlay)))
             ((overlay-get ol 'active))
             (pos (peek-overlay--get-supposed-position)))
    (move-overlay ol pos pos)))

(defun peek-after-change-function (rb re _plen)
  "Live update peek view after buffer changes.
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

;;;###autoload
(defun peek-display-eldoc (docs interactive)
  "Display eldoc DOCS in peek view.
Related function: `eldoc-display-functions'.
Includes an Origin Firewall: inspects the :origin plist of each doc
item and blocks payloads originating from `flymake-eldoc-function'."
  (when (>= emacs-major-version 28)
    (when-let ((interactive)
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
      (peek-overlay-auto-set-content ol)
      (peek-overlay--set-active ol t)
      (peek-display--overlay-update ol))))

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
ULD: use last definition."
  (let ((peek-type (overlay-get ol 'peek-type)))
    (pcase peek-type
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
      (_ (error "Invalid peek-type!")))))

;;;###autoload
(defun peek-next-line ()
  "Scroll down current peek view 1 line."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
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
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-min (pcase peek-type
                          ('string 0)
                          ('definition 0)
                          (_ (error "Invalid peek-type!"))))
             (next-offset (max (1- offset) bound-min)))
    (overlay-put ol 'peek-offset next-offset)
    (peek-overlay-auto-set-content ol t)))

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
    (add-hook 'post-command-hook #'peek-display--overlay-update))
   (t
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (remove-hook 'eldoc-display-functions #'peek-display-eldoc))
    (peek-clean-all-overlays)
    (remove-hook 'window-buffer-change-functions #'peek--cleanup-dead-window-overlays)
    (remove-hook 'kill-buffer-hook #'peek--cleanup-current-buffer-overlays)
    (remove-hook 'post-command-hook #'peek-display--overlay-update))))

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
          (message "region stored"))
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
            (overlay-put ol 'peek-lines (split-string text "\n")))
          (peek-overlay-auto-set-content ol)
          (setq peek--marked-region-unused nil))
        (peek-overlay--toggle-active ol)))))

;;;###autoload
(defun peek-view-refresh ()
  "Refresh content in the current peek view."
  (interactive)
  (when-let ((ol (peek-get-window-overlay))
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
(defun peek-overlay-set-custom-content (str &optional window)
  "Set custom content STR for the peek overlay window."
  (unless global-peek-mode (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay window)))
    (overlay-put ol 'peek-type 'string)
    (overlay-put ol 'peek-markers nil)
    (overlay-put ol 'peek-lines (split-string str "\n"))
    (peek-overlay-auto-set-content ol)))

;;;###autoload
(defun peek-definition (func &optional args)
  "Peek the definition using given FUNC and ARGS."
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
  "Go to definition of IDENTIFIER using xref backend directly.
Bypasses `xref-find-definitions' to prevent polluting `xref-marker-stack'."
  (let* ((backend (run-hook-with-args-until-success 'xref-backend-functions))
         (defs (xref-backend-definitions backend identifier)))
    (when defs
      (let* ((loc (xref-item-location (car defs)))
             (marker (xref-location-marker loc)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;;;###autoload
(defun peek-xref-definition ()
  "Peek xref definition."
  (interactive)
  (peek-definition
   #'peek-goto-xref-definition-func
   (list (thing-at-point 'symbol))))

(defun peek-goto-xref-references-func (identifier)
  "Go to the first reference of IDENTIFIER without polluting history."
  (let* ((backend (run-hook-with-args-until-success 'xref-backend-functions))
         (refs (xref-backend-references backend identifier)))
    (when refs
      (let* ((loc (xref-item-location (car refs)))
             (marker (xref-location-marker loc)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;;;###autoload
(defun peek-xref-references ()
  "Peek xref references inline."
  (interactive)
  (peek-definition #'peek-goto-xref-references-func (list (thing-at-point 'symbol))))

(provide 'peek)
;;; peek.el ends here
