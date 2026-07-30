;;; eval-overlay.el --- Universal evaluation result display engine -*- lexical-binding: t; -*-

;; Author: Ahsanur Rahman
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (posframe "1.4") (quickrun "2.3") (eros "0.2"))
;; Keywords: convenience, tools

;;; Commentary:
;; A universal evaluation result display engine that routes code execution
;; output through a three-tier display system:
;;
;;   Tier 1 (≤2 lines, ≤80 cols): eros-style inline overlay at EOL
;;   Tier 2 (3–20 lines):         posframe childframe near cursor (PRIMARY)
;;   Tier 3 (>20 lines):          dedicated bottom popup buffer
;;
;; Execution is delegated to `quickrun' for universal language support.
;; The quickrun sentinel is intercepted via `:filter-return' advice to
;; capture stdout before the default popup fires.
;;
;; Architecture (mirrors Doom Emacs `tools/eval +overlay'):
;;   quickrun--make-sentinel  ←── :filter-return advice (wraps sentinel)
;;   quickrun--pop-to-buffer  ←── :override advice (suppresses popup)
;;   quickrun--recenter       ←── :override advice (no window to recenter)
;;   quickrun-output-only     ←── set to t (suppresses header/footer)
;;
;; Integrates with:
;;   - posframe  (childframe display, inherits user's border theming)
;;   - eros      (inline overlay rendering engine, used as a library)
;;   - evil      (visual selection awareness)
;;   - general.el (leader keybindings, configured externally)

;;; Code:

(require 'posframe)

;; Silence byte-compiler for optional dependencies loaded at runtime.
(declare-function quickrun "quickrun")
(declare-function quickrun-region "quickrun")
(declare-function quickrun-replace-region "quickrun")
(declare-function eros--make-result-overlay "eros")
(declare-function eros--remove-result-overlay "eros")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")

;; ═══════════════════════════════════════════════════════════════════
;; CUSTOMIZATION
;; ═══════════════════════════════════════════════════════════════════

(defgroup eval-overlay nil
  "Universal evaluation result display engine."
  :prefix "eval-overlay-"
  :group 'tools)

(defcustom eval-overlay-inline-max-lines 2
  "Maximum number of lines for inline overlay display (Tier 1).
Results with more lines are routed to the posframe childframe."
  :type 'integer
  :group 'eval-overlay)

(defcustom eval-overlay-inline-max-width 80
  "Maximum string width for inline overlay display (Tier 1).
Results wider than this are routed to the posframe childframe."
  :type 'integer
  :group 'eval-overlay)

(defcustom eval-overlay-posframe-max-lines 20
  "Maximum number of lines for posframe childframe display (Tier 2).
Results with more lines are routed to the popup buffer."
  :type 'integer
  :group 'eval-overlay)

(defcustom eval-overlay-posframe-auto-hide t
  "If non-nil, auto-hide the posframe on the next command."
  :type 'boolean
  :group 'eval-overlay)

(defcustom eval-overlay-posframe-timeout 30
  "Seconds before auto-hiding the posframe.  Nil means no timeout."
  :type '(choice (integer :tag "Seconds")
          (const :tag "No timeout" nil))
  :group 'eval-overlay)

(defcustom eval-overlay-result-prefix "⇒ "
  "Prefix string displayed before inline evaluation results."
  :type 'string
  :group 'eval-overlay)

(defcustom eval-overlay-popup-height 0.3
  "Height fraction of the frame for the popup buffer (Tier 3)."
  :type 'float
  :group 'eval-overlay)

;; ═══════════════════════════════════════════════════════════════════
;; FACES
;; ═══════════════════════════════════════════════════════════════════

(defface eval-overlay-result-face
  '((((background dark))
     :background "#292e42" :foreground "#9ece6a"
     :box (:line-width -1 :color "#3b4261"))
    (((background light))
     :background "#d5d6db" :foreground "#2c780a"
     :box (:line-width -1 :color "#9ca0a4")))
  "Face for inline evaluation result overlays."
  :group 'eval-overlay)

(defface eval-overlay-error-face
  '((((background dark))
     :background "#292e42" :foreground "#f7768e"
     :box (:line-width -1 :color "#3b4261"))
    (((background light))
     :background "#d5d6db" :foreground "#c4314b"
     :box (:line-width -1 :color "#9ca0a4")))
  "Face for evaluation error overlays."
  :group 'eval-overlay)

(defface eval-overlay-posframe-header
  '((((background dark))
     :foreground "#7aa2f7" :weight bold :height 0.9)
    (((background light))
     :foreground "#2e5aac" :weight bold :height 0.9))
  "Face for the posframe header line."
  :group 'eval-overlay)

;; ═══════════════════════════════════════════════════════════════════
;; INTERNAL STATE
;; ═══════════════════════════════════════════════════════════════════

(defvar eval-overlay--posframe-buffer " *eval-overlay-result*"
  "Buffer name for the posframe childframe.")

(defvar eval-overlay--popup-buffer "*Eval Output*"
  "Buffer name for the popup output window (Tier 3).")

(defvar eval-overlay--posframe-timer nil
  "Timer for auto-hiding the posframe.")

(defvar eval-overlay--last-error nil
  "Non-nil if the last evaluation produced an error.")

(defvar eval-overlay--force-popup nil
  "When non-nil, force the next result into the popup buffer.
Captured from `current-prefix-arg' at command invocation time,
because the sentinel fires asynchronously after the prefix arg
has been cleared.")

;; ═══════════════════════════════════════════════════════════════════
;; TIER 1: INLINE OVERLAY (eros engine)
;; ═══════════════════════════════════════════════════════════════════

(defun eval-overlay--display-inline (output source-buffer)
  "Display OUTPUT as an inline overlay in SOURCE-BUFFER.
Uses the eros overlay engine for rendering.  Passes an explicit
cons cell to `:where' to prevent eros from calling `backward-sexp',
which can fail in indentation-sensitive modes like Python."
  (with-current-buffer source-buffer
    (let* ((this-command #'eval-overlay-region-or-buffer)
           (eros-eval-result-prefix eval-overlay-result-prefix)
           (eros-overlays-use-font-lock nil)
           (face (if eval-overlay--last-error
                     'eval-overlay-error-face
                   'eval-overlay-result-face))
           (display-str (string-trim output))
           (lines (split-string display-str "\n"))
           (prefixlen (length eval-overlay-result-prefix))
           (max-len (+ (apply #'max (mapcar #'length lines))
                       prefixlen))
           ;; If the result is multi-line or too wide, place the overlay
           ;; at the beginning of the next line with indentation padding.
           (next-line-p (or (cdr lines)
                            (< (- (window-width)
                                  (save-excursion
                                    (goto-char (pos-eol))
                                    (- (current-column) (window-hscroll))))
                               max-len)))
           (pad (if next-line-p (+ (window-hscroll) prefixlen) 0)))
      ;; Truncate extremely long single-line results.
      (when (> (string-width display-str) (* 3 (window-width)))
        (setq display-str
              (concat (substring display-str 0 (* 3 (window-width)))
                      "…\nResult truncated.")))
      (eros--make-result-overlay
	  (concat (make-string (max 0 (- pad prefixlen)) ?\s)
		  eval-overlay-result-prefix
		  (string-join lines
                               (concat hard-newline
                                       (make-string pad ?\s))))
	:where (if next-line-p
                   (cons (pos-bol 2) (pos-bol 2))
                 (cons (pos-bol) (pos-eol)))
	:duration eros-eval-result-duration
	:prepend-face face
	:format "%s"))))

;; ═══════════════════════════════════════════════════════════════════
;; TIER 2: POSFRAME CHILDFRAME (primary display)
;; ═══════════════════════════════════════════════════════════════════

(defun eval-overlay--posframe-cleanup ()
  "Hide the evaluation posframe and cancel timers."
  (when eval-overlay--posframe-timer
    (cancel-timer eval-overlay--posframe-timer)
    (setq eval-overlay--posframe-timer nil))
  (when (posframe-workable-p)
    (posframe-hide eval-overlay--posframe-buffer)))

(defun eval-overlay--posframe-auto-hide-hook ()
  "Auto-hide posframe on next command.
Skips dismissal when the next command is another evaluation."
  (unless (memq this-command
                '(eval-overlay-posframe-dismiss
                  eval-overlay-buffer
                  eval-overlay-region
                  eval-overlay-line
                  eval-overlay-region-or-buffer))
    (eval-overlay--posframe-cleanup)
    (remove-hook 'pre-command-hook
                 #'eval-overlay--posframe-auto-hide-hook)))

(defun eval-overlay--display-posframe (output source-buffer)
  "Display OUTPUT in a posframe childframe anchored to SOURCE-BUFFER.
Falls back to the popup buffer (Tier 3) when posframe is unavailable
\\(e.g., TTY sessions without `tty-child-frames'\\)."
  (if (not (posframe-workable-p))
      (eval-overlay--display-popup output source-buffer)
    ;; Cancel any existing timer before creating a new one.
    (eval-overlay--posframe-cleanup)
    (let* ((buf (get-buffer-create eval-overlay--posframe-buffer))
           (mode-name-str (buffer-local-value 'mode-name source-buffer))
           (header (concat
                    (propertize
                     (if eval-overlay--last-error " ✗ Error" " ✓ Result")
                     'face 'eval-overlay-posframe-header)
                    (propertize
                     (format "  [%s]" mode-name-str)
                     'face 'shadow)))
           (body (string-trim output))
           (separator (propertize (make-string 60 ?─) 'face 'shadow))
           (content (concat header "\n" separator "\n" body "\n")))
      ;; Populate the posframe buffer.
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert content)
          (goto-char (point-min))
          (setq buffer-read-only t)))
      ;; Calculate dimensions from buffer content.
      (let* ((line-count (with-current-buffer buf
                           (count-lines (point-min) (point-max))))
             (max-w (with-current-buffer buf
                      (save-excursion
                        (goto-char (point-min))
                        (let ((mw 0))
                          (while (not (eobp))
                            (setq mw (max mw (- (pos-eol) (pos-bol))))
                            (forward-line 1))
                          mw))))
             (width (min (max (+ max-w 4) 40) 100))
             (height (min (max line-count 3) 25)))
        (with-current-buffer source-buffer
          (posframe-show buf
                         :poshandler #'posframe-poshandler-point-bottom-left-corner
                         :width width
                         :height height
                         :min-width 40
                         :min-height 3
                         :max-width 120
                         :max-height 30
                         :internal-border-width 8
                         :border-color (face-attribute 'region :background nil t)
                         :override-parameters '((cursor-type . nil)
                                                (no-accept-focus . t)
                                                (no-focus-on-map . t))
                         :respect-mode-line nil)))
      ;; Auto-hide behavior.
      (when eval-overlay-posframe-auto-hide
        (add-hook 'pre-command-hook
                  #'eval-overlay--posframe-auto-hide-hook))
      (when eval-overlay-posframe-timeout
        (setq eval-overlay--posframe-timer
              (run-at-time eval-overlay-posframe-timeout nil
                           #'eval-overlay--posframe-cleanup))))))

(defun eval-overlay-posframe-dismiss ()
  "Dismiss the evaluation posframe."
  (interactive)
  (eval-overlay--posframe-cleanup))

;; ═══════════════════════════════════════════════════════════════════
;; TIER 3: POPUP BUFFER (long output)
;; ═══════════════════════════════════════════════════════════════════

(defun eval-overlay--display-popup (output _source-buffer)
  "Display OUTPUT in a dedicated bottom popup buffer.
Routes to a 30%% bottom side-window following the Ghostel/Eshell
drawer paradigm already established in the configuration."
  (let ((buf (get-buffer-create eval-overlay--popup-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim output))
        (goto-char (point-min))
        (special-mode)))
    (let ((display-buffer-alist
           `((,eval-overlay--popup-buffer
              (display-buffer-in-side-window)
              (side . bottom)
              (window-height . ,eval-overlay-popup-height)
              (window-parameters (no-delete-other-windows . t))))))
      (pop-to-buffer buf))))

;; ═══════════════════════════════════════════════════════════════════
;; DISPLAY ROUTER
;; ═══════════════════════════════════════════════════════════════════

(defun eval-overlay-display-results (output source-buffer)
  "Route OUTPUT to the appropriate display tier.
SOURCE-BUFFER is the buffer where evaluation was initiated.

Routing logic:
  - `eval-overlay--force-popup' non-nil: always use popup buffer.
  - ≤2 lines and ≤80 chars wide: inline overlay (Tier 1).
  - 3–20 lines: posframe childframe (Tier 2).
  - >20 lines: popup buffer (Tier 3)."
  (let* ((trimmed (string-trim output))
         (lines (split-string trimmed "\n"))
         (line-count (length lines))
         (max-width (if lines
                        (apply #'max (mapcar #'string-width lines))
                      0)))
    (cond
     ;; Prefix arg forces popup.
     (eval-overlay--force-popup
      (eval-overlay--display-popup trimmed source-buffer))
     ;; Tier 1: Inline overlay (success only).
     ((and (<= line-count eval-overlay-inline-max-lines)
           (<= max-width eval-overlay-inline-max-width)
           (not eval-overlay--last-error))
      (eval-overlay--display-inline trimmed source-buffer))
     ;; Tier 2: Posframe childframe.
     ((<= line-count eval-overlay-posframe-max-lines)
      (eval-overlay--display-posframe trimmed source-buffer))
     ;; Tier 3: Popup buffer.
     (t
      (eval-overlay--display-popup trimmed source-buffer)))))

;; ═══════════════════════════════════════════════════════════════════
;; QUICKRUN INTEGRATION (Doom Emacs pattern)
;; ═══════════════════════════════════════════════════════════════════

;; Error detection: specific Python/language traceback patterns.
;; Avoids false positives from the bare word "Error" in normal output.
(defvar eval-overlay--error-regexp
  (concat "Traceback (most recent call last)"
          "\\|\\(?:Syntax\\|Name\\|Type\\|Value\\|Key\\|Index"
          "\\|Attribute\\|Import\\|Runtime\\|FileNotFound"
          "\\|ZeroDivision\\|Overflow\\|Memory\\)Error"
          "\\|\\(?:Unhandled\\|Fatal\\) [Ee]xception"
          "\\|Segmentation fault"
          "\\|panic:")
  "Regexp matching common language error/traceback patterns.")

(define-advice quickrun--make-sentinel
    (:filter-return eval-overlay--sentinel-filter)
  (sentinel)
  "Wrap the quickrun sentinel to intercept output.
After the original SENTINEL completes, extract the *quickrun* buffer
contents and route them through the three-tier display system."
  (lambda (process event)
    ;; Call the original sentinel first (handles footer, cleanup, etc.).
    (funcall sentinel process event)
    ;; Post-process: extract output and display via our router.
    (when (memq (process-status process) '(exit signal))
      (with-no-warnings
        (let ((quickrun-buf (get-buffer quickrun--buffer-name)))
          (when (and quickrun-buf (buffer-live-p quickrun-buf))
            (with-current-buffer quickrun-buf
              (let ((output (string-trim
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
                (when (> (length output) 0)
                  ;; Detect errors from exit status or traceback patterns.
                  (setq eval-overlay--last-error
                        (or (not (zerop (process-exit-status process)))
                            (string-match-p eval-overlay--error-regexp
                                            output)))
                  (eval-overlay-display-results
                   output
                   (or quickrun--original-buffer (current-buffer))))))))))))

(define-advice quickrun--pop-to-buffer
    (:override eval-overlay--inhibit-popup)
  (buf cb)
  "Suppress quickrun's default popup window.
Temporarily pops to BUF within `save-window-excursion' so the
window configuration is restored, sets the outputter to `ignore',
and calls CB.  This prevents the *quickrun* buffer from being
displayed permanently."
  (with-no-warnings
    (setq quickrun--original-buffer (current-buffer)))
  (save-window-excursion
    (with-current-buffer (pop-to-buffer buf)
      (with-no-warnings
        (setq quickrun-option-outputter #'ignore))
      (funcall cb))))

(define-advice quickrun--recenter
    (:override eval-overlay--inhibit-recenter)
  (_arg)
  "Suppress quickrun recenter.
No window exists for the *quickrun* buffer after popup inhibition,
so recentering would signal a `window-live-p' error.")

;; ═══════════════════════════════════════════════════════════════════
;; EVIL VISUAL REGION AWARENESS
;; ═══════════════════════════════════════════════════════════════════

(defun eval-overlay--get-region-bounds ()
  "Return (BEG . END) respecting Evil visual selections.
Falls back to standard `region-beginning'/`region-end'."
  (if (and (bound-and-true-p evil-local-mode)
           (eq (bound-and-true-p evil-state) 'visual))
      (cons (bound-and-true-p evil-visual-beginning)
            (bound-and-true-p evil-visual-end))
    (cons (region-beginning) (region-end))))

;; ═══════════════════════════════════════════════════════════════════
;; INTERACTIVE COMMANDS
;; ═══════════════════════════════════════════════════════════════════

;;;###autoload
(defun eval-overlay-buffer ()
  "Evaluate the entire buffer and display results via the overlay engine.
With prefix arg (\\[universal-argument]), force the popup buffer."
  (interactive)
  (eval-overlay--posframe-cleanup)
  (setq eval-overlay--last-error nil
        eval-overlay--force-popup (not (null current-prefix-arg)))
  (quickrun))

;;;###autoload
(defun eval-overlay-region (beg end)
  "Evaluate the region between BEG and END.
With prefix arg (\\[universal-argument]), force the popup buffer."
  (interactive "r")
  (eval-overlay--posframe-cleanup)
  (setq eval-overlay--last-error nil
        eval-overlay--force-popup (not (null current-prefix-arg)))
  (quickrun-region beg end))

;;;###autoload
(defun eval-overlay-line ()
  "Evaluate the current line.
With prefix arg (\\[universal-argument]), force the popup buffer."
  (interactive)
  (eval-overlay-region (pos-bol) (pos-eol)))

;;;###autoload
(defun eval-overlay-region-or-buffer ()
  "Evaluate the active region, or the entire buffer if no region is active.
With prefix arg (\\[universal-argument]), force the popup buffer."
  (interactive)
  (if (use-region-p)
      (call-interactively #'eval-overlay-region)
    (eval-overlay-buffer)))

;;;###autoload
(defun eval-overlay-region-and-replace (beg end)
  "Evaluate the region between BEG and END and replace it with the output."
  (interactive "r")
  (setq eval-overlay--last-error nil)
  (quickrun-replace-region beg end))

;;;###autoload
(defun eval-overlay-dismiss ()
  "Dismiss any visible evaluation result display.
Clears inline overlays, hides the posframe, and closes the popup buffer."
  (interactive)
  (eval-overlay--posframe-cleanup)
  (when (fboundp 'eros--remove-result-overlay)
    (eros--remove-result-overlay))
  (when-let* ((win (get-buffer-window eval-overlay--popup-buffer)))
    (delete-window win)))

;; ═══════════════════════════════════════════════════════════════════
;; MINOR MODE
;; ═══════════════════════════════════════════════════════════════════

;;;###autoload
(define-minor-mode eval-overlay-mode
  "Universal evaluation result display engine.
Intercepts quickrun output and routes it through a three-tier
display system: inline overlay, posframe childframe, or popup buffer.

When enabled, activates the following advice on quickrun internals:
  - `quickrun--make-sentinel' :filter-return — wraps the sentinel
  - `quickrun--pop-to-buffer' :override — suppresses the popup
  - `quickrun--recenter'      :override — prevents recenter errors

Sets `quickrun-output-only' to t to suppress header/footer noise."
  :lighter " EvalOv"
  :global t
  (if eval-overlay-mode
      (progn
        ;; Activate all three advice layers.
        (advice-add #'quickrun--make-sentinel :filter-return
                    #'eval-overlay--sentinel-filter)
        (advice-add #'quickrun--pop-to-buffer :override
                    #'eval-overlay--inhibit-popup)
        (advice-add #'quickrun--recenter :override
                    #'eval-overlay--inhibit-recenter)
        ;; Suppress quickrun header/footer at the source.
        (with-no-warnings
          (setq quickrun-output-only t)
          (setq quickrun-focus-p nil)))
    ;; Teardown: remove all advice and restore quickrun defaults.
    (advice-remove #'quickrun--make-sentinel
                   #'eval-overlay--sentinel-filter)
    (advice-remove #'quickrun--pop-to-buffer
                   #'eval-overlay--inhibit-popup)
    (advice-remove #'quickrun--recenter
                   #'eval-overlay--inhibit-recenter)
    (with-no-warnings
      (setq quickrun-output-only nil)
      (setq quickrun-focus-p t))
    (eval-overlay--posframe-cleanup)))

(provide 'eval-overlay)
;;; eval-overlay.el ends here
