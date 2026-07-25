;;; org-eldoc.el --- Multi-layer echo-area composition engine for Org -*- lexical-binding: t; -*-
;;; Commentary:
;; Replaces the single-function router with 9 focused layers registered
;; into `eldoc-documentation-functions' at explicit depths.  Uses
;; `eldoc-documentation-compose' so all non-nil results are joined and
;; displayed together (≤2 lines → echo area, >2 → childframe via the
;; routing function in eldoc-childframe.el).
;;
;; Layers (display order = hook order, most-negative depth first):
;;  -100  Clock           – active org-clock task
;;   -90  Babel delegation – language-native eldoc inside src blocks
;;   -80  Link target     – denote/file/id/http link under point
;;   -75  Src header      – #+begin_src / #+end_src line metadata
;;   -70  Table cell      – cell coordinates and formula
;;   -60  Property drawer – key: value under point
;;   -40  Heading info    – TODO keyword, priority, tags
;;   -30  Timestamps      – SCHEDULED / DEADLINE / CLOSED
;;   -10  Breadcrumb      – cached imenu heading path (O(1) amortized)
;;; Code:

(require 'org)
(require 'ob-core)
(require 'eldoc)
(require 'org-element)

;; ---------------------------------------------------------------------------
;; Babel language eldoc cache
;; ---------------------------------------------------------------------------

(defvar org-eldoc--lang-cache (make-hash-table :size 40 :test 'equal)
  "Cache of major-mode `eldoc-documentation-functions' for Babel blocks.")

(defun org-eldoc--get-lang-doc-fn (lang)
  "Retrieve and cache the eldoc function for Babel LANG."
  (let ((cached (gethash lang org-eldoc--lang-cache 'empty))
        (mode-fn (org-src-get-lang-mode lang)))
    (if (eq 'empty cached)
        (when (fboundp mode-fn)
          (with-temp-buffer
            (funcall mode-fn)
            (let ((doc-fns (when (boundp 'eldoc-documentation-functions)
                             (copy-sequence eldoc-documentation-functions))))
              (puthash lang doc-fns org-eldoc--lang-cache)
              doc-fns)))
      cached)))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defun org-eldoc--src-lang ()
  "Return the language of the src block surrounding point, or nil."
  (let ((el (save-match-data (org-element-at-point))))
    (and (eq (org-element-type el) 'src-block)
         (>= (line-beginning-position)
             (org-element-property :post-affiliated el))
         (<= (line-end-position)
             (org-with-wide-buffer
              (goto-char (org-element-property :end el))
              (skip-chars-backward " \t\n")
              (line-end-position)))
         (org-element-property :language el))))

(defun org-eldoc--sep ()
  "Propertized separator for composed lines."
  (propertize " → " 'face '(:inherit shadow :height 0.8)))

;; ---------------------------------------------------------------------------
;; Layer 1: Clock (depth -100)
;; ---------------------------------------------------------------------------

(defun org-eldoc--clock (_callback &rest _args)
  "Show the active org-clock task name."
  (when (and (bound-and-true-p org-clock-current-task)
             (stringp org-clock-current-task))
    (concat
     (propertize "CLOCKING: " 'face 'org-special-keyword)
     (propertize org-clock-current-task 'face 'org-clock-overlay))))

;; ---------------------------------------------------------------------------
;; Layer 2: Babel language delegation (depth -90)
;; ---------------------------------------------------------------------------

(defun org-eldoc--babel-delegation (callback &rest _args)
  "Delegate to the src-block language's native eldoc functions.
This is LOCAL introspection only; Eglot does not run in inline Org blocks."
  (let ((lang (org-eldoc--src-lang)))
    (when lang
      (cond
       ;; Prevent infinite recursion for org-in-org blocks.
       ((string= lang "org") nil)
       ;; Emacs Lisp: use native C-level introspection.
       ((member lang '("emacs-lisp" "elisp"))
        (when (and (fboundp 'elisp-eldoc-var-docstring)
                   (fboundp 'elisp-eldoc-funcall))
          (let ((eldoc-documentation-functions
                 '(elisp-eldoc-var-docstring elisp-eldoc-funcall)))
            (run-hook-with-args-until-success
             'eldoc-documentation-functions callback))
          t))
       ;; Other languages: use cached temp-buffer scrape.
       (t
        (let ((doc-fns (org-eldoc--get-lang-doc-fn lang)))
          (when doc-fns
            (condition-case nil
                (let ((eldoc-documentation-functions doc-fns))
                  (run-hook-with-args-until-success
                   'eldoc-documentation-functions callback))
              (error nil))
            t)))))))

;; ---------------------------------------------------------------------------
;; Layer 3: Link target (depth -80)
;; ---------------------------------------------------------------------------

(defun org-eldoc--link-target (_callback &rest _args)
  "Show the resolved target when point is on an Org link."
  (let ((ctx (ignore-errors (org-element-context))))
    (when (and ctx (eq (org-element-type ctx) 'link))
      (let* ((type (org-element-property :type ctx))
             (path (org-element-property :path ctx))
             (raw  (org-element-property :raw-link ctx))
             (desc (org-element-contents ctx)))
        (concat
         (propertize
          (cond
           ((string= type "file")   (format "File: %s" (or path "")))
           ((string= type "denote") (format "Denote: %s" (or path "")))
           ((string= type "id")     (format "ID: %s" (or path "")))
           ((string-match-p "^https?$" type)
            (format "URL: %s" (or raw path "")))
           ((string= type "fuzzy")  (format "Fuzzy: %s" (or raw path "")))
           (t (or raw path "")))
          'face 'font-lock-string-face)
         (when desc
           (concat " "
                   (propertize
                    (org-element-interpret-data desc)
                    'face 'shadow))))))))

;; ---------------------------------------------------------------------------
;; Layer 4: Src block header (depth -75)
;; ---------------------------------------------------------------------------

(defun org-eldoc--src-header (_callback &rest _args)
  "Show language and header args on #+begin_src / #+end_src lines."
  (let ((case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (let* ((info (org-babel-get-src-block-info 'light))
                 (lang (nth 0 info))
                 (hdr  (nth 2 info)))
            (concat
             (propertize (or lang "no-lang") 'face 'font-lock-string-face)
             (when hdr
               (concat ": "
                       (mapconcat
                        (lambda (pair)
                          (when-let ((val (and (cdr pair)
                                               (format "%s" (cdr pair)))))
                            (unless (string-empty-p val)
                              (concat
                               (propertize (symbol-name (car pair))
                                           'face 'org-list-dt)
                               " "
                               (propertize val 'face 'org-verbatim)))))
                        hdr " "))))))))))

;; ---------------------------------------------------------------------------
;; Layer 5: Table cell (depth -70)
;; ---------------------------------------------------------------------------

(defun org-eldoc--table-cell (_callback &rest _args)
  "Show cell coordinates and formula when inside an Org table."
  (when (and (org-at-table-p)
             (not (and (fboundp 'org-at-table.el-p)
                       (org-at-table.el-p))))
    (let* ((row (org-table-current-dline))
           (col (org-table-current-column))
           (formula (ignore-errors
                      (org-table-get-stored-formulas))))
      (concat
       (propertize (format "Cell[%d,%d]" row col)
                   'face 'font-lock-type-face)
       (when formula
         (let ((col-formula (cdr (assq col formula))))
           (when col-formula
             (concat " "
                     (propertize "Formula:" 'face 'org-list-dt)
                     " "
                     (propertize col-formula 'face 'org-verbatim)))))))))

;; ---------------------------------------------------------------------------
;; Layer 6: Property drawer (depth -60)
;; ---------------------------------------------------------------------------

(defun org-eldoc--property-drawer (_callback &rest _args)
  "Show key: value when point is inside a property drawer."
  (let ((ctx (ignore-errors (org-element-context))))
    (when (and ctx (eq (org-element-type ctx) 'node-property))
      (let ((key   (org-element-property :key ctx))
            (value (org-element-property :value ctx)))
        (concat
         (propertize (format "%s" key) 'face 'org-list-dt)
         ": "
         (propertize (or value "") 'face 'org-verbatim))))))

;; ---------------------------------------------------------------------------
;; Layer 7: Heading metadata (depth -40)
;; ---------------------------------------------------------------------------

(defun org-eldoc--heading-info (_callback &rest _args)
  "Show TODO keyword, priority, and tags on a headline."
  (let ((el (org-element-at-point)))
    (when (eq (org-element-type el) 'headline)
      (let ((todo  (org-element-property :todo-keyword el))
            (pri   (org-element-property :priority el))
            (tags  (org-element-property :tags el)))
        (when (or todo pri tags)
          (mapconcat
           #'identity
           (delq nil
                 (list
                  (when todo
                    (propertize todo
                                'face (org-get-todo-face todo)))
                  (when pri
                    (propertize (format "[#%c]" pri)
                                'face (org-get-priority-face pri)))
                  (when tags
                    (propertize
                     (concat ":" (mapconcat #'identity tags ":") ":")
                     'face 'org-tag))))
           " "))))))

;; ---------------------------------------------------------------------------
;; Layer 8: Timestamps (depth -30)
;; ---------------------------------------------------------------------------

(defun org-eldoc--timestamps (_callback &rest _args)
  "Show SCHEDULED / DEADLINE / CLOSED on the current headline."
  (let ((el (org-element-at-point)))
    (when (eq (org-element-type el) 'headline)
      (let ((sched (org-element-property :scheduled el))
            (dead  (org-element-property :deadline el))
            (close (org-element-property :closed el)))
        (when (or sched dead close)
          (mapconcat
           #'identity
           (delq nil
                 (list
                  (when sched
                    (concat
                     (propertize "SCHED: " 'face 'org-special-keyword)
                     (propertize
                      (org-element-property :raw-value sched)
                      'face 'org-date)))
                  (when dead
                    (concat
                     (propertize "DEAD: " 'face 'org-special-keyword)
                     (propertize
                      (org-element-property :raw-value dead)
                      'face 'org-date)))
                  (when close
                    (concat
                     (propertize "DONE: " 'face 'org-special-keyword)
                     (propertize
                      (org-element-property :raw-value close)
                      'face 'org-done)))))
           (propertize " │ " 'face 'shadow)))))))

;; ---------------------------------------------------------------------------
;; Layer 9: Breadcrumb (depth -10, always active)
;; ---------------------------------------------------------------------------

(defun org-eldoc--breadcrumb (_callback &rest _args)
  "Show the cached imenu heading path via breadcrumb.el.
Uses breadcrumb's idle-timer cache (O(1) amortized).  Never calls
`org-get-outline-path' (O(N) per keystroke)."
  (when (fboundp 'breadcrumb-imenu-crumbs)
    (or (breadcrumb-imenu-crumbs)
        (cond
         ((bobp) (propertize "Top Level" 'face 'shadow))
         ((eobp) (propertize "Bottom Level" 'face 'shadow))
         (t (propertize "Top Level" 'face 'shadow))))))

;; ---------------------------------------------------------------------------
;; Registration
;; ---------------------------------------------------------------------------

;;;###autoload
(defun org-eldoc-load ()
  "Register all org-eldoc layers in the current Org buffer."
  (when (boundp 'eldoc-documentation-functions)
    ;; Composition strategy: collect ALL results, join with newlines.
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose)
    ;; Register layers at explicit depths (most-negative = first in hook).
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--clock -100 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--babel-delegation -90 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--link-target -80 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--src-header -75 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--table-cell -70 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--property-drawer -60 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--heading-info -40 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--timestamps -30 t)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc--breadcrumb -10 t)))

(provide 'org-eldoc)
;;; org-eldoc.el ends here
