;;; org-eldoc.el --- High-performance Org ElDoc integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom, optimized ElDoc router for Org mode. Excises heavy AST parsing
;; and breadcrumb generation (delegated to `breadcrumb.el`), focusing strictly
;; on high-value, low-latency metadata: src-block headers, in-block language
;; delegation, link targets, and property drawers.

;;; Code:

(require 'org)
(require 'ob-core)
(require 'eldoc)
(require 'org-element)

(defvar org-eldoc-local-functions-cache (make-hash-table :size 40 :test 'equal)
  "Cache of major-mode's `eldoc-documentation-functions' for Babel blocks.")

(defun org-eldoc--get-mode-local-documentation (lang)
  "Retrieve and cache the ElDoc documentation function for LANG."
  (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
        (mode-func (org-src-get-lang-mode lang)))
    (if (eq 'empty cached-func)
        (when (fboundp mode-func)
          (with-temp-buffer
            (funcall mode-func)
            (let ((doc-func (when (boundp 'eldoc-documentation-functions)
                              (let ((doc-funs eldoc-documentation-functions))
                                (lambda (callback)
                                  (let ((eldoc-documentation-functions doc-funs))
                                    (run-hook-with-args-until-success
                                     'eldoc-documentation-functions callback)))))))
              (puthash lang doc-func org-eldoc-local-functions-cache)
              doc-func)))
      cached-func)))

(defun org-eldoc--src-header ()
  "Return language and header arguments when on a src block boundary."
  (let ((case-fold-search t) info lang hdr-args)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (setq info (org-babel-get-src-block-info 'light)
                lang (propertize (or (nth 0 info) "no lang") 'face 'font-lock-string-face)
                hdr-args (nth 2 info))
          (concat lang ": "
                  (mapconcat
                   (lambda (elem)
                     (when-let ((val (and (cdr elem) (format "%s" (cdr elem)))))
                       (unless (string-empty-p val)
                         (concat (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                                 " "
                                 (propertize val 'face 'org-verbatim)
                                 " "))))
                   hdr-args " ")))))))

(defun org-eldoc--src-lang ()
  "Return the language of the current src block if point is inside the body."
  (let ((element (save-match-data (org-element-at-point))))
    (and (eq (org-element-type element) 'src-block)
         (>= (line-beginning-position)
             (org-element-property :post-affiliated element))
         (<= (line-end-position)
             (org-with-wide-buffer
              (goto-char (org-element-property :end element))
              (skip-chars-backward " \t\n")
              (line-end-position)))
         (org-element-property :language element))))

(defun org-eldoc--link-target ()
  "Return the resolved target description when point is on an Org link."
  (let ((context (ignore-errors (org-element-context))))
    (when (and context (eq (org-element-type context) 'link))
      (let* ((type (org-element-property :type context))
             (path (org-element-property :path context))
             (raw (org-element-property :raw-link context)))
        (cond
         ((string= type "file")
          (propertize (format "File: %s" (or path "")) 'face 'font-lock-string-face))
         ((string= type "denote")
          (propertize (format "Denote ID: %s" (or path "")) 'face 'font-lock-string-face))
         ((string= type "id")
          (propertize (format "Org ID: %s" (or path "")) 'face 'font-lock-string-face))
         ((string-match-p "^https?$" type)
          (propertize (format "URL: %s" (or raw path "")) 'face 'font-lock-string-face))
         ((string= type "fuzzy")
          (propertize (format "Fuzzy: %s" (or raw path "")) 'face 'font-lock-string-face))
         (t (propertize (or raw path "") 'face 'font-lock-string-face)))))))

(defun org-eldoc--property-drawer ()
  "Return the key-value pair when point is inside a property drawer."
  (let ((context (ignore-errors (org-element-context))))
    (when (and context (memq (org-element-type context) '(node-property)))
      (let ((key (org-element-property :key context))
            (value (org-element-property :value context)))
        (propertize (format "%s: %s" key (or value "")) 'face 'org-list-dt)))))

;;;###autoload
(defun org-eldoc-documentation-function (callback &rest _args)
  "High-performance ElDoc router for Org mode.
Delegates to CALLBACK according to the Emacs 28+ async protocol."
  ;; Massive file guard to prevent main-thread freezing.
  ;; Guarded by `fboundp` to ensure clean byte-compilation in isolation.
  (unless (and (fboundp 'too-long-file-p) (too-long-file-p))
    (or
     ;; 1. Property Drawers (O(1) context check)
     (org-eldoc--property-drawer)
     ;; 2. Link Targets (O(1) context check)
     (org-eldoc--link-target)
     ;; 3. Src Block Boundaries
     (org-eldoc--src-header)
     ;; 4. Inside Src Block (Language Delegation)
     (let ((lang (org-eldoc--src-lang)))
       (when lang
         (cond
          ((string= lang "org") nil) ; Prevent inf-loop
          ((or (string= lang "emacs-lisp") (string= lang "elisp"))
           (when (and (fboundp 'elisp-eldoc-var-docstring)
                      (fboundp 'elisp-eldoc-funcall))
             (let ((eldoc-documentation-functions
                    '(elisp-eldoc-var-docstring elisp-eldoc-funcall)))
               (run-hook-with-args-until-success 'eldoc-documentation-functions callback))))
          (t
           (let ((doc-fun (org-eldoc--get-mode-local-documentation lang)))
             (when (functionp doc-fun)
               ;; Crash guard: prevent foreign backend errors from silencing ElDoc.
               (condition-case nil
                   (if (functionp callback)
                       (funcall doc-fun callback)
                     (funcall doc-fun))
                 (error nil)))))))))))

;;;###autoload
(defun org-eldoc-load ()
  "Register `org-eldoc-documentation-function' in the buffer."
  (when (boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions
              #'org-eldoc-documentation-function nil t)))

(provide 'org-eldoc)
;;; org-eldoc.el ends here
