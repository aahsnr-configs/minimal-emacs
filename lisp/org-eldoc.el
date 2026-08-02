;;; org-eldoc.el --- display org header and src block info using eldoc -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Free Software Foundation, Inc.

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 11
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: eldoc, outline, breadcrumb, org, babel, minibuffer

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides zero-overhead ElDoc support for Org mode buffers.
;; Displays breadcrumbs for headlines, header arguments for source blocks,
;; link/URL tooltips, bridges native Emacs Lisp ElDoc inside Emacs Lisp
;; source blocks, and supports legacy language-specific eldoc packages.
;; Non-Lisp/Legacy source blocks gracefully yield to prevent async LSP hangs.

;;; Code:

(require 'org)
(require 'org-element)
(require 'eldoc)
(require 'elisp-mode)
(require 'thingatpt)

(defgroup org-eldoc nil
  "ElDoc support for Org mode."
  :group 'org)

(defcustom org-eldoc-breadcrumb-separator " → "
  "Breadcrumb separator."
  :group 'org-eldoc
  :type 'string)

(defconst org-eldoc--elisp-functions
  (delq nil
        (list 'elisp-eldoc-var-docstring
              'elisp-eldoc-funcall
              (when (fboundp 'elisp-eldoc-funcall-with-docstring)
                'elisp-eldoc-funcall-with-docstring)))
  "Pre-computed list of native Emacs Lisp Eldoc functions.
Memoized at load time to prevent GC pressure during active typing.")

(defconst org-eldoc--lang-aliases
  '(("sh" . "shell") ("bash" . "shell") ("zsh" . "shell")
    ("js" . "javascript") ("ts" . "typescript")
    ("py" . "python") ("rb" . "ruby")
    ("c++" . "cpp") ("c#" . "csharp"))
  "Alist of common Babel language aliases to normalize legacy ElDoc routing.")

;; Register structural navigation commands so ElDoc triggers instantly
;; upon heading traversal, folding, and block insertion.
(eldoc-add-command 'org-self-insert-command
                   'org-cycle
                   'org-return
                   'org-next-visible-heading
                   'org-previous-visible-heading
                   'outline-up-heading
                   'org-metaleft
                   'org-metaright
                   'org-metaup
                   'org-metadown)

(defun org-eldoc-get-breadcrumb (el)
  "Return breadcrumb if EL is a headline, or nil."
  (when (eq (org-element-type el) 'headline)
    (let ((begin (org-element-property :begin el)))
      (when (and begin
                 (>= (line-end-position) begin)
                 (<= (line-beginning-position) begin))
        ;; Use WITH-SELF (t) to fetch the full path including the current headline
        ;; in a single cached AST pass, avoiding manual list appending.
        (let ((path (org-get-outline-path t)))
          (org-format-outline-path
           path
           (frame-width)
           ""
           org-eldoc-breadcrumb-separator))))))

(defun org-eldoc-get-src-header (el)
  "On src line, return lang and list of header properties for EL.
Return nil when not on src line.
Recognizes both #+begin_src and #+end_src lines via robust AST math
that safely handles completely empty source blocks."
  (when (eq (org-element-type el) 'src-block)
    (let* ((post-aff (org-element-property :post-affiliated el))
           (end (org-element-property :end el))
           (post-blank (or (org-element-property :post-blank el) 0))
           (cb (line-beginning-position))
           (ce (line-end-position))
           ;; Safely calculate line boundaries without `contents-begin` nil traps
           (begin-line-start (when post-aff
                               (save-excursion (goto-char post-aff) (line-beginning-position))))
           (begin-line-end (when post-aff
                             (save-excursion (goto-char post-aff) (line-end-position))))
           (end-line-start (save-excursion (goto-char (- end post-blank)) (line-beginning-position)))
           (end-line-end (save-excursion (goto-char (- end post-blank)) (line-end-position)))
           (on-begin-line (and begin-line-start (>= cb begin-line-start) (<= ce begin-line-end)))
           (on-end-line (and (>= cb end-line-start) (<= ce end-line-end))))
      (when (or on-begin-line on-end-line)
        (let* ((lang (or (org-element-property :language el) "no lang"))
               (lang-prop (propertize lang 'face 'font-lock-string-face))
               (info (ignore-errors (org-babel-get-src-block-info 'light el)))
               (hdr-args (nth 2 info)))
          (if (and hdr-args (listp hdr-args))
              (concat lang-prop ": "
                      (mapconcat
                       (lambda (elem)
                         (let ((val (and (cdr elem) (format "%s" (cdr elem)))))
                           (when (and val (not (string-empty-p val)))
                             (concat (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                                     " "
                                     (propertize val 'face 'org-verbatim)
                                     " "))))
                       hdr-args ""))
            lang-prop))))))

(defun org-eldoc-get-link-info ()
  "Return help-echo text or URL at point, or nil if neither exists.
Provides fallback documentation for Org links when no other ElDoc
backend yields a result."
  (let ((help-echo (get-text-property (point) 'help-echo))
        (url (thing-at-point 'url t)))
    (cond
     ((functionp help-echo)
      (funcall help-echo (selected-window) (current-buffer) (point)))
     ((stringp help-echo) help-echo)
     (url (format "LINK: %s" url))
     (t nil))))

(defun org-eldoc-get-src-lang (el)
  "Return value of lang for EL if point is strictly inside the block body.
Returns nil otherwise. Utilizes O(1) AST boundary properties."
  (when (eq (org-element-type el) 'src-block)
    (let ((cb (line-beginning-position))
          (ce (line-end-position))
          (contents-begin (org-element-property :contents-begin el))
          (contents-end (org-element-property :contents-end el)))
      ;; For empty blocks, contents-begin/end are nil, so this correctly yields nil
      ;; because point cannot be strictly inside an empty body.
      (when (and contents-begin contents-end
                 (>= cb contents-begin)
                 (<= ce contents-end))
        (org-element-property :language el)))))

(declare-function c-eldoc-print-current-symbol-info "c-eldoc" ())
(declare-function css-eldoc-function "css-eldoc" ())
(declare-function php-eldoc-function "php-eldoc" ())
(declare-function go-eldoc--documentation-function "go-eldoc" ())

(defun org-eldoc-documentation-function (&rest args)
  "Return breadcrumbs when on a headline, args for src block header-line.
Calls native Elisp documentation functions when inside an elisp src body.
Calls legacy eldoc packages for C, CSS, PHP, and Go.
Displays link/URL tooltips as a fallback.
Yields gracefully for LSP-backed languages to prevent async hangs.
Executes a single AST pass using `cached-only' to guarantee zero overhead."
  (let* ((callback (car args))
         (el (org-element-at-point nil 'cached-only)))
    (or
     ;; 1. Headline breadcrumbs
     (org-eldoc-get-breadcrumb el)

     ;; 2. Source block header line (#+begin_src / #+end_src)
     (org-eldoc-get-src-header el)

     ;; 3. Link / help-echo fallback (Doom Emacs parity)
     (org-eldoc-get-link-info)

     ;; 4. Inside source block body
     (let* ((raw-lang (org-eldoc-get-src-lang el))
            (lang (or (cdr (assoc-string raw-lang org-eldoc--lang-aliases t))
                      raw-lang)))
       (when lang
         (cond
          ;; Prevent inf-loop for Org src blocks
          ((string= lang "org") nil)

          ;; Bridge native Emacs Lisp / Lisp ElDoc (Async Callback Protocol)
          ((or (string= lang "emacs-lisp")
               (string= lang "elisp")
               (string= lang "lisp"))
           (when callback
             (let ((eldoc-documentation-functions org-eldoc--elisp-functions))
               (run-hook-with-args-until-success
                'eldoc-documentation-functions callback))
             t))

          ;; Legacy C/C++ ElDoc
          ((or (string= lang "c") (string= lang "C")
               (string= lang "cpp") (string= lang "C++"))
           (when (require 'c-eldoc nil t)
             (c-eldoc-print-current-symbol-info)))

          ;; Legacy CSS ElDoc
          ((string= lang "css")
           (when (require 'css-eldoc nil t)
             (css-eldoc-function)))

          ;; Legacy PHP ElDoc
          ((string= lang "php")
           (when (require 'php-eldoc nil t)
             (php-eldoc-function)))

          ;; Legacy Go ElDoc
          ((or (string= lang "go") (string= lang "golang"))
           (when (require 'go-eldoc nil t)
             (go-eldoc--documentation-function)))

          ;; Yield for all other languages (Python, Shell, Rust, Plantuml, etc.)
          (t nil)))))))

;;;###autoload
(defun org-eldoc-load ()
  "Set up org-eldoc documentation function."
  (interactive)
  (add-hook 'eldoc-documentation-functions
            #'org-eldoc-documentation-function nil t))

(add-hook 'org-mode-hook #'org-eldoc-load)

(provide 'org-eldoc)
;;; org-eldoc.el ends here
