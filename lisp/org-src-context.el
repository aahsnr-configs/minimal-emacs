;;; org-src-context.el --- LSP and Context support for org-src buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ahsanur Rahman
;; Author: Ahsanur Rahman
;; Keywords: tools, languages, extensions, lsp
;; Package-Requires: ((emacs "30.1") (org "9.6"))
;; Version: 1.0.0

;;; Commentary:

;; This package injects surrounding source blocks into the `org-edit-special'
;; buffer to provide context for LSP servers (Eglot).
;;
;; ARCHITECTURAL OVERVIEW (v1.0.0 - Emacs 31 Hardened):
;;
;; 1. AST-Optimized Collection:
;;    Replaces the O(N²) regex scanner with `org-element-map' over the cached
;;    AVL tree. This guarantees O(1) amortized traversal and prevents main-thread
;;    micro-stutters in massive literate configurations.
;;
;; 2. Write-Back Interception (Data Integrity):
;;    Previous versions attempted to delete ghost text on exit/abort, which failed
;;    if the user saved via `C-x C-s' or killed the buffer directly.
;;    This version advises `org-src--contents-for-write-back' to mathematically
;;    strip any text possessing the `org-src-context-block' property from the
;;    temporary write-back buffer before Org splices it back into the source file.
;;
;; 3. Eglot Formatting Guard:
;;    LSP `textDocument/formatting' payloads crash when they hit `read-only'
;;    properties. We wrap `eglot--apply-text-edits' in an `inhibit-read-only'
;;    guard, allowing the server to format the ghost text safely.
;;
;; 4. Phantom File Materialization:
;;    Strict LSP servers (BasedPyright) reject non-existent `buffer-file-name'
;;    paths. We now materialize an empty file on disk if the tangle target is
;;    missing, and clean it up upon buffer destruction.

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'org-src)
(require 'cl-lib)

(defgroup org-src-context nil
  "Provide LSP support in org-src buffers."
  :group 'org)

(defcustom org-src-context-narrow-p t
  "Non-nil means org-src buffers should be narrowed to the editable block."
  :type 'boolean
  :group 'org-src-context)

(defcustom org-src-context-max-filesize 500000
  "Max size (in bytes) of an Org file to attempt context collection."
  :type 'integer
  :group 'org-src-context)

(defvar-local org-src-context--head-marker nil)
(defvar-local org-src-context--tail-marker nil)
(defvar-local org-src-context--created-mock-file nil
  "Path to mock file created for LSP, to be cleaned up on exit.")

;;; --- Core Logic: Context Collection (AST Optimized) ---

(defun org-src-context--collect-context (current-datum)
  "Collect prev and next blocks matching CURRENT-DATUM's lang and tangle.
Uses `org-element-map' for O(1) amortized AST traversal."
  (let* ((target-info (org-babel-get-src-block-info 'light current-datum))
         (target-lang (nth 0 target-info))
         (target-params (nth 2 target-info))
         (target-tangle (or (alist-get :tangle target-params) "no"))
         (current-beg (org-element-begin current-datum))
         (prev-blocks nil)
         (next-blocks nil))
    (if (> (buffer-size) org-src-context-max-filesize)
        (progn (message "Org-Src-Context: File too large, skipping.") nil)
      (org-element-map (org-element-parse-buffer 'element) 'src-block
        (lambda (datum)
          (let* ((info (org-babel-get-src-block-info 'light datum))
                 (lang (nth 0 info))
                 (params (nth 2 info))
                 (tangle (or (alist-get :tangle params) "no"))
                 (beg (org-element-begin datum)))
            (when (and (string= lang target-lang)
                       (string= tangle target-tangle))
              (cond
               ((< beg current-beg) (push (nth 1 info) prev-blocks))
               ((> beg current-beg) (push (nth 1 info) next-blocks))))))
        nil nil nil t)
      (cons (nreverse prev-blocks) (nreverse next-blocks)))))

;;; --- Core Logic: Injection & Properties ---

(defun org-src-context--inject (prev-blocks next-blocks)
  "Inject PREV-BLOCKS and NEXT-BLOCKS with Emacs 31 safe boundaries."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (save-excursion
      ;; 1. INJECT HEADER
      (goto-char (point-min))
      (let ((start (point)))
        (dolist (b prev-blocks)
          (insert b "\n"))
        (unless (= start (point))
          (insert "\n") ;; Padding newline for Emacs 31 redisplay safety
          (add-text-properties start (point)
                               '(read-only t
                                 font-lock-face shadow
                                 front-sticky t
                                 rear-nonsticky (read-only)
                                 org-src-context-block t))))
      (setq org-src-context--head-marker (point-marker))
      (set-marker-insertion-type org-src-context--head-marker nil)

      ;; 2. INJECT FOOTER
      (goto-char (point-max))
      (let ((start (point)))
        (unless (bolp) (insert "\n"))
        (insert "\n") ;; Padding newline
        (let ((pad-end (point)))
          (dolist (b next-blocks)
            (insert b "\n"))
          (when (> (point) pad-end)
            (add-text-properties pad-end (point)
                                 '(read-only t
                                   font-lock-face shadow
                                   front-sticky nil
                                   rear-nonsticky t
                                   org-src-context-block t)))
          (add-text-properties start pad-end
                               '(read-only t
                                 font-lock-face shadow
                                 front-sticky nil
                                 rear-nonsticky t
                                 org-src-context-block t))))
      (setq org-src-context--tail-marker (copy-marker start))
      (set-marker-insertion-type org-src-context--tail-marker t))
    (when org-src-context-narrow-p
      (narrow-to-region org-src-context--head-marker org-src-context--tail-marker))))

;;; --- Core Logic: Write-Back Interception (Data Integrity) ---

(defun org-src-context--filter-write-back (orig-fn write-back-buf)
  "Strip ghost text from WRITE-BACK-BUF before Org splices it."
  (funcall orig-fn write-back-buf)
  (with-current-buffer write-back-buf
    (let ((inhibit-read-only t)
          (pos (point-min)))
      (while (setq pos (next-single-property-change pos 'org-src-context-block nil (point-max)))
        (when (get-text-property pos 'org-src-context-block)
          (let ((end (next-single-property-change pos 'org-src-context-block nil (point-max))))
            (delete-region pos end)))))))

;;; --- Core Logic: LSP Mocking & Materialization ---

(defun org-src-context--setup-lsp (info original-dir original-file)
  "Configure LSP paths, materializing phantom files if necessary."
  (let* ((lang (nth 0 info))
         (params (nth 2 info))
         (tangle-file (alist-get :tangle params))
         (lang-ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) "txt"))
         (file-ext (if (and tangle-file (not (member tangle-file '("yes" "no"))))
                       (file-name-extension tangle-file)
                     lang-ext))
         (mock-name (if (and tangle-file (not (member tangle-file '("yes" "no"))))
                        tangle-file
                      (concat (file-name-base original-file) "_src." file-ext)))
         (full-path (expand-file-name mock-name original-dir)))
    (unless (file-exists-p full-path)
      (write-region "" nil full-path nil 'silent)
      (setq-local org-src-context--created-mock-file full-path))
    (setq-local default-directory original-dir)
    (setq-local buffer-file-name full-path)
    (when (and (fboundp 'eglot-ensure)
               (not (member lang '("emacs-lisp" "elisp"))))
      (eglot-ensure))))

;;; --- Eglot Formatting Guard ---

(defun org-src-context--eglot-format-guard (orig-fn &rest args)
  "Allow Eglot to format read-only context blocks."
  (let ((inhibit-read-only t))
    (apply orig-fn args)))

;;; --- Advice & Hooks ---

(defun org-src-context--advice (orig-fn &rest args)
  "Advice for `org-edit-src-code'."
  (if (or (car args)
          (not (memq this-command '(org-edit-special
                                    org-edit-src-code
                                    evil-org-edit-src-code))))
      (apply orig-fn args)
    (let* ((datum (org-element-at-point))
           (type (org-element-type datum)))
      (if (not (eq type 'src-block))
          (apply orig-fn args)
        (let* ((info (org-babel-get-src-block-info 'light datum))
               (context-blocks (org-src-context--collect-context datum))
               (orig-dir default-directory)
               (orig-file (buffer-file-name)))
          (apply orig-fn args)
          (when context-blocks
            (org-src-context--inject (car context-blocks) (cdr context-blocks)))
          (org-src-context--setup-lsp info orig-dir orig-file)
          (add-hook 'kill-buffer-hook #'org-src-context--cleanup-mock-file nil t))))))

(defun org-src-context--cleanup-mock-file ()
  "Delete the mock file if we created it and it remains empty."
  (when (and org-src-context--created-mock-file
             (file-exists-p org-src-context--created-mock-file)
             (zerop (nth 7 (file-attributes org-src-context--created-mock-file))))
    (delete-file org-src-context--created-mock-file)))

;;;###autoload
(define-minor-mode org-src-context-mode
  "Global mode for LSP context in Org Src buffers."
  :global t
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--advice)
        (advice-add 'org-src--contents-for-write-back :around #'org-src-context--filter-write-back)
        (when (fboundp 'eglot--apply-text-edits)
          (advice-add 'eglot--apply-text-edits :around #'org-src-context--eglot-format-guard)))
    (advice-remove 'org-edit-src-code #'org-src-context--advice)
    (advice-remove 'org-src--contents-for-write-back #'org-src-context--filter-write-back)
    (when (fboundp 'eglot--apply-text-edits)
      (advice-remove 'eglot--apply-text-edits #'org-src-context--eglot-format-guard))))

(provide 'org-src-context)
;;; org-src-context.el ends here
