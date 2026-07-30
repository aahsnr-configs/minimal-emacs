;;; org-src-context.el --- LSP and Context support for org-src buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ahsanur Rahman
;; Author: Ahsanur Rahman
;; Keywords: tools, languages, extensions, lsp
;; Package-Requires: ((emacs "30.1") (org "9.6"))
;; Version: 1.2.0

;;; Commentary:
;; This package injects surrounding source blocks into the `org-edit-special'
;; buffer to provide context for LSP servers (Eglot).
;;
;; ARCHITECTURAL OVERVIEW (v1.2.0 - Emacs 31 Write-Back Safe):
;;
;; 1. AST-Optimized Collection:
;;    Uses `org-element-map' over the cached AVL tree for O(1) amortized
;;    traversal. Prevents main-thread micro-stutters in massive literate configs.
;;
;; 2. Marker-Based Cleanup BEFORE Write-Back (The Exit Freeze Fix):
;;    `org-src--contents-for-write-back' calls `org-with-wide-buffer' which
;;    WIDENS the buffer, defeating narrowing. Then `org-no-properties' strips
;;    ALL text properties, making property-based filtering impossible.
;;    Solution: advise `org-edit-src-exit' and `org-edit-src-abort' with
;;    `:before' to widen and delete ghost text using MARKERS before Org's
;;    write-back pipeline ever runs. This is mathematically bulletproof.
;;
;; 3. Eglot Force-Shutdown (The Hang Fix):
;;    Before buffer mutation, we force-kill the Eglot JSON-RPC connection
;;    via `jsonrpc-shutdown' with the FORCE flag. This prevents the
;;    synchronous `shutdown' handshake from hanging when the mock file
;;    is deleted concurrently.
;;
;; 4. Eglot Formatting Guard:
;;    LSP `textDocument/formatting' payloads crash on `read-only' properties.
;;    We wrap `eglot--apply-text-edits' in an `inhibit-read-only' guard.
;;
;; 5. Phantom File Materialization:
;;    Strict LSP servers (BasedPyright) reject non-existent paths.
;;    We materialize an empty file on disk and clean it up on buffer kill.

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
          (insert "\n")
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
        (insert "\n")
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

;;; --- Core Logic: Marker-Based Cleanup (The Exit Freeze Fix) ---

(defun org-src-context--cleanup-before-exit (&rest _)
  "Delete ghost text and force-shutdown Eglot BEFORE Org writes back.

This runs as a `:before' advice on `org-edit-src-exit' and
`org-edit-src-abort'. It MUST execute before Org calls
`org-src--contents-for-write-back', because that function uses
`org-with-wide-buffer' (which widens the buffer, defeating narrowing)
and `org-no-properties' (which strips all text properties, making
property-based filtering impossible).

By deleting the ghost text via markers BEFORE the write-back pipeline
runs, we guarantee the buffer contains ONLY the user's code when Org
extracts it. This eliminates the O(n²) diff freeze in
`org-replace-buffer-contents'."
  (when (and org-src-context--head-marker
             (marker-buffer org-src-context--head-marker))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      ;; Force-shutdown Eglot BEFORE buffer mutation.
      ;; The FORCE flag (t) sends SIGKILL to the process, preventing
      ;; the synchronous `shutdown' handshake from hanging when the
      ;; mock file is deleted concurrently.
      (when (and (fboundp 'eglot-managed-p)
                 (eglot-managed-p))
        (ignore-errors
          (let ((server (eglot-current-server)))
            (when server
              (jsonrpc-shutdown server t)))))
      ;; Widen and delete ghost text using markers.
      (widen)
      (when (and org-src-context--tail-marker
                 (marker-buffer org-src-context--tail-marker))
        (delete-region org-src-context--tail-marker (point-max))
        (set-marker org-src-context--tail-marker nil))
      (delete-region (point-min) org-src-context--head-marker)
      (set-marker org-src-context--head-marker nil))))

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

;;; --- Mock File Cleanup ---

(defun org-src-context--cleanup-mock-file ()
  "Delete the mock file if we created it and it remains empty."
  (when (and org-src-context--created-mock-file
             (file-exists-p org-src-context--created-mock-file)
             (zerop (nth 7 (file-attributes org-src-context--created-mock-file))))
    (delete-file org-src-context--created-mock-file)))

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

;;;###autoload
(define-minor-mode org-src-context-mode
  "Global mode for LSP context in Org Src buffers."
  :global t
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--advice)
        ;; Marker-based cleanup BEFORE write-back (replaces the ineffective
        ;; `org-src--contents-for-write-back' property-stripping advice).
        (advice-add 'org-edit-src-exit :before #'org-src-context--cleanup-before-exit)
        (advice-add 'org-edit-src-abort :before #'org-src-context--cleanup-before-exit)
        (when (fboundp 'eglot--apply-text-edits)
          (advice-add 'eglot--apply-text-edits :around #'org-src-context--eglot-format-guard)))
    (advice-remove 'org-edit-src-code #'org-src-context--advice)
    (advice-remove 'org-edit-src-exit #'org-src-context--cleanup-before-exit)
    (advice-remove 'org-edit-src-abort #'org-src-context--cleanup-before-exit)
    (when (fboundp 'eglot--apply-text-edits)
      (advice-remove 'eglot--apply-text-edits #'org-src-context--eglot-format-guard))))

(provide 'org-src-context)
;;; org-src-context.el ends here
