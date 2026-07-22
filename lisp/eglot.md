```elisp
;; ARCHITECTURAL ROLE:
;; eglot sits between the language server process (e.g. basedpyright, clangd)  and Emacs' native subsystems:
;; - completion-at-point-functions  → corfu / cape  (IntelliSense)
;; - flymake                        → diagnostics    (squiggles, Problems panel)
;; - xref                           → navigation     (go-to-definition, references)
;; - eldoc                          → hover / signature help
;; - imenu                          → document symbols / outline
;; - project.el                     → workspace root detection
;;
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :custom

  (eglot-stay-out-of '("company" "yasnippet"))
  (eglot-code-action-indications '(margin))
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-autoshutdown t)

  (eglot-confirm-server-edits t)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))

  :config
  (setq eglot-code-action-indicator "💡")

  (setq eglot-max-file-watches 10000)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (eglot-managed-p)
                (eglot-semantic-tokens-mode 1)
                (eglot-inlay-hints-mode 1))))

  (defun ar/eglot-rename-file (new-name)
    "Rename current file to NEW-NAME and notify the LSP server.
Triggers `workspace/willRenameFiles' to update imports project-wide,
performs the OS-level rename, then sends `workspace/didRenameFiles'."
    (interactive
     (list (read-file-name "Rename to: "
                           (file-name-directory (buffer-file-name))
                           (buffer-file-name))))
    (let* ((old-name (buffer-file-name))
           (old-uri (eglot-path-to-uri old-name))
           (new-uri (eglot-path-to-uri new-name))
           (server (eglot-current-server)))
      (unless server
        (user-error "No active Eglot server in this buffer"))
      (let ((edits (eglot--request
                    server
                    :workspace/willRenameFiles
                    `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))))
        (when edits
          (eglot--apply-workspace-edit edits 'ar/eglot-rename-file)))
      (rename-file old-name new-name 1)
      (set-visited-file-name new-name nil t)
      (jsonrpc-notify server :workspace/didRenameFiles
                      `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))
      (message "Renamed %s → %s and updated LSP references"
               (file-name-nondirectory old-name)
               (file-name-nondirectory new-name))))

  (defun ar/eglot-moniker-at-point ()
    "Display the LSP moniker for the symbol at point in the echo area."
    (interactive)
    (let ((server (eglot-current-server)))
      (if (not server)
          (user-error "No active Eglot server")
        ;; eglot--async-request sends the request and returns immediately.
        ;; The :success-fn lambda runs when the server responds.
        (eglot--async-request
         server
         :textDocument/moniker
         ;; Construct standard LSP position parameters for the current point.
         (eglot--TextDocumentPositionParams)
         :success-fn
         (lambda (result)
           ;; result is a vector of Moniker plists, or nil/empty.
           (if (and result (cl-plusp (length result)))
               (let* ((primary (aref result 0))
                      (scheme (plist-get primary :scheme))
                      (identifier (plist-get primary :identifier))
                      (kind (plist-get primary :kind)))
                 (message "Moniker [%s]: %s (Kind: %s)"
                          scheme identifier kind))
             (message "No moniker found for symbol at point.")))))))

  (defun ar/eglot-copy-moniker ()
    "Copy the primary moniker identifier at point to the kill-ring."
    (interactive)
    (let ((server (eglot-current-server)))
      (if (not server)
          (user-error "No active Eglot server")
        (eglot--async-request
         server
         :textDocument/moniker
         (eglot--TextDocumentPositionParams)
         :success-fn
         (lambda (result)
           (if (and result (cl-plusp (length result)))
               (let ((identifier (plist-get (aref result 0) :identifier)))
                 ;; kill-new adds the string to the kill ring (clipboard).
                 (kill-new identifier)
                 (message "Copied moniker: %s" identifier))
             (message "No moniker found to copy."))))))))

```
