(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  ;; ==========================================
  ;; 1. HOOK TIMING (The LSP Race Condition Fix)
  ;; ==========================================
  ;; By default, `envrc` hooks into `after-change-major-mode-hook`. This runs
  ;; AFTER major mode hooks (like `python-mode-hook`). If `lsp-deferred` is
  ;; in `python-mode-hook`, the LSP server starts before `envrc` applies the
  ;; project's `VIRTUAL_ENV` or `PATH`.
  ;; It is moved to `change-major-mode-after-body-hook`, which runs BEFORE
  ;; major mode hooks, guaranteeing the environment is ready for LSP.
  (add-hook 'envrc-global-mode-hook
            (lambda ()
              ;; Emacs 30+ / newer envrc versions renamed this internal function.
              ;; The `fboundp` guard ensures compatibility across versions.
              (let ((fn (if (fboundp #'envrc-global-mode-enable-in-buffers)
                            #'envrc-global-mode-enable-in-buffers
                          #'envrc-global-mode-enable-in-buffer)))
                (if (not envrc-global-mode)
                    ;; Teardown safety: remove from BOTH hooks when disabled
                    ;; to prevent orphaned hook references.
                    (progn
                      (remove-hook 'change-major-mode-after-body-hook fn)
                      (remove-hook 'after-change-major-mode-hook fn))
                  (remove-hook 'after-change-major-mode-hook fn)
                  (add-hook 'change-major-mode-after-body-hook fn 100)))))

  ;; ==========================================
  ;; 2. ORG BABEL ENVIRONMENT PROPAGATION (Jupyter Support)
  ;; ==========================================
  ;; Upstream `envrc` advises `org-babel-eval`, but Jupyter kernel initialization
  ;; happens BEFORE that low-level shell spawn. Advising the higher-level
  ;; `org-babel-execute-src-block` ensures Jupyter kernels and persistent
  ;; sessions inherit the buffer-local `envrc` environment (e.g. VIRTUAL_ENV).
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)

  ;; ==========================================
  ;; 3. GRACEFUL FAILURE (Missing Executable Guard)
  ;; ==========================================
  ;; Prevents `envrc-global-mode` from spamming errors on every file open
  ;; if the `direnv` binary is not installed on the system (or over TRAMP).
  (define-advice envrc-global-mode (:before-while (&rest _) fail-gracefully)
    (or (executable-find envrc-direnv-executable)
        (ignore (message "Failed to locate direnv executable; aborting envrc-global-mode"))))

  ;; ==========================================
  ;; 4. DEBOUNCE INTERNAL BUFFERS
  ;; ==========================================
  ;; Prevents envrc from triggering inside its own *envrc* log buffers,
  ;; avoiding redundant processing or feedback loops.
  (define-advice envrc--update (:before-while (&rest _) debounce)
    (not (string-prefix-p "*envrc" (buffer-name))))

  ;; ==========================================
  ;; 5. LSP SERVER RESTART ON ENVRC RELOAD
  ;; ==========================================
  ;; When `.envrc` is updated, the environment changes, but the running LSP
  ;; server is still using the old environment. This advice catches the
  ;; update and restarts associated `lsp-mode` workspaces.
  (with-eval-after-load 'lsp-mode
    (when (fboundp 'envrc--mode-buffers)
      ;; Helper to restart LSP servers for a specific environment directory
      (defun ar/envrc--restart-lsp-workspaces (env-dir)
        "Restart `lsp-mode' workspaces associated with ENV-DIR."
        (let ((lsp-servers nil))
          (dolist (buf (envrc--mode-buffers))
            (with-current-buffer buf
              (let ((buf-env-dir (ignore-errors (envrc--find-env-dir))))
                (when (and buf-env-dir (string= buf-env-dir env-dir))
                  (when (bound-and-true-p lsp-mode)
                    (dolist (ws (lsp-workspaces))
                      (cl-pushnew ws lsp-servers :test #'equal)))))))
          (when lsp-servers
            (mapc #'lsp-workspace-restart lsp-servers)
            (message "Restarted %d LSP servers associated with direnv"
                     (length lsp-servers)))))

      ;; Standard reload vector (e.g., M-x envrc-reload)
      (define-advice envrc--update-env (:after (env-dir) restart-lsp)
        (ar/envrc--restart-lsp-workspaces env-dir))

      ;; Bulk reload vector (e.g., M-x envrc-reload-all)
      ;; Bypasses `envrc--update-env`, so it is caught here to restart
      ;; all unique LSP workspaces across all active envrc buffers.
      (define-advice envrc-reload-all (:after () restart-lsp-all)
        (let ((lsp-servers nil))
          (dolist (buf (envrc--mode-buffers))
            (with-current-buffer buf
              (when (bound-and-true-p lsp-mode)
                (dolist (ws (lsp-workspaces))
                  (cl-pushnew ws lsp-servers :test #'equal)))))
          (when lsp-servers
            (mapc #'lsp-workspace-restart lsp-servers)
            (message "Restarted %d LSP servers after direnv bulk reload"
                     (length lsp-servers))))))))
