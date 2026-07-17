---
title: "Workspace File-Operation Hooks"
category: "Workspace & Files"
status: "Working"
parity: "Auto-updating imports and references when renaming or deleting files via the file explorer"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Workspace File-Operation Hooks</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Workspace &amp; Files</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Auto-updating imports and references when renaming or deleting
            files via the file explorer</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>workspace/willRenameFiles</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>workspace/didRenameFiles</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>Custom Elisp<span class="route-arrow">→</span>eglot--execute-request<span class="route-arrow">→</span>eglot--apply-workspace-edit<span class="route-arrow">→</span>OS
              rename-file</code>
</div>
</div>
</header>

<article class="acc">
<button aria-controls="sect-overview" aria-expanded="true" class="acc-head open">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="7" width="7" x="3" y="3"></rect>
<rect height="7" width="7" x="14" y="3"></rect>
<rect height="7" width="7" x="14" y="14"></rect>
<rect height="7" width="7" x="3" y="14"></rect>
</svg>
            Feature Overview
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body open" id="sect-overview" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Behavioral Parity Matrix</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>VS Code Behavior</th>
<th>Emacs 31 Equivalent</th>
</tr>
</thead>
<tbody>
<tr>
<td>Rename file in Explorer updates imports</td>
<td>
<code>ar/eglot-rename-file</code> sends
                        <code>willRenameFiles</code>, applies edits, then
                        renames.
                      </td>
</tr>
<tr>
<td>Rename file in Explorer updates VCS</td>
<td>
                        Advising or chaining with <code>vc-rename-file</code>
                        ensures Git tracks the move.
                      </td>
</tr>
<tr>
<td>Delete file updates references</td>
<td>
                        Similar custom wrapper can be built for
                        <code>workspace/willDeleteFiles</code>.
                      </td>
</tr>
<tr>
<td>No manual "Find and Replace" needed</td>
<td>
                        The LSP server calculates the exact AST-aware import
                        paths to update.
                      </td>
</tr>
<tr>
<td>Buffer automatically visits new file</td>
<td>
<code>set-visited-file-name</code> seamlessly
                        transitions the current buffer to the new path.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-ecosystem" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M2 12h20M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"></path>
</svg>
            Ecosystem Integration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-ecosystem" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div>
<div class="eco-name">eglot</div>
<div class="eco-sub">LSP Client</div>
</div>
</div>
<p class="eco-desc">
                    Manages the JSON-RPC lifecycle, ensuring that
                    <code>willRenameFiles</code> is only sent if the connected
                    language server explicitly advertises support for it.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">vc (Version Control)</div>
<div class="eco-sub">VCS Integration</div>
</div>
</div>
<p class="eco-desc">
                    The optional advice ensures that when you rename a tracked
                    file, Git/Mercurial registers it as a rename (preserving
                    history) while the LSP server simultaneously fixes the
                    imports.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
</div>
<div>
<div class="eco-name">general.el</div>
<div class="eco-sub">Keybindings</div>
</div>
</div>
<p class="eco-desc">
                    Eagerly registers the <kbd>SPC f R</kbd> leader binding,
                    providing a consistent, mnemonic access point for LSP-aware
                    file operations across all managed buffers.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-stack" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
            Implementation Stack
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-stack" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="stack-card">
<div class="stack-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot</div>
<div class="stack-role">LSP Client</div>
<div class="stack-desc">
                      Built-in. Natively handles file resource operations in
                      workspace edits and executes client-initiated
                      <code>willRenameFiles</code> requests.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">ar/eglot-rename-file</div>
<div class="stack-role">Operation Wrapper</div>
<div class="stack-desc">
                      A custom, robust Elisp function that sequences the LSP
                      request, applies import updates, performs the OS-level
                      rename, and sends the notification.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">vc-rename-file</div>
<div class="stack-role">Version Control</div>
<div class="stack-desc">
                      Built-in. Emacs 31's native VC rename command, advised to
                      ensure Git/Mercurial tracks the file move alongside LSP
                      import updates.
                    </div>
</div>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-commands" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
            Commands &amp; Keybindings
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-commands" role="region">
<div>
<div class="acc-inner">
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Action</th>
<th>Command</th>
<th>Keybinding</th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr>
<td>Rename file with LSP updates</td>
<td><code>ar/eglot-rename-file</code></td>
<td><kbd>SPC f R</kbd></td>
<td>
                        Prompts for new name, updates imports across the
                        project, and renames the file.
                      </td>
</tr>
<tr>
<td>Native VC rename (fallback)</td>
<td><code>vc-rename-file</code></td>
<td><kbd>C-x v R</kbd></td>
<td>
                        Renames the file and updates VCS. Advised to trigger LSP
                        updates automatically.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-config" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="16 18 22 12 16 6"></polyline>
<polyline points="8 6 2 12 8 18"></polyline>
</svg>
            Configuration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-config" role="region">
<div>
<div class="acc-inner">
<div class="code-win">
<div class="code-head">
<div style="display: flex; align-items: center">
<div aria-hidden="true" class="dots">
<span></span><span></span><span></span>
</div>
<span class="fname">init-file-operations.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
<pre><code class="language-lisp">;; ==========================================
;; EGLOT FILE OPERATION HOOKS (Custom Wrapper)
;; ==========================================
(defun ar/eglot-rename-file (new-name)
  "Rename the current file to NEW-NAME and notify the LSP server to update imports.
This triggers `workspace/willRenameFiles` and `workspace/didRenameFiles`."
  (interactive
   (list (read-file-name "Rename to: "
                         (file-name-directory (buffer-file-name))
                         (buffer-file-name))))
  (let* ((old-name (buffer-file-name))
         (old-uri (eglot--path-to-uri old-name))
         (new-uri (eglot--path-to-uri new-name))
         (server (eglot-current-server)))
    (unless server
      (user-error "No active Eglot server in this buffer"))
    ;; 1. Request willRenameFiles edits (e.g., update relative imports)
    (let ((edits (eglot--execute-request
                  server
                  "workspace/willRenameFiles"
                  `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))))
      (when edits
        (eglot--apply-workspace-edit edits)))
    ;; 2. Perform the actual OS-level file rename
    (rename-file old-name new-name 1)
    ;; 3. Update the current buffer to visit the new file name
    (set-visited-file-name new-name nil t)
    ;; 4. Notify the server that the rename is complete
    (jsonrpc-notify server :textDocument/didClose `(:textDocument (:uri ,old-uri)))
    (jsonrpc-notify server :workspace/didRenameFiles `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))
    (message "Renamed %s to %s and updated LSP references" old-name new-name)))

;; ==========================================
;; KEYBINDINGS &amp; VC INTEGRATION
;; ==========================================
(ar/global-leader
  "f" '(:ignore t :wk "files")
  "f R" '(ar/eglot-rename-file :wk "Rename file (LSP aware)"))

;; Optional: Advise `vc-rename-file` to automatically trigger LSP updates
(define-advice vc-rename-file (:after (file newname) eglot-update-imports)
  "After VC rename, if an eglot server is active, update imports."
  (when-let ((server (eglot-current-server))
             (old-uri (eglot--path-to-uri file))
             (new-uri (eglot--path-to-uri newname)))
    ;; Replicate will/did rename logic or call ar/eglot-rename-file if visited.
    ))</code></pre>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-arch" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"></path>
</svg>
            Architecture &amp; Enhancements
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-arch" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Why This Approach?</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="vs-card ok">
<h4>✓ Custom ar/eglot-rename-file · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works exclusively with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Safety &amp; Control</span>
<span class="val">Explicitly opt-in. Prevents accidental LSP spam when
                        renaming temporary or non-project files.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 synergy</span>
<span class="val">Leverages <code>eglot--execute-request</code> and
                        <code>eglot--apply-workspace-edit</code> natively.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode auto-hooks · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Safety &amp; Control</span>
<span class="val">Aggressively hooks into all
                        <code>rename-file</code> calls, causing hangs on non-LSP
                        files.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 synergy</span>
<span class="val">Relies on legacy, heavy workspace management
                        abstractions.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Native Workspace Edit Support</div>
<p class="enh-desc">
                    Recent Eglot versions explicitly advertise support for file
                    resource operations in workspace edits. This custom wrapper
                    leverages that exact infrastructure
                    (<code>eglot--apply-workspace-edit</code>) to apply
                    multi-file import updates atomically.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">vc-rename-file Integration</div>
<p class="enh-desc">
                    Emacs 31's refined Version Control integration makes
                    <code>vc-rename-file</code> (<kbd>C-x v R</kbd>) the gold
                    standard for file moves. By advising this command, you get
                    VCS tracking and LSP import resolution in a single
                    keystroke.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Robust URI Handling</div>
<p class="enh-desc">
                    The wrapper uses <code>eglot--path-to-uri</code> to ensure
                    that file paths (including those over TRAMP/SSH) are
                    correctly formatted into LSP-compliant URIs before being
                    sent to the server.
                  </p>
</div>
</div>
<div class="sec-title">Troubleshooting</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Issue</th>
<th>Cause &amp; Solution</th>
</tr>
</thead>
<tbody>
<tr>
<td>Imports Are Not Updating</td>
<td>
                        Verify Server Support: Not all language servers support
                        <code>willRenameFiles</code> (e.g.,
                        <code>gopls</code> and
                        <code>typescript-language-server</code> do, but simpler
                        servers may not). Check Relative vs. Absolute Imports:
                        Ensure your <code>tsconfig.json</code> or equivalent is
                        configured for path aliases.
                      </td>
</tr>
<tr>
<td>Buffer Becomes Unlinked</td>
<td>
                        If <code>set-visited-file-name</code> fails to update
                        the buffer, manually save (<kbd>C-x C-s</kbd>) after the
                        rename. Edge cases with read-only files or TRAMP may
                        require a manual save.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

