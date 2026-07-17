---
title: "On-type Formatting"
category: "Formatting & Editing"
status: "Working"
parity: "Auto-indent and auto-pairing on trigger characters ( }, ;, (, etc.)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>On-type Formatting</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Formatting &amp; Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Auto-indent and auto-pairing on trigger characters (
            <code>}</code>, <code>;</code>, <code>(</code>, etc.)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/onTypeFormatting</code>
<span aria-hidden="true" class="meta-sep">·</span>
<span style="color: var(--text-dim); font-size: 12px">(Explicitly ignored)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>electric-indent-mode + electric-pair-mode +
              electric-layout-mode</code>
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
<td>
                        Auto-indents when typing <code>}</code> or
                        <code>;</code>
</td>
<td>
<code>electric-indent-mode</code> re-indents instantly
                        on trigger characters.
                      </td>
</tr>
<tr>
<td>
                        Auto-inserts closing <code>)</code> or <code>}</code>
</td>
<td>
<code>electric-pair-mode</code> inserts the matching
                        delimiter.
                      </td>
</tr>
<tr>
<td>Auto-inserts <code>/*</code> and <code>*/</code></td>
<td>
                        Emacs 31 <code>electric-pair-pairs</code> natively
                        supports multi-character string pairs.
                      </td>
</tr>
<tr>
<td>Adds newlines around <code>{</code> automatically</td>
<td>
<code>electric-layout-mode</code> enforces structural
                        newline rules.
                      </td>
</tr>
<tr>
<td>No lag or stutter while typing</td>
<td>
                        Native C-level execution guarantees 0ms latency, unlike
                        LSP network requests.
                      </td>
</tr>
<tr>
<td>Cursor stays in the correct position</td>
<td>
                        Native modes preserve point and mark perfectly; LSP
                        on-type formatting is known to displace the cursor.
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
<div class="eco-sub">LSP Guard</div>
</div>
</div>
<p class="eco-desc">
                    By adding <code>:documentOnTypeFormattingProvider</code> to
                    <code>eglot-ignored-server-capabilities</code>, we
                    proactively prevent the language server from attempting to
                    format on every keystroke, preserving a buttery-smooth
                    typing experience.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">apheleia</div>
<div class="eco-sub">On-save Formatting</div>
</div>
</div>
<p class="eco-desc">
                    While <code>electric-*</code> modes handle on-type
                    structural formatting, <code>apheleia</code> handles on-save
                    whole-file formatting. This separation of concerns ensures
                    zero latency while typing.
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
<div class="eco-name">evil-mode</div>
<div class="eco-sub">Modal Compatibility</div>
</div>
</div>
<p class="eco-desc">
                    Electric modes are fully compatible with Evil. Auto-pairing
                    and auto-indentation trigger correctly whether you are in
                    <code>insert-state</code> or using <kbd>R</kbd> (replace)
                    state.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">org-mode</div>
<div class="eco-sub">Literate Programming</div>
</div>
</div>
<p class="eco-desc">
<code>electric-pair-mode</code> seamlessly auto-pairs
                    delimiters inside Org source blocks, improving the literate
                    programming experience without requiring LSP intervention.
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
<path d="M4 7h16M4 12h10M4 17h16"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">electric-indent-mode</div>
<div class="stack-role">Auto-Indentation</div>
<div class="stack-desc">
                      Built-in. Re-indents the current line or block instantly
                      upon typing trigger characters like <kbd>RET</kbd>,
                      <code>}</code>, or <code>;</code> with zero network
                      latency.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M8 12h8M12 8v8"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">electric-pair-mode</div>
<div class="stack-role">Auto-Pairing</div>
<div class="stack-desc">
                      Built-in. Automatically inserts closing delimiters
                      (<code>()</code>, <code>{}</code>, <code>[]</code>,
                      <code>""</code>). Emacs 31 natively extends this to
                      multi-character pairs (e.g., <code>/*</code> and
                      <code>*/</code>).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">electric-layout-mode</div>
<div class="stack-role">Layout Control</div>
<div class="stack-desc">
                      Built-in. Dictates exactly where newlines are inserted
                      automatically (e.g., adding a newline before and after
                      <code>{</code> in C-like languages).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(247, 118, 142, 0.1);
                      color: var(--red);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<line x1="4.93" x2="19.07" y1="4.93" y2="19.07"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">
                      eglot-ignored-server-capabilities
                    </div>
<div class="stack-role">LSP Guard</div>
<div class="stack-desc">
                      Explicitly disables
                      <code>:documentOnTypeFormattingProvider</code> to prevent
                      main-thread blocking and cursor-jumping bugs from LSP
                      servers.
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
<td>Toggle auto-indent</td>
<td><code>electric-indent-mode</code></td>
<td><kbd>SPC t i</kbd></td>
<td>
                        Re-indents on trigger characters. Enabled globally by
                        default.
                      </td>
</tr>
<tr>
<td>Toggle auto-pairing</td>
<td><code>electric-pair-mode</code></td>
<td><kbd>SPC t p</kbd></td>
<td>
                        Inserts matching closing delimiters. Enabled globally by
                        default.
                      </td>
</tr>
<tr>
<td>Toggle layout rules</td>
<td><code>electric-layout-mode</code></td>
<td><kbd>SPC t l</kbd></td>
<td>
                        Enforces structural newline insertion (e.g., around
                        <code>{}</code>).
                      </td>
</tr>
<tr>
<td>Manually indent line</td>
<td><code>indent-according-to-mode</code></td>
<td><kbd>TAB</kbd></td>
<td>
                        Fallback manual indentation if electric modes miss a
                        trigger.
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
<span class="fname">init-on-type-formatting.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
{% raw %}<pre><code class="language-lisp">;; ==========================================
;; 1. EGLOT: DISABLE LSP ON-TYPE FORMATTING
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Explicitly ignore onTypeFormatting to prevent main-thread blocking
  ;; and cursor-jumping issues on every keystroke.
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

;; ==========================================
;; 2. NATIVE ELECTRIC MODES (Zero-Latency Formatting)
;; ==========================================
(use-package elec-pair
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Support for multi-character paired delimiters.
  ;; Automatically pairs "/*" with "*/" and handles spacing intelligently.
  (electric-pair-pairs '(("/*" . "*/")))
  (electric-pair-text-pairs '(("/*" . "*/")))
  ;; Prevent pairing inside strings/comments where it causes syntax errors.
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode 1))

(use-package electric
  :ensure nil
  :custom
  ;; Re-indent automatically when typing trigger characters like '}' or ';'.
  (electric-indent-mode 1)
  ;; Enforce structural newlines (e.g., adding newlines around '{' in C/JS).
  (electric-layout-mode 1))</code></pre>{% endraw %}
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
<h4>✓ Native electric-* modes · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>0ms.</b> Executed in C-level Emacs core.</span>
</div>
<div class="vs-row">
<span class="lab">UI Stability</span>
<span class="val"><b>Perfect.</b> No main-thread blocking or cursor
                        jumping.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack by explicitly
                        disabling problematic LSP features.</span>
</div>
<div class="vs-row">
<span class="lab">Undo History</span>
<span class="val"><b>Clean.</b> Native Emacs commands integrate
                        seamlessly with the undo tree.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ LSP textDocument/onTypeFormatting · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Latency</span>
<span class="val"><b>High.</b> Requires a synchronous network roundtrip
                        to the LSP server on every keystroke.</span>
</div>
<div class="vs-row">
<span class="lab">UI Stability</span>
<span class="val"><b>Poor.</b> Known to cause severe UI stutter and
                        incorrect point placement.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Violates the principle of a responsive editor; heavily
                        discouraged in the eglot community.</span>
</div>
<div class="vs-row">
<span class="lab">Undo History</span>
<span class="val"><b>Fragmented.</b> LSP text edits applied mid-typing
                        often corrupt or split undo boundaries.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">
                    Multi-Character Electric Pairs (NEW)
                  </div>
<p class="enh-desc">
                    Emacs 31's <code>electric-pair-mode</code> now natively
                    supports multi-character paired delimiters. You can define
                    pairs like <code>("/*" . "*/")</code> in
                    <code>electric-pair-pairs</code>, and Emacs will
                    intelligently auto-complete them with optional auto-spacing,
                    a feature previously requiring heavy third-party packages
                    like <code>smartparens</code>.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Conservative Inhibition</div>
<p class="enh-desc">
                    The <code>electric-pair-conservative-inhibit</code>
                    predicate (standard in modern Emacs) prevents auto-pairing
                    from triggering inside strings or comments, eliminating the
                    "phantom bracket" syntax errors that plagued older
                    configurations.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Treesit Integration</div>
<p class="enh-desc">
                    Native electric modes work flawlessly alongside
                    <code>treesit</code> major modes (e.g.,
                    <code>python-ts-mode</code>, <code>rust-ts-mode</code>), as
                    they operate on the syntax table and character level,
                    completely independent of the LSP server's parsing speed.
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
<td>Cursor Jumps or Stutters When Typing</td>
<td>
                        This is the hallmark symptom of LSP
                        <code>onTypeFormatting</code> being active. Verify that
                        <code>:documentOnTypeFormattingProvider</code> is
                        present in your
                        <code>eglot-ignored-server-capabilities</code> list.
                      </td>
</tr>
<tr>
<td>Auto-Pairing Triggers Inside Strings/Comments</td>
<td>
                        Ensure <code>electric-pair-inhibit-predicate</code> is
                        set to <code>'electric-pair-conservative-inhibit</code>.
                        This prevents Emacs from inserting closing quotes or
                        brackets where they would break string/comment syntax.
                      </td>
</tr>
<tr>
<td>Multi-Character Pairs Not Working</td>
<td>
                        Verify you are running Emacs 31 or later, as
                        multi-character string support in
                        <code>electric-pair-pairs</code> is a recent
                        enhancement. For older versions, third-party packages
                        like <code>smartparens</code> were required.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

