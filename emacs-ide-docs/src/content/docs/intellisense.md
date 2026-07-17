---
title: "IntelliSense / Code Completion"
category: "Completion & Intelligence"
status: "Working"
parity: "Autocomplete popup · ghost text · auto-imports · fuzzy filtering"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>IntelliSense / Code Completion</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Autocomplete popup · ghost text · auto-imports · fuzzy
            filtering</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/completion</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>completionItem/resolve</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>capf<span class="route-arrow">→</span>cape<span class="route-arrow">→</span>corfu</code>
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
<th>Emacs Equivalent</th>
</tr>
</thead>
<tbody>
<tr>
<td>Autocomplete popup on typing</td>
<td>
<code>corfu-auto t</code> with
                        <code>corfu-auto-delay 0.2</code>
</td>
</tr>
<tr>
<td>Ghost text (inline preview)</td>
<td>
<code>corfu-candidate-overlay-mode</code> renders dimmed
                        text in-buffer
                      </td>
</tr>
<tr>
<td>Auto-import on accept</td>
<td>
<code>eglot</code> processes
                        <code>additionalTextEdits</code> on
                        <code>corfu-insert</code>
</td>
</tr>
<tr>
<td>Fuzzy / Substring filtering</td>
<td>
<code>orderless</code> matching styles (space-separated
                        components)
                      </td>
</tr>
<tr>
<td>Snippet integration</td>
<td>
<code>yasnippet-capf</code> merged into pipeline via
                        <code>cape</code>
</td>
</tr>
<tr>
<td>Documentation on hover</td>
<td>
<code>corfu-popupinfo</code> (<kbd>M-h</kbd>) triggers
                        <code>completionItem/resolve</code>
</td>
</tr>
<tr>
<td>Icons in autocomplete list</td>
<td>
<code>nerd-icons-corfu</code> injects glyphs into the
                        Corfu margin
                      </td>
</tr>
<tr>
<td>Dismiss on Escape</td>
<td>
<code>corfu-quit</code> bound to <kbd>ESC</kbd> in
                        <code>corfu-map</code>
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
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">cape</div>
<div class="eco-sub">Backend Merger</div>
</div>
</div>
<p class="eco-desc">
                    Merges <code>eglot</code> with <code>dabbrev</code> and
                    <code>file</code>. Uses
                    <code>cape-wrap-nonexclusive</code> so LSP doesn't shadow
                    local words.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div>
<div class="eco-name">orderless</div>
<div class="eco-sub">Filtering Engine</div>
</div>
</div>
<p class="eco-desc">
                    Typing <code>get usr</code> matches
                    <code>get_current_user</code> because orderless matches
                    space-separated components anywhere.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="14" rx="2" width="20" x="2" y="7"></rect>
<path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16"></path>
</svg>
</div>
<div>
<div class="eco-name">vertico</div>
<div class="eco-sub">Minibuffer UI</div>
</div>
</div>
<p class="eco-desc">
                    Handles minibuffer completion.
                    <code>global-corfu-minibuffer nil</code> ensures they never
                    fight for UI control.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(224, 175, 104, 0.1); color: var(--yellow);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">yasnippet</div>
<div class="eco-sub">Snippet Injection</div>
</div>
</div>
<p class="eco-desc">
                    Injected via <code>yasnippet-capf</code>, allowing
                    tab-completion of functions and local snippets from one
                    menu.
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
                      Built-in. Drives completion, injects candidates, handles
                      just-in-time docstrings &amp; auto-imports.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">corfu</div>
<div class="stack-role">UI Engine</div>
<div class="stack-desc">
                      Renders the minimal, high-performance child-frame popup
                      using native completion APIs.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">cape</div>
<div class="stack-role">Backend Merger</div>
<div class="stack-desc">
                      Merges LSP candidates with local Dabbrev, File, and
                      Snippets via non-exclusive wrappers.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">orderless</div>
<div class="stack-role">Filtering</div>
<div class="stack-desc">
                      Provides space-separated, out-of-order fuzzy filtering for
                      the candidate list.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(224, 175, 104, 0.1); color: var(--yellow);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 7h16M4 12h10M4 17h16"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">corfu-candidate-overlay</div>
<div class="stack-role">Ghost Text</div>
<div class="stack-desc">
                      Draws an inline, dimmed preview of the selected candidate
                      directly in the buffer.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(255, 158, 100, 0.1); color: var(--orange);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">nerd-icons-corfu</div>
<div class="stack-role">Icons</div>
<div class="stack-desc">
                      Injects LSP symbol icons into the Corfu margin for VS Code
                      visual parity.
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
<td>Trigger completion</td>
<td><code>completion-at-point</code></td>
<td><kbd>TAB</kbd> / <kbd>C-SPC</kbd></td>
<td>Indents if unaligned, triggers popup if aligned.</td>
</tr>
<tr>
<td>Next candidate</td>
<td><code>corfu-next</code></td>
<td><kbd>TAB</kbd> / <kbd>C-n</kbd></td>
<td>Cycles forward.</td>
</tr>
<tr>
<td>Previous candidate</td>
<td><code>corfu-previous</code></td>
<td><kbd>S-TAB</kbd> / <kbd>C-p</kbd></td>
<td>Cycles backward.</td>
</tr>
<tr>
<td>Insert candidate</td>
<td><code>corfu-insert</code></td>
<td><kbd>RET</kbd></td>
<td>Commits, triggering auto-imports via eglot.</td>
</tr>
<tr>
<td>Toggle documentation</td>
<td><code>corfu-popupinfo-toggle</code></td>
<td><kbd>M-h</kbd></td>
<td>Shows/hides completionItem/resolve docstring.</td>
</tr>
<tr>
<td>Quick select (insert)</td>
<td><code>corfu-quick-insert</code></td>
<td><kbd>M-q</kbd></td>
<td>Avy-style 1-2 char jump to insert candidate.</td>
</tr>
<tr>
<td>Exit completion</td>
<td><code>corfu-quit</code></td>
<td><kbd>ESC</kbd> / <kbd>C-g</kbd></td>
<td>Cancels popup without inserting.</td>
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
<span class="fname">init-completion.el</span>
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
;; 1. EGLOT (LSP Completion Backend)
;; ==========================================
(use-package eglot
:ensure nil
:custom
(eglot-completion-at-point t))
;; ==========================================
;; 2. CORFU (In-Buffer Completion UI)
;; ==========================================
(use-package corfu
:init (global-corfu-mode)
:custom
(global-corfu-minibuffer nil)
(corfu-auto t)
(corfu-auto-delay 0.2)
(corfu-auto-prefix 2)
(corfu-preselect 'prompt)
:bind (:map corfu-map
("TAB" . corfu-next)
("RET" . corfu-insert)
("&lt;escape&gt;" . corfu-quit)))</code></pre>
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
<div class="grid-2" style="margin-bottom: 24px;">
<div class="vs-card ok">
<h4>✓ corfu + eglot · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Architecture</span>
<span class="val">Uses native <code>completion-at-point</code> APIs and
                        child-frames.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Exponentially lighter; zero main-thread blocking during
                        typing.</span>
</div>
<div class="vs-row">
<span class="lab">Filtering</span>
<span class="val">Integrates with <code>orderless</code> for out-of-order
                        fuzzy matching.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31</span>
<span class="val">Leverages native TTY child-frames and PGTK Wayland
                        fixes.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ company + lsp-mode · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Architecture</span>
<span class="val">Custom overlay engine and heavy workspace
                        management.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Prone to micro-stutters and "stutter-and-vanish" popup
                        bugs.</span>
</div>
<div class="vs-row">
<span class="lab">Filtering</span>
<span class="val">Requires <code>company-flx</code>; struggles with
                        space-separated queries.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Breaks the <code>eglot</code>-only stack mandate.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">TTY Child-Frame Support</div>
<p class="enh-desc">
                    Introduces <code>tty-tip-mode</code> and native TTY
                    child-frames. Allows <code>corfu</code> to render perfectly
                    in terminals like Ghostty or Kitty.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">PGTK Wayland Fixes</div>
<p class="enh-desc">
                    Child-frame positioning is now pixel-accurate. Eliminates
                    the "drifting popup" bug, ensuring Corfu anchors to the
                    cursor baseline.
                  </p>
</div>
<div class="enh-card y" style="grid-column: 1 / -1; max-width: 50%; justify-self: center;">
<div class="enh-title">Eager Display API</div>
<p class="enh-desc">
<code>completion-eager-display</code> ensures the native
                    <code>*Completions*</code> fallback appears immediately if
                    Corfu is bypassed.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

