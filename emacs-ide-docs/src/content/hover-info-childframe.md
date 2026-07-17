---
title: "Hover Info (Childframe Parity)"
category: "Completion & Intelligence"
status: "Working"
parity: "Floating tooltip with rich markdown, type info, and signatures"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Hover Info (Childframe Parity)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Floating tooltip with rich markdown, type info, and
            signatures</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/hover</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eldoc<span class="route-arrow">→</span>eldoc-box (childframe) OR *eldoc* buffer (TTY)</code>
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
<td>Floating tooltip on cursor idle</td>
<td>
<code>eldoc-box-hover-at-point-mode</code> spawns
                        childframe after <code>eldoc-idle-delay</code>.
                      </td>
</tr>
<tr>
<td>Rich markdown rendering</td>
<td>
<code>markdown-ts-mode</code> fontifies code blocks
                        inside the <code>eldoc-box</code> childframe.
                      </td>
</tr>
<tr>
<td>Tooltip disappears on cursor move</td>
<td>
<code>eldoc-box-clear-after-use t</code> destroys the
                        childframe instantly.
                      </td>
</tr>
<tr>
<td>Single-line hints in status bar</td>
<td>
<code>eldoc-box-only-multi-line t</code> keeps 1-liners
                        in the echo area.
                      </td>
</tr>
<tr>
<td>Scroll long documentation</td>
<td>
<kbd>C-M-v</kbd> / <kbd>C-M-S-v</kbd> scrolls the
                        <code>eldoc-box</code> childframe window.
                      </td>
</tr>
<tr>
<td>Hover on completion candidate</td>
<td>
<code>corfu-popupinfo-toggle</code> (<kbd>M-h</kbd>)
                        spawns a childframe for
                        <code>completionItem/resolve</code>.
                      </td>
</tr>
<tr>
<td>Works over SSH / Terminal</td>
<td>
                        Emacs 31
                        <code>eldoc-echo-area-prefer-doc-buffer</code> routes to
                        <code>*eldoc*</code> buffer natively.
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
                    Drives <code>textDocument/hover</code>, returning Markdown
                    payloads natively via
                    <code>eglot-hover-eldoc-function</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">eldoc</div>
<div class="eco-sub">Documentation Router</div>
</div>
</div>
<p class="eco-desc">
                    Aggregates hover payloads and routes them to the active
                    display backend (childframe or TTY fallback).
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">eldoc-box</div>
<div class="eco-sub">GUI Rendering Engine</div>
</div>
</div>
<p class="eco-desc">
                    Spawns a floating childframe anchored to the cursor,
                    rendering rich markdown without shifting buffer text.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">markdown-ts-mode</div>
<div class="eco-sub">Markdown Fontification</div>
</div>
</div>
<p class="eco-desc">
                    Provides C-level tree-sitter syntax highlighting for code
                    blocks inside the hover tooltip.
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
                      Built-in. Drives <code>textDocument/hover</code>,
                      returning Markdown payloads natively.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eldoc</div>
<div class="stack-role">Documentation Router</div>
<div class="stack-desc">
                      Built-in. Aggregates hover payloads and routes them to the
                      active display backend.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eldoc-box</div>
<div class="stack-role">GUI Rendering Engine</div>
<div class="stack-desc">
                      Spawns a floating childframe anchored to the cursor,
                      rendering rich markdown without shifting buffer text.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eldoc (TTY Fallback)</div>
<div class="stack-role">TTY Fallback Engine</div>
<div class="stack-desc">
                      Routes payloads to an ephemeral <code>*eldoc*</code>
                      buffer when childframes are unavailable (e.g., over SSH).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(224, 175, 104, 0.1);
                      color: var(--yellow);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 16v-4M12 8h.01"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">markdown-ts-mode</div>
<div class="stack-role">Markdown Fontification</div>
<div class="stack-desc">
                      Built-in. Provides C-level tree-sitter syntax highlighting
                      for code blocks inside the hover tooltip.
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
<td>Hover at point (keyboard)</td>
<td><code>eldoc</code></td>
<td><kbd>K</kbd> (Evil normal)</td>
<td>
                        Triggers <code>textDocument/hover</code> and spawns the
                        childframe.
                      </td>
</tr>
<tr>
<td>Help at point</td>
<td><code>help-at-point</code></td>
<td><kbd>C-h .</kbd></td>
<td>
                        Native Emacs help surfacing via
                        <code>eldoc-help-at-pt</code>.
                      </td>
</tr>
<tr>
<td>Scroll hover tooltip</td>
<td>
<code>eldoc-box-scroll-up</code> /
                        <code>down</code>
</td>
<td><kbd>C-M-v</kbd> / <kbd>C-M-S-v</kbd></td>
<td>
                        Scrolls the childframe when docstrings exceed the
                        viewport.
                      </td>
</tr>
<tr>
<td>Toggle candidate docs</td>
<td><code>corfu-popupinfo-toggle</code></td>
<td><kbd>M-h</kbd> (in <code>corfu-map</code>)</td>
<td>
                        Shows/hides childframe docs for the active completion
                        candidate.
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
<span class="fname">init-hover-childframe.el</span>
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
;; 1. ELDOC-BOX (GUI Childframe Hover)
;; ==========================================
(use-package eldoc-box
  :ensure t
  :after eglot
  :custom
  ;; Clear the childframe immediately when the cursor moves off the symbol.
  (eldoc-box-clear-after-use t)
  ;; Only spawn the childframe for multi-line payloads.
  (eldoc-box-only-multi-line t)
  ;; Position the childframe slightly offset from the cursor.
  (eldoc-box-offset '(10 10 10))
  :custom-face
  ;; Tokyo Night synergy: Match the childframe background and border.
  (eldoc-box-border ((t (:background "#292e42"))))
  (eldoc-box-default-face ((t (:background "#1a1b26" :foreground "#c0caf5"))))
  :config
  ;; Enable hover-at-point tracking.
  (eldoc-box-hover-at-point-mode 1))

;; ==========================================
;; 2. ELDOC CORE (Emacs 31 TTY Fallback &amp; Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-help-at-pt t)
  ;; TTY Fallback: Route long docs to the ephemeral `*eldoc*` buffer.
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay 0.5)
  :config
  (global-eldoc-mode 1))</code></pre>{% endraw %}
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
<h4>✓ eldoc-box · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Rendering</span>
<span class="val">Childframe (Floating GUI window). Floats above text;
                        zero layout shift.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Works with <i>any</i> eldoc backend (eglot).</span>
</div>
<div class="vs-row">
<span class="lab">Use Case</span>
<span class="val">Hover Info (VS Code Tooltip parity).</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Lightweight, respects
                        <code>eldoc-box-only-multi-line</code>.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-doc / peek · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Rendering</span>
<span class="val"><code>lsp-ui-doc</code> requires forbidden lsp-mode;
                        <code>peek</code> uses overlays (shifts text).</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Hard-bound to <code>lsp-mode</code> ecosystem or
                        designed for inline panels.</span>
</div>
<div class="vs-row">
<span class="lab">Use Case</span>
<span class="val"><code>peek</code> is strictly for Peek Definition
                        (Alt+F12).</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Heavy child-frame pipeline or high redisplay
                        overhead.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">PGTK Child-Frame Pixel Accuracy</div>
<p class="enh-desc">
                    Emacs 31 fixes severe child-frame positioning bugs on
                    Wayland (PGTK builds). <code>eldoc-box</code> tooltips now
                    anchor perfectly to the cursor baseline without drifting.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Native TTY Degradation</div>
<p class="enh-desc">
                    If <code>eldoc-box</code> detects a TTY frame, Emacs 31's
                    native <code>eldoc-echo-area-prefer-doc-buffer</code>
                    seamlessly intercepts the payload and routes it to a split
                    <code>*eldoc*</code> buffer.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">markdown-ts-mode Integration</div>
<p class="enh-desc">
                    Emacs 31's native tree-sitter markdown mode fontifies the
                    childframe buffer at C-speed, providing syntax-highlighted
                    code blocks inside the hover tooltip.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

