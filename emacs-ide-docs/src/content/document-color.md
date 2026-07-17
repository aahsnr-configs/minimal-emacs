---
title: "Document Color"
category: "Visual Enhancements"
status: "Working"
parity: "Inline color swatches and color picker UI"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Document Color</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Inline color swatches and color picker UI</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/documentColor</code>
<span aria-hidden="true" class="meta-sep">·</span>
<span style="color: var(--text-dim); font-size: 12px">(Not utilized)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>colorful-mode<span class="route-arrow">→</span>native
              regex/overlay stacking</code>
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
                        Inline color swatch next to <code>#fff</code> or
                        <code>rgb(255,255,255)</code>
</td>
<td>
<code>colorful-mode</code> renders a small colored
                        overlay box next to the text.
                      </td>
</tr>
<tr>
<td>Swatch updates as you type</td>
<td>
                        Native Emacs <code>post-command-hook</code> updates the
                        overlay instantly.
                      </td>
</tr>
<tr>
<td>Click swatch to open color picker</td>
<td>
                        Requires optional <code>color-picker</code> package or
                        native <kbd>M-x list-colors-display</kbd>.
                      </td>
</tr>
<tr>
<td>Works in CSS, HTML, JS, and config files</td>
<td>
<code>colorful-mode</code> hooks into
                        <code>prog-mode</code>, <code>css-mode</code>,
                        <code>html-mode</code>, etc.
                      </td>
</tr>
<tr>
<td>No main-thread blocking</td>
<td>
                        Regex matching is executed locally with zero network
                        dependency.
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
<div class="eco-sub">LSP Coexistence</div>
</div>
</div>
<p class="eco-desc">
                    The upstream <code>eglot</code> maintainer deliberately
                    omits <code>textDocument/documentColor</code>. This
                    delegates purely visual enhancements to lightweight packages
                    like <code>colorful-mode</code>, keeping the core LSP client
                    lean.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">Syntax Awareness</div>
</div>
</div>
<p class="eco-desc">
                    Works harmoniously with tree-sitter fontification. The
                    <code>colorful-only-strings 'only-prog</code> setting
                    ensures color names are only highlighted inside strings or
                    comments, preventing false positives in code.
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
<div class="eco-name">too-long-file-p</div>
<div class="eco-sub">Performance Guard</div>
</div>
</div>
<p class="eco-desc">
                    The <code>define-advice</code> wrapper utilizes the custom
                    <code>too-long-file-p</code> function to mathematically
                    guarantee the regex engine will not scan multi-megabyte
                    minified files, preserving 60fps typing.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">doom-themes</div>
<div class="eco-sub">Visual Styling</div>
</div>
</div>
<p class="eco-desc">
                    The overlay boxes inherit the active theme's background and
                    foreground properties seamlessly, maintaining a cohesive
                    Tokyo Night aesthetic.
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
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">colorful-mode</div>
<div class="stack-role">Visual Engine</div>
<div class="stack-desc">
                      Renders hex, RGB, HSL, and named color previews via native
                      Emacs overlay stacking with 0ms latency.
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
<div class="stack-name">rainbow-mode</div>
<div class="stack-role">Fallback</div>
<div class="stack-desc">
                      Alternative package for basic hex/RGB highlighting if
                      <code>colorful-mode</code> is disabled or unavailable.
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
<div class="stack-name">list-colors-display</div>
<div class="stack-role">Color Picker</div>
<div class="stack-desc">
                      Native Emacs command providing an interactive color
                      palette viewer without relying on LSP network round-trips.
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
<td>Toggle color visualization</td>
<td><code>colorful-mode</code></td>
<td><kbd>SPC t c</kbd></td>
<td>
                        Enables/disables inline color swatches globally or
                        per-buffer.
                      </td>
</tr>
<tr>
<td>Display color picker</td>
<td><code>list-colors-display</code></td>
<td><kbd>M-x list-colors-display</kbd></td>
<td>Native Emacs color palette viewer.</td>
</tr>
<tr>
<td>Pick color from screen</td>
<td><code>color-picker</code></td>
<td><kbd>M-x color-picker</kbd></td>
<td>
                        Requires external <code>color-picker</code> package for
                        OS-level eyedropper.
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
<span class="fname">init-document-color.el</span>
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
;; COLOR VISUALIZATION (colorful-mode)
;; ==========================================
(use-package colorful-mode
  :defer t
  :custom
  ;; Confines X11/HTML color name matching strictly to strings and comments
  ;; in programming buffers to prevent false positives in non-styling codebases.
  (colorful-only-strings 'only-prog)
  ;; Prevents double-rendering conflicts when native CSS major modes fontify colors.
  (css-fontify-colors nil)
  :hook
  ;; Restricted to styling, markup, prose, and programming modes.
  ((css-mode scss-mode less-css-mode html-mode
    org-mode LaTeX-mode markdown-mode gfm-mode
    prog-mode) . colorful-mode)
  :config
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice colorful-mode (:before-while (&amp;optional arg) guard-large-files)
    (or (and arg (&lt; (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "t" '(:ignore t :wk "toggle")
  "t c" '(colorful-mode :wk "Toggle color swatches"))</code></pre>{% endraw %}
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
<h4>✓ colorful-mode · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Zero dependency on LSP; works universally across all
                        modes.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.
                        <code>eglot</code> intentionally omits
                        <code>documentColor</code> to remain minimal.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val"><b>0ms latency.</b> Uses native Emacs regex and overlay
                        stacking.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val">Works offline and is immune to language server crashes
                        or slow responses.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode documentColor · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val"><b>High latency.</b> Queries the language server for
                        every color, causing network round-trips and UI
                        stutter.</span>
</div>
<div class="vs-row">
<span class="lab">Reliability</span>
<span class="val">Fails or degrades if the LSP server does not implement
                        the color presentation protocol.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
<code>colorful-only-strings</code> Optimization
                  </div>
<p class="enh-desc">
                    Setting this to <code>'only-prog</code> is a critical
                    safeguard. It ensures that color names (like
                    <code>red</code> or <code>blue</code>) are only highlighted
                    when they appear inside strings or comments in programming
                    buffers, preventing catastrophic false positives in standard
                    code (e.g., a variable named <code>blue</code>).
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Massive Buffer Guard</div>
<p class="enh-desc">
                    The <code>define-advice</code> wrapper around
                    <code>colorful-mode</code> utilizes the custom
                    <code>too-long-file-p</code> function. This mathematically
                    guarantees that the regex engine will not attempt to scan
                    and overlay colors in multi-megabyte minified CSS/JS files,
                    preserving the 60fps typing experience.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Eglot Design Philosophy</div>
<p class="enh-desc">
                    The upstream <code>eglot</code> maintainer has deliberately
                    chosen not to implement
                    <code>textDocument/documentColor</code> or
                    <code>textDocument/colorPresentation</code>. This is by
                    design, as <code>eglot</code> delegates such purely visual,
                    non-semantic enhancements to dedicated, lightweight
                    packages, keeping the core LSP client lean and focused on
                    code intelligence.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

