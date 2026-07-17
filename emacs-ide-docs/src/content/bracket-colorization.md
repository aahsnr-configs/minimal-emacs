---
title: "Bracket Pair Colorization"
category: "Visual Enhancements"
status: "Working"
parity: "Color-coded highlighting for nested parentheses, brackets, and braces"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Bracket Pair Colorization</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Color-coded highlighting for nested parentheses, brackets, and
            braces</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<span style="color: var(--text-dim); font-size: 12px">N/A (Pure editor visual enhancement)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>rainbow-delimiters-mode<span class="route-arrow">→</span>text
              properties / overlays</code>
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
<td>Nested brackets colorized by depth</td>
<td>
<code>rainbow-delimiters-mode</code> assigns distinct
                        colors to each nesting level.
                      </td>
</tr>
<tr>
<td>Matching pair highlighted on cursor</td>
<td>
<code>show-paren-mode</code> highlights the matching
                        delimiter instantly.
                      </td>
</tr>
<tr>
<td>Colors match theme</td>
<td>
                        Custom faces integrate seamlessly with Tokyo Night
                        palette.
                      </td>
</tr>
<tr>
<td>No performance lag</td>
<td>
                        Text property-based rendering is highly optimized.
                      </td>
</tr>
<tr>
<td>Works across all programming modes</td>
<td>
                        Hooked into <code>prog-mode</code>, covering Python,
                        Rust, TypeScript, etc.
                      </td>
</tr>
<tr>
<td>Mismatched brackets visually distinct</td>
<td>
<code>show-paren-mismatch</code> face highlights errors
                        in red.
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
<circle cx="12" cy="12" r="10"></circle>
<path d="M8 12h8M12 8v8"></path>
</svg>
</div>
<div>
<div class="eco-name">show-paren-mode</div>
<div class="eco-sub">Matching Highlight</div>
</div>
</div>
<p class="eco-desc">
                    Complements <code>rainbow-delimiters</code> by providing
                    instant visual feedback when the cursor is on a delimiter,
                    while <code>rainbow-delimiters</code> provides the
                    persistent depth-based colorization.
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
<div class="eco-sub">AST Fontification</div>
</div>
</div>
<p class="eco-desc">
                    Works harmoniously with tree-sitter fontification, ensuring
                    that bracket colors don't interfere with syntax highlighting
                    or semantic tokens.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
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
                    The customized faces inherit the Tokyo Night palette,
                    maintaining a cohesive, professional IDE aesthetic where
                    each nesting level has a distinct, readable color.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
</div>
<div>
<div class="eco-name">evil-collection</div>
<div class="eco-sub">Modal Parity</div>
</div>
</div>
<p class="eco-desc">
                    Fully compatible with Evil mode; bracket colorization works
                    correctly in normal, insert, and visual states without
                    interfering with modal editing.
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
<div class="stack-name">rainbow-delimiters</div>
<div class="stack-role">Primary Engine</div>
<div class="stack-desc">
                      GNU ELPA package. Highlights delimiters such as
                      parentheses, brackets, or braces according to their
                      nesting depth with distinct colors.
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
<div class="stack-name">show-paren-mode</div>
<div class="stack-role">Complementary Matcher</div>
<div class="stack-desc">
                      Built-in. Highlights the matching pair when the cursor is
                      on or adjacent to a delimiter.
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
<div class="stack-name">
                      rainbow-delimiters-depth-N-face
                    </div>
<div class="stack-role">Visual Styling</div>
<div class="stack-desc">
                      Customizable faces for each nesting level (depth 1, 2, 3,
                      etc.) allowing precise theme integration.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">Internal depth limiting</div>
<div class="stack-role">Performance Guard</div>
<div class="stack-desc">
                      Prevents excessive color depth in deeply nested code,
                      avoiding visual clutter and performance degradation.
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
<td>Toggle rainbow delimiters</td>
<td><code>rainbow-delimiters-mode</code></td>
<td><kbd>SPC t r</kbd></td>
<td>
                        Enables/disables color-coded bracket highlighting.
                      </td>
</tr>
<tr>
<td>Toggle globally</td>
<td><code>global-rainbow-delimiters-mode</code></td>
<td>—</td>
<td>
                        Enables rainbow delimiters across all programming
                        buffers.
                      </td>
</tr>
<tr>
<td>Toggle show-paren</td>
<td><code>show-paren-mode</code></td>
<td>—</td>
<td>
                        Built-in mode that highlights matching pair at cursor.
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
<span class="fname">init-bracket-colorization.el</span>
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
;; RAINBOW-DELIMITERS (Depth-Based Colorization)
;; ==========================================
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  ;; Maximum nesting depth to colorize (prevents visual clutter in deeply nested code).
  (rainbow-delimiters-max-face-count 9)
  ;; Disable in specific modes where rainbow colors are distracting.
  (rainbow-delimiters-disabled-modes '(org-mode text-mode))
  :config
  ;; Customize faces to match Tokyo Night theme
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#bb9af7"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#7dcfff"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#e0af68"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#9ece6a"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#f7768e"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#c0caf5"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#a9b1d8")))))
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice rainbow-delimiters-mode (:before-while (&amp;optional arg) guard-large-files)
    (or (and arg (&lt; (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; SHOW-PAREN-MODE (Built-in Matching)
;; ==========================================
(use-package paren
  :ensure nil
  :custom
  ;; Delay before highlighting (prevents flicker during fast movement).
  (show-paren-delay 0)
  ;; Style: 'expression' highlights the entire expression between parens.
  (show-paren-style 'expression)
  ;; Emacs 31 NEW: Prevent phantom bracket highlighting inside strings and comments.
  (show-paren-not-in-comments-or-strings t)
  :custom-face
  ;; Tokyo Night synergy for show-paren faces
  (show-paren-match ((t (:background "#292e42" :foreground "#7aa2f7" :weight bold))))
  (show-paren-mismatch ((t (:background "#f7768e" :foreground "#1a1b26" :weight bold)))))
  :config
  ;; Highlight the matching parenthesis when cursor is on/near one.
  (show-paren-mode 1))</code></pre>{% endraw %}
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
<h4>✓ rainbow-delimiters + show-paren-mode · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Lightweight text properties; negligible overhead even
                        in large files.</span>
</div>
<div class="vs-row">
<span class="lab">Customization</span>
<span class="val">Fully customizable faces for each depth level, allowing
                        perfect theme integration.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs Philosophy</span>
<span class="val">Follows Emacs' modular design: separate modes for
                        different highlighting behaviors.</span>
</div>
<div class="vs-row">
<span class="lab">Flexibility</span>
<span class="val">Can be disabled per-mode, per-buffer, or conditionally
                        based on file size.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ VS Code Native (reference)</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Built-in C++ engine; extremely fast but not
                        customizable.</span>
</div>
<div class="vs-row">
<span class="lab">Customization</span>
<span class="val">Limited to predefined color sets; harder to match
                        custom themes.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs Philosophy</span>
<span class="val">Monolithic implementation tightly coupled to the editor
                        core.</span>
</div>
<div class="vs-row">
<span class="lab">Flexibility</span>
<span class="val">Global setting with limited granular control.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Refined Text Property Engine</div>
<p class="desc">
                    Emacs 31's optimized text property application ensures that
                    <code>rainbow-delimiters</code> renders colors with zero
                    redisplay lag, even in files with hundreds of nested levels.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Treesit Integration</div>
<p class="desc">
                    When used with <code>treesit</code> major modes (e.g.,
                    <code>python-ts-mode</code>, <code>rust-ts-mode</code>),
                    <code>rainbow-delimiters</code> works seamlessly alongside
                    AST-based syntax highlighting without conflicts.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
                    show-paren-not-in-comments-or-strings (NEW)
                  </div>
<p class="desc">
                    Emacs 31 introduces this native variable to prevent
                    <code>show-paren-mode</code> from erroneously highlighting
                    mismatched or matched brackets that reside strictly inside
                    string literals or comments, eliminating visual noise.
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
<td>Colors Are Too Subtle or Hard to Distinguish</td>
<td>
                        Adjust the face customizations to increase saturation or
                        contrast. The default <code>rainbow-delimiters</code>
                        colors can be drab and indistinguishable, so explicitly
                        setting bright, theme-aligned colors (as shown in the
                        configuration) is recommended.
                      </td>
</tr>
<tr>
<td>Rainbow Delimiters Not Appearing in Certain Modes</td>
<td>
                        Check if the mode is listed in
                        <code>rainbow-delimiters-disabled-modes</code>. If you
                        want rainbow delimiters in a specific mode, ensure it
                        derives from <code>prog-mode</code> or add it explicitly
                        to the hook.
                      </td>
</tr>
<tr>
<td>Performance Lag in Large Files</td>
<td>
                        The <code>too-long-file-p</code> guard should prevent
                        activation in massive buffers. If you still experience
                        lag, reduce
                        <code>rainbow-delimiters-max-face-count</code> to limit
                        the number of depth levels being colorized.
                      </td>
</tr>
<tr>
<td>Mismatched Brackets Not Highlighted</td>
<td>
                        Ensure <code>show-paren-mode</code> is enabled and that
                        <code>show-paren-style</code> is set appropriately. The
                        <code>expression</code> style provides the most
                        comprehensive highlighting, showing both the delimiter
                        and the enclosed region.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

