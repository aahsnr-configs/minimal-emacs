---
title: "Inline Values (Debugger)"
category: "Diagnostics & Debugging"
status: "Working"
parity: "Inline variable value overlays displayed at the end of the line when execution is paused"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Inline Values (Debugger)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Debugging</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Inline variable value overlays displayed at the end of the line
            when execution is paused</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">Protocol</span>
<code>Debug Adapter Protocol (DAP)</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>dape<span class="route-arrow">→</span>dape-inline-variables<span class="route-arrow">→</span>native buffer overlays</code>
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
                        Inline grey text showing variable values when paused
                      </td>
<td>
<code>dape-inline-variables t</code> renders
                        <code>dape-inline-value-face</code> overlays at the end
                        of the line.
                      </td>
</tr>
<tr>
<td>Overlays update automatically on step</td>
<td>
<code>dape-next</code> / <code>dape-step-in</code>
                        triggers an overlay refresh with the new scope's
                        variable state.
                      </td>
</tr>
<tr>
<td>Overlays clear when continuing</td>
<td>
<code>dape-continue</code> automatically removes all
                        inline value overlays.
                      </td>
</tr>
<tr>
<td>Click to evaluate or expand</td>
<td>
<code>dape</code> overlays can be configured to trigger
                        <code>dape-evaluate</code> on mouse interaction (or via
                        <kbd>SPC d e</kbd>).
                      </td>
</tr>
<tr>
<td>Respects editor theme</td>
<td>
<code>dape-inline-value-face</code> is customized to
                        inherit <code>shadow</code> with Tokyo Night accent
                        colors.
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
                    Manages all LSP features (completion, diagnostics, semantic
                    tokens), while <code>dape</code> exclusively handles the
                    debugging lifecycle. This strict separation prevents
                    protocol overlap.
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
<div class="eco-sub">Font-lock Priority</div>
</div>
</div>
<p class="eco-desc">
                    The inline value overlays are rendered with a lower priority
                    than <code>treesit</code> font-lock, ensuring that primary
                    syntax highlighting always takes visual precedence.
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
                    Eagerly registers the <kbd>SPC d</kbd> prefix for all
                    debugging commands, ensuring consistent, mnemonic access
                    regardless of the major mode.
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
                    The custom <code>dape-inline-value-face</code> inherits the
                    Tokyo Night <code>shadow</code> and <code>#73daca</code>
                    (teal) accents, maintaining a cohesive IDE aesthetic.
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
<div class="stack-name">dape</div>
<div class="stack-role">Debug Client</div>
<div class="stack-desc">
                      The modern, lightweight Debug Adapter Protocol client for
                      Emacs, explicitly designed as a minimalist alternative to
                      the heavy <code>dap-mode</code> ecosystem.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">dape-inline-variables</div>
<div class="stack-role">Overlay Engine</div>
<div class="stack-desc">
                      Built-in <code>dape</code> feature that renders variable
                      values as <code>after-string</code> overlays at the end of
                      the line when the debugger is paused.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="4"></circle>
<path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">dape-inline-value-face</div>
<div class="stack-role">Visual Styling</div>
<div class="stack-desc">
                      Customizable face allowing the inline values to blend
                      seamlessly with the editor theme (e.g., Tokyo Night).
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
<td>Toggle inline variables</td>
<td><code>dape-toggle-inline-variables</code></td>
<td>—</td>
<td>Toggles the overlay visibility dynamically.</td>
</tr>
<tr>
<td>Continue execution</td>
<td><code>dape-continue</code></td>
<td><kbd>F5</kbd> / <kbd>SPC d c</kbd></td>
<td>
                        Resumes execution; inline overlays automatically clear.
                      </td>
</tr>
<tr>
<td>Step over</td>
<td><code>dape-next</code></td>
<td><kbd>F10</kbd> / <kbd>SPC d n</kbd></td>
<td>
                        Steps to the next line; overlays update with new
                        variable states.
                      </td>
</tr>
<tr>
<td>Step into</td>
<td><code>dape-step-in</code></td>
<td><kbd>F11</kbd> / <kbd>SPC d i</kbd></td>
<td>Steps into the current function call.</td>
</tr>
<tr>
<td>Evaluate expression</td>
<td><code>dape-evaluate</code></td>
<td><kbd>SPC d e</kbd></td>
<td>
                        Opens minibuffer to evaluate and temporarily display an
                        expression.
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
<span class="fname">init-inline-values.el</span>
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
;; DAPE (Lightweight Debug Adapter Protocol)
;; ==========================================
(use-package dape
  :ensure t
  :defer t
  :commands (dape dape-breakpoint-toggle dape-continue dape-next dape-step-in)
  :custom
  ;; Enable inline variable overlays while paused in the debugger.
  (dape-inline-variables t)
  ;; Optional: Configure the window layout for the REPL/buffers
  (dape-buffer-window-arrangement 'right)
  :config
  ;; Enable global breakpoint fringe indicators
  (dape-breakpoint-global-mode 1)
  ;; ==========================================
  ;; VISUAL STYLING (Tokyo Night Synergy)
  ;; ==========================================
  (custom-set-faces
   '(dape-inline-value-face
     ((t (:inherit shadow
                   :foreground "#73daca"
                   :height 0.9
                   :slant italic
                   :box (:line-width 1 :color "#292e42" :style nil)))))))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "d" '(:ignore t :wk "debug")
  "d c" '(dape-continue :wk "Continue")
  "d n" '(dape-next :wk "Step over")
  "d i" '(dape-step-in :wk "Step into")
  "d o" '(dape-step-out :wk "Step out")
  "d b" '(dape-breakpoint-toggle :wk "Toggle breakpoint")
  "d d" '(dape :wk "Start debugging"))</code></pre>{% endraw %}
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
<h4>✓ dape · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Ecosystem Weight</span>
<span class="val">Minimalist, modern, actively maintained.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol Compliance</span>
<span class="val">Honors the "no <code>dap-mode</code>" constraint while
                        providing full DAP parity.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Lightweight overlay rendering with minimal main-thread
                        impact.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">Integrates cleanly with modern Emacs window management
                        and <code>eglot</code>-managed buffers.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ dap-mode · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Ecosystem Weight</span>
<span class="val">Heavy, complex, and increasingly deprecated in favor of
                        <code>dape</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol Compliance</span>
<span class="val">Violates the strict architectural constraint against
                        the <code>dap-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Known for UI stutter and heavy buffer management during
                        debug sessions.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">Legacy architecture with outdated window splitting
                        logic.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Minimalist Overlay Architecture</div>
<p class="enh-desc">
<code>dape</code> was explicitly designed to avoid the heavy
                    UI overhead of legacy debuggers. Its inline variable
                    implementation uses efficient <code>after-string</code> text
                    properties that do not interfere with
                    <code>treesit</code> fontification or
                    <code>eglot</code> semantic tokens.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Seamless eglot Coexistence</div>
<p class="enh-desc">
                    Because <code>dape</code> handles only the Debug Adapter
                    Protocol, it coexists perfectly with <code>eglot</code>
                    (which handles the Language Server Protocol). There is no
                    protocol overlap or resource contention, allowing both to
                    run simultaneously in the same buffer without conflict.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Native Breakpoint Indicators</div>
<p class="enh-desc">
<code>dape-breakpoint-global-mode</code> provides clean,
                    non-intrusive fringe indicators for breakpoints,
                    complementing the inline value overlays without cluttering
                    the margin.
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
<td>Inline Values Not Appearing</td>
<td>
                        Verify DAP Configuration: Ensure your
                        <code>dape-configs</code> are correctly set up for your
                        language (e.g., <code>python</code> using
                        <code>debugpy</code>, <code>go</code> using
                        <code>dlv</code>). Check Variable Scope: Inline values
                        only appear for variables in the current stack frame's
                        scope.
                      </td>
</tr>
<tr>
<td>Overlays Clash with Syntax Highlighting</td>
<td>
                        If the inline values are too bright or distracting,
                        adjust the <code>dape-inline-value-face</code> in your
                        configuration to use a more recessive color (e.g.,
                        <code>:foreground "#565f89"</code>) and ensure
                        <code>:inherit shadow</code> is set.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

