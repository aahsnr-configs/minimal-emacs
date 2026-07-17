---
title: "Multi-Cursor Editing"
category: "Precision Editing"
status: "Working"
parity: "Alt+Click (arbitrary), Ctrl+D (next occurrence), Ctrl+Shift+L (all occurrences), Alt+Shift+Down (column selection)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Multi-Cursor Editing</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Precision Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Alt+Click (arbitrary), Ctrl+D (next occurrence), Ctrl+Shift+L (all
            occurrences), Alt+Shift+Down (column selection)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<span style="color: var(--text-dim); font-size: 12px">N/A (Local buffer manipulation)</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>iedit<span class="route-arrow">→</span>evil-multiedit<span class="route-arrow">→</span>evil-mc</code>
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
<td><kbd>Alt+Click</kbd> to place cursors anywhere</td>
<td>
<kbd>C-S-&lt;mouse-1&gt;</kbd>
                        (<code>evil-mc-make-cursor-here</code>).
                      </td>
</tr>
<tr>
<td><kbd>Ctrl+D</kbd> to select next occurrence</td>
<td>
<kbd>M-d</kbd>
                        (<code>evil-multiedit-match-symbol-and-next</code>).
                      </td>
</tr>
<tr>
<td><kbd>Ctrl+Shift+L</kbd> to select all occurrences</td>
<td>
<kbd>M-D</kbd> (<code>evil-multiedit-match-all</code>).
                      </td>
</tr>
<tr>
<td><kbd>Alt+Shift+Down</kbd> for column selection</td>
<td>
<kbd>C-S-&lt;down&gt;</kbd> in visual state
                        (<code>evil-mc-make-cursor-in-next-line</code>).
                      </td>
</tr>
<tr>
<td>Type/delete simultaneously at all cursors</td>
<td>
                        Native behavior of both <code>iedit</code> and
                        <code>evil-mc</code>.
                      </td>
</tr>
<tr>
<td>
                        Execute <code>ciw</code> or <code>daw</code> at all
                        cursors
                      </td>
<td>
                        Natively supported by <code>evil-mc</code>'s fake cursor
                        engine.
                      </td>
</tr>
<tr>
<td>
<kbd>Esc</kbd> or <kbd>C-g</kbd> to exit multi-cursor
                      </td>
<td>
<kbd>C-g</kbd> cleanly invokes
                        <code>evil-mc-undo-all-cursors</code> or
                        <code>evil-normal-state</code>.
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
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">evil</div>
<div class="eco-sub">Modal Engine</div>
</div>
</div>
<p class="eco-desc">
<code>evil-mc</code> hooks directly into Evil's command
                    loop, ensuring that macros (<kbd>@</kbd>), registers, and
                    operators function identically to single-cursor editing.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
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
                    The <kbd>C-S-&lt;down&gt;</kbd> and
                    <kbd>C-S-&lt;up&gt;</kbd> bindings are strictly confined to
                    <code>visual</code> state, preventing accidental activation
                    during normal typing.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
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
                    For project-wide symbol renaming, this multi-cursor setup is
                    intentionally bypassed in favor of
                    <code>eglot-rename</code> (<kbd>SPC c r</kbd>), which safely
                    updates cross-file references.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
</div>
<div>
<div class="eco-name">undo-fu</div>
<div class="eco-sub">History Management</div>
</div>
</div>
<p class="eco-desc">
                    Multi-cursor edits are treated as a single atomic operation,
                    allowing an entire multi-line insertion to be undone with a
                    single <kbd>u</kbd> press.
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
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">iedit</div>
<div class="stack-role">Foundation Engine</div>
<div class="stack-desc">
                      GNU ELPA. Provides the core mechanism for highlighting and
                      synchronizing mutations across multiple instances of a
                      symbol or region.
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
<div class="stack-name">evil-multiedit</div>
<div class="stack-role">Symbol-Based Editing</div>
<div class="stack-desc">
                      Wraps <code>iedit</code> in a dedicated Evil state,
                      mapping explicit Vim-mnemonics for rapid cursor
                      accumulation and bulk mutation.
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
<div class="stack-name">evil-mc</div>
<div class="stack-role">Arbitrary Placement</div>
<div class="stack-desc">
                      Creates true, Evil-native fake cursors that flawlessly
                      execute standard Evil motions and operators across all
                      points simultaneously.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">Evil integration hooks</div>
<div class="stack-role">State Management</div>
<div class="stack-desc">
                      Ensures clean exit to <code>evil-normal-state</code> upon
                      aborting multi-cursor operations, preventing modal
                      corruption.
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
<td>Add cursor on click</td>
<td><code>evil-mc-make-cursor-here</code></td>
<td><kbd>C-S-&lt;mouse-1&gt;</kbd></td>
<td>
                        VS Code parity: click anywhere to place an arbitrary
                        cursor.
                      </td>
</tr>
<tr>
<td>Edit lines in rectangle</td>
<td><code>evil-mc-make-cursor-in-next-line</code></td>
<td><kbd>C-S-&lt;down&gt;</kbd> (visual)</td>
<td>
                        Adds a cursor to each line in the active visual
                        rectangle.
                      </td>
</tr>
<tr>
<td>Mark next occurrence</td>
<td><code>evil-multiedit-match-symbol-and-next</code></td>
<td><kbd>M-d</kbd></td>
<td>
                        Evil-native: adds the next occurrence of the current
                        symbol.
                      </td>
</tr>
<tr>
<td>Mark all occurrences</td>
<td><code>evil-multiedit-match-all</code></td>
<td><kbd>M-D</kbd></td>
<td>Evil-native: adds all occurrences in the buffer.</td>
</tr>
<tr>
<td>Exit multi-cursor mode</td>
<td><code>evil-mc-undo-all-cursors</code></td>
<td><kbd>C-g</kbd> or <kbd>RET</kbd></td>
<td>Exits multi-cursor mode, leaving a single cursor.</td>
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
<span class="fname">init-multi-cursor.el</span>
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
 ;; 1. IEDIT (Foundation Engine)
 ;; ==========================================
 (use-package iedit
   :defer t
   :commands (iedit-mode iedit-rectangle-mode)
   :custom
   ;; Prevent iedit from accidentally matching substrings of larger words.
   (iedit-match-subword t))
 ;; ==========================================
 ;; 2. EVIL MULTIEDIT (Symbol-Based Editing)
 ;; ==========================================
 (use-package evil-multiedit
   :defer t
   :after (evil iedit)
   :commands (evil-multiedit-match-symbol-and-next
              evil-multiedit-match-all
              evil-multiedit-toggle-or-restrict-region)
   :custom-face
   ;; Tokyo Night synergy: Distinct background for active multi-cursor regions.
   (evil-multiedit-match-face ((t (:background "#bb9af7" :foreground "#1a1b26" :weight bold))))
   :config
   ;; Register default Evil keybindings (e.g., M-d for next, M-D for all).
   (evil-multiedit-default-keybinds)
   ;; Ensure clean exit to normal state when multi-edit is aborted.
   (add-hook 'evil-multiedit-exit-hook #'evil-normal-state))
 ;; ==========================================
 ;; 3. EVIL MC (Arbitrary &amp; Rectangular Placement)
 ;; ==========================================
 (use-package evil-mc
   :defer t
   :after evil
   :commands (evil-mc-make-and-goto-next-match
              evil-mc-make-and-goto-prev-match
              evil-mc-make-all-cursors
              evil-mc-undo-all-cursors)
   :custom
   ;; Disable the default cursor blink to prevent visual distraction.
   (evil-mc-cursor-default-state 'bar)
   :config
   (global-evil-mc-mode 1)
   ;; VS Code Parity: Allow Ctrl+Shift+Click to place arbitrary cursors.
   (global-set-key (kbd "C-S-&lt;mouse-1&gt;") #'evil-mc-make-cursor-here)
   ;; Rectangular multi-cursor (VS Code Alt+Shift+Down parity).
   (general-define-key
    :states 'visual
    "C-S-&lt;down&gt;" #'evil-mc-make-cursor-in-next-line
    "C-S-&lt;up&gt;" #'evil-mc-make-cursor-in-prev-line)
   ;; Safe exit: C-g terminates multi-cursors and returns to a single cursor.
   (define-key evil-mc-key-map (kbd "C-g") #'evil-mc-undo-all-cursors))</code></pre>
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
<h4>✓ evil-mc + evil-multiedit · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Evil Operator Support</span>
<span class="val">Flawless. Commands like <code>daw</code>,
                        <code>ciw</code> execute perfectly across all
                        cursors.</span>
</div>
<div class="vs-row">
<span class="lab">Separation of Concerns</span>
<span class="val"><code>evil-multiedit</code> handles symbol matching;
                        <code>evil-mc</code> handles arbitrary placement.</span>
</div>
<div class="vs-row">
<span class="lab">Undo History</span>
<span class="val"><code>evil-mc</code> groups multi-cursor edits into a
                        single, clean undo step natively.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol Compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate for
                        project-wide operations.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ multiple-cursors · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Evil Operator Support</span>
<span class="val">Fragile. Requires complex advice to trick it into
                        respecting Evil's modal grammar.</span>
</div>
<div class="vs-row">
<span class="lab">Separation of Concerns</span>
<span class="val">Attempts to do both but fails at symbol matching and
                        struggles with Evil integration.</span>
</div>
<div class="vs-row">
<span class="lab">Undo History</span>
<span class="val">Frequently fragments the undo tree, making
                        <kbd>u</kbd> behave erratically.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol Compliance</span>
<span class="val">N/A</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Native Rectangle Synergy</div>
<p class="desc">
<code>evil-mc</code> integrates flawlessly with Emacs 31's
                    refined rectangle mark (<kbd>C-x SPC</kbd>), allowing a
                    drawn rectangle to be instantly converted to multiple
                    cursors via visual state bindings.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Evil State Guards</div>
<p class="desc">
                    The <code>evil-multiedit-exit-hook</code> and
                    <code>evil-mc-key-map</code> explicitly restore
                    <code>evil-normal-state</code>, preventing the "stuck in
                    insert state" bug that plagued older vanilla Emacs
                    configurations.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Subword Matching</div>
<p class="desc">
<code>iedit-match-subword</code> is enabled by default,
                    ensuring that editing <code>myVariable</code> does not
                    accidentally match <code>myVariableName</code> unless
                    intended, providing surgical precision.
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
<td>Cursors Disappear or Behave Erratically</td>
<td>
                        Ensure <code>iedit</code> and <code>evil-mc</code> are
                        not activated simultaneously in the same buffer. Use
                        <code>evil-multiedit</code> for symbol-based tasks and
                        <code>evil-mc</code> for arbitrary/rectangular tasks. If
                        corrupted, press <kbd>C-g</kbd> twice to forcefully
                        reset the buffer state.
                      </td>
</tr>
<tr>
<td>Mouse Click Adds Cursor but Doesn't Type</td>
<td>
                        Verify that <code>global-evil-mc-mode</code> is active.
                        The <code>global-set-key</code> for
                        <kbd>C-S-&lt;mouse-1&gt;</kbd> relies on the minor mode
                        being globally enabled to intercept and process the fake
                        cursor creation.
                      </td>
</tr>
<tr>
<td>Evil State Conflicts on Exit</td>
<td>
                        If stuck in <code>insert-state</code> after pressing
                        <kbd>C-g</kbd>, the exit hooks may not have fired. This
                        is resolved by the explicit
                        <code>(add-hook 'evil-multiedit-exit-hook
                          #'evil-normal-state)</code>
                        and
                        <code>(define-key evil-mc-key-map (kbd "C-g")
                          #'evil-mc-undo-all-cursors)</code>
                        guards in the configuration.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

