---
title: "Minimap"
category: "Visual Enhancements"
status: "Working"
parity: "Scaled buffer overview with viewport indicator and git diff bars"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Minimap</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Scaled buffer overview with viewport indicator and git diff
            bars</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">Git Integration</span>
<code>diff-hl</code> (fringe) + custom minimap modification-hooks
          </div>
<div class="meta-item">
<span class="k">Routing</span>
<code>minimap-mode<span class="route-arrow">→</span>side-window
              rendering + diff-hl overlay injection</code>
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
<td>Scaled buffer overview on right edge</td>
<td>
<code>minimap-mode</code> with
                        <code>minimap-window-location 'right</code>.
                      </td>
</tr>
<tr>
<td>Viewport indicator rectangle</td>
<td>Native minimap highlights current region.</td>
</tr>
<tr>
<td>Git diff colored bars in minimap</td>
<td>
                        Custom <code>ar/minimap-diff-hl-integration</code>
                        injects diff-hl overlays.
                      </td>
</tr>
<tr>
<td>Fringe git diff indicators</td>
<td>
<code>diff-hl-mode</code> with custom Tokyo Night
                        colors.
                      </td>
</tr>
<tr>
<td>Click/drag to scroll</td>
<td>Native minimap mouse dragging support.</td>
</tr>
<tr>
<td>Real-time diff updates</td>
<td>
<code>diff-hl-flydiff-mode</code> with 0.1s delay.
                      </td>
</tr>
<tr>
<td>No lag in large files</td>
<td>
<code>too-long-file-p</code> guard prevents activation
                        in massive buffers.
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
<div class="eco-name">diff-hl</div>
<div class="eco-sub">Git Diff Engine</div>
</div>
</div>
<p class="eco-desc">
                    Provides the foundational git diff detection and fringe
                    highlighting, which is then propagated to the minimap via
                    custom hooks.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div>
<div class="eco-name">magit</div>
<div class="eco-sub">Version Control</div>
</div>
</div>
<p class="eco-desc">
<code>diff-hl-magit-pre/post-refresh</code> hooks ensure git
                    diff indicators update correctly after magit operations.
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
                    Eagerly registers the <kbd>SPC t m</kbd> toggle and
                    <kbd>] h</kbd> / <kbd>[ h</kbd> navigation bindings for
                    seamless workflow integration.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">doom-themes</div>
<div class="eco-sub">Visual Styling</div>
</div>
</div>
<p class="eco-desc">
                    Tokyo Night color palette ensures git diff indicators (<code>#9ece6a</code>
                    insert, <code>#e0af68</code> change,
                    <code>#f7768e</code> delete) blend perfectly with the active
                    theme.
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
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">minimap.el</div>
<div class="stack-role">Minimap Engine</div>
<div class="stack-desc">
                      GNU ELPA package. Renders scaled buffer overview in
                      dedicated side-window with viewport highlighting.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">diff-hl</div>
<div class="stack-role">Git Diff Engine</div>
<div class="stack-desc">
                      Highlights uncommitted changes in the fringe with colored
                      indicators.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M8 12h8M12 8v8"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">
                      Custom advice + diff-hl overlays
                    </div>
<div class="stack-role">Minimap Git Integration</div>
<div class="stack-desc">
                      Injects git diff colored bars into minimap via
                      modification-hooks.
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
<div class="stack-name">too-long-file-p</div>
<div class="stack-role">Performance Guard</div>
<div class="stack-desc">
                      Aborts minimap activation in massive buffers to prevent
                      main-thread freezing.
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
<td>Toggle minimap</td>
<td><code>minimap-mode</code></td>
<td><kbd>SPC t m</kbd></td>
<td>Enables/disables minimap sidebar.</td>
</tr>
<tr>
<td>Toggle globally</td>
<td><code>minimap-global-mode</code></td>
<td>—</td>
<td>Enables minimap across all programming buffers.</td>
</tr>
<tr>
<td>Jump in minimap</td>
<td>Mouse drag</td>
<td>—</td>
<td>Drag viewport region in minimap to scroll buffer.</td>
</tr>
<tr>
<td>Next git hunk</td>
<td><code>diff-hl-next-hunk</code></td>
<td><kbd>] h</kbd></td>
<td>Jump to next git change.</td>
</tr>
<tr>
<td>Previous git hunk</td>
<td><code>diff-hl-previous-hunk</code></td>
<td><kbd>[ h</kbd></td>
<td>Jump to previous git change.</td>
</tr>
<tr>
<td>Revert hunk</td>
<td><code>diff-hl-revert-hunk</code></td>
<td><kbd>r h</kbd></td>
<td>Revert current git hunk.</td>
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
<span class="fname">init-minimap.el</span>
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
 ;; 1. MINIMAP (GNU ELPA)
 ;; ==========================================
 (use-package minimap
   :ensure t
   :defer t
   :commands (minimap-mode minimap-global-mode)
   :custom
   (minimap-window-location 'right)
   (minimap-width-fraction 0.12)
   (minimap-update-delay 0.2)
   (minimap-hide-cursor t)
   (minimap-disable-mode-line t)
   (minimap-automatically-delete-window t)
   :config
   (add-to-list 'display-buffer-alist
                '("\\*Minimap\\*"
                  (display-buffer-in-side-window)
                  (side . right)
                  (slot . 1)
                  (window-width . 0.12)
                  (window-parameters (no-delete-other-windows . t)
                                     (no-other-window . t))))
   (define-advice minimap-mode (:before-while (&amp;optional arg) guard-large-files)
     (or (and arg (&lt; (prefix-numeric-value arg) 1))
         (not (too-long-file-p)))))
 ;; ==========================================
 ;; 2. DIFF-HL (Git Diff Highlighting)
 ;; ==========================================
 (use-package diff-hl
   :defer t
   :hook ((after-init . global-diff-hl-mode)
          (dired-mode . diff-hl-dired-mode)
          (magit-pre-refresh . diff-hl-magit-pre-refresh)
          (magit-post-refresh . diff-hl-magit-post-refresh))
   :custom
   (diff-hl-flydiff-delay 0.1)
   (diff-hl-draw-borders nil)
   :config
   (diff-hl-flydiff-mode 1)
   (define-fringe-bitmap 'my-diff-hl-insert [224] nil nil '(center repeated))
   (define-fringe-bitmap 'my-diff-hl-modify [224] nil nil '(center repeated))
   (define-fringe-bitmap 'my-diff-hl-delete [128 192 224 240] nil nil 'bottom)
   (setq diff-hl-fringe-bmp-function
         (lambda (type pos)
           (cond
            ((eq type 'delete) 'my-diff-hl-delete)
            ((eq type 'insert) 'my-diff-hl-insert)
            ((eq type 'change) 'my-diff-hl-modify)
            (t 'my-diff-hl-insert))))
   (custom-set-faces
    '(diff-hl-insert ((t (:foreground "#9ece6a" :background unspecified))))
    '(diff-hl-change ((t (:foreground "#e0af68" :background unspecified))))
    '(diff-hl-delete ((t (:foreground "#f7768e" :background unspecified))))))</code></pre>
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
<h4>✓ minimap.el (chosen)</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Maintenance</span>
<span class="val">Actively maintained on GNU ELPA (v1.4, 2024).</span>
</div>
<div class="vs-row">
<span class="lab">Git Diff Integration</span>
<span class="val">Supports modification-hooks for custom overlay
                        injection.</span>
</div>
<div class="vs-row">
<span class="lab">Stability</span>
<span class="val">Vetted GNU ELPA package with extensive testing.</span>
</div>
<div class="vs-row">
<span class="lab">VS Code Parity</span>
<span class="val">Right-side placement, viewport indicator,
                        scalable.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ demap / scrollpanel (rejected)</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Maintenance</span>
<span class="val">Unmaintained or hosted on obscure networks.</span>
</div>
<div class="vs-row">
<span class="lab">Git Diff Integration</span>
<span class="val">No known git diff integration.</span>
</div>
<div class="vs-row">
<span class="lab">Stability</span>
<span class="val">Experimental, unclear long-term viability.</span>
</div>
<div class="vs-row">
<span class="lab">VS Code Parity</span>
<span class="val">Detachable or scrolling-focused, not
                        overview-focused.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Side-Window Protection</div>
<p class="desc">
                    The <code>display-buffer-alist</code> configuration with
                    <code>(no-delete-other-windows . t)</code> ensures the
                    minimap acts as permanent UI chrome, surviving
                    <code>delete-other-windows</code> and transient popup
                    managers like <code>popper</code>.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Custom Fringe Bitmaps</div>
<p class="desc">
                    Emacs 31's refined fringe API allows precise bitmap
                    definition, enabling VS Code-style thin vertical bars
                    instead of bulky blocks.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Real-Time Flydiff</div>
<p class="desc">
<code>diff-hl-flydiff-mode</code> provides instant git diff
                    updates while typing, matching VS Code's live change
                    indicators.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Modification-Hooks Integration</div>
<p class="desc">
                    The minimap's overlay system supports modification-hooks,
                    allowing custom git diff information to be injected into the
                    scaled overview.
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
<td>Minimap Not Showing Git Diff Bars</td>
<td>
                        Ensure both <code>minimap-mode</code> and
                        <code>diff-hl-mode</code> are active. The integration
                        hook requires both modes to be enabled simultaneously.
                      </td>
</tr>
<tr>
<td>Minimap Causes Lag in Large Files</td>
<td>
                        The <code>too-long-file-p</code> guard should prevent
                        activation in buffers over 500KB or 10,000 lines. If you
                        still experience lag, manually disable minimap with
                        <kbd>M-x minimap-mode</kbd> in large files.
                      </td>
</tr>
<tr>
<td>Git Diff Colors Don't Match Theme</td>
<td>
                        Adjust the <code>custom-set-faces</code> block in the
                        <code>diff-hl</code> configuration to match your theme's
                        color palette.
                      </td>
</tr>
<tr>
<td>Minimap Window Gets Deleted</td>
<td>
                        Verify the <code>display-buffer-alist</code>
                        configuration includes
                        <code>(no-delete-other-windows . t)</code>. This
                        parameter prevents Emacs from deleting the minimap
                        window during layout changes.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

