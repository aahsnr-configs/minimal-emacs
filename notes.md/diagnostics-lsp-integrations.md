I have ingested the source-of-truth files and the unified protocol. I acknowledge the strict formatting rules, the negative constraints, and the current project state (Version 57). The next pending subsection is **Development Tools: Flycheck & LSP-UI Hardening**. I am locked in and will write zero code until you give the signal.

Below is the deep architectural analysis of the "chasing cursor" diagnostic bug, an evaluation of your proposed alternatives, and the robust native plan to resolve it without fragile advice.

### 1. Diagnosis: Why the Previous Advice was Fragile

The previous approach of stripping `lsp-diagnostics-updated-hook` and `lsp-managed-mode-hook` is a "whack-a-mole" anti-pattern. `lsp-mode` relies on `lsp-diagnostics-updated-hook` for critical internal state synchronization, including resetting modeline diagnostic caches [[10]]. Severing this hook causes the modeline to display stale error counts and breaks downstream integrations that depend on the push stream, even if they don't render inline. It fights against `lsp-mode`'s native state machine rather than working within its boundaries.

### 2. Evaluation of Proposed Alternatives

#### Alternative A: `flymake` + `lsp-ui-sideline`

- **Technical Merit:** `lsp-ui-sideline` renders diagnostics in the margin or at the end of the line [[12]]. This completely eliminates the inline text-reflow "chasing" bug because it doesn't shift buffer geometry or force inline reflows while typing. Furthermore, `flymake` is native to Emacs 31 and fully supported by `lsp-mode`'s `:flymake` provider [[21]].
- **Architectural Verdict:** **REJECTED.** Your `project_operationals.yaml` explicitly mandates `"NO flymake (replaced by flycheck + lsp-ui-sideline)"`. Adopting this would violate your established negative constraints and fragment the IDE stack you have already finalized.

#### Alternative B: On-Demand Childframe (`flycheck-posframe` / `lsp-ui-doc`)

- **Technical Merit:** Floating childframes have **zero main-buffer redisplay cost** [[54]]. The error only renders when the cursor rests on the symbol or via a manual toggle, completely bypassing the push-stream collision.
- **Architectural Verdict:** **VIABLE BUT REGRESSIVE.** This abandons the "always-visible" Error Lens paradigm (`flycheck-annotate-mode`) that you explicitly configured. While it is the most performant approach, it sacrifices the IDE-like inline visibility you desire in favor of a hover-based paradigm.
