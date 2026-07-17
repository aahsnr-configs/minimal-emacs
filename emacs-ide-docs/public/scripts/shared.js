/**
 * shared.js — Client-side interactivity for Emacs IDE Docs
 *
 * Carries over from the tactical layer with these behaviors:
 * - Desktop sidebar toggle
 * - Mobile menu handling
 * - Accordion logic (with ARIA state management)
 * - Focus mode (hides UI, ESC to exit)
 * - Clipboard-API-first copyCode() — navigator.clipboard.writeText()
 *   is the primary path; textarea/execCommand is ONLY a fallback for
 *   non-secure contexts (execCommand is deprecated per MDN)
 */

(function () {
  "use strict";

  // ---- Desktop Sidebar Toggle ----
  const sb = document.getElementById("sb");
  const sbToggle = document.getElementById("sbToggle");
  if (sbToggle) {
    sbToggle.addEventListener("click", () => {
      sb.classList.toggle("collapsed");
    });
  }

  // ---- Mobile Menu Handling ----
  const mobileMenuBtn = document.getElementById("mobileMenuBtn");
  const overlay = document.getElementById("overlay");

  if (mobileMenuBtn && overlay) {
    mobileMenuBtn.addEventListener("click", () => {
      sb.classList.add("mobile-open");
      overlay.classList.add("active");
    });

    overlay.addEventListener("click", () => {
      sb.classList.remove("mobile-open");
      overlay.classList.remove("active");
    });
  }

  // ---- Accordion Logic (with ARIA state management) ----
  document.querySelectorAll(".acc-head").forEach((head) => {
    head.addEventListener("click", () => {
      const body = head.nextElementSibling;
      const isExpanded = head.getAttribute("aria-expanded") === "true";
      head.classList.toggle("open");
      body.classList.toggle("open");
      head.setAttribute("aria-expanded", String(!isExpanded));
    });
  });

  // ---- Focus Mode (Hides UI, doesn't force accordion states) ----
  const focusBtn = document.getElementById("focusBtn");

  function toggleFocus() {
    document.body.classList.toggle("focus");
  }

  if (focusBtn) {
    focusBtn.addEventListener("click", toggleFocus);
  }

  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && document.body.classList.contains("focus")) {
      toggleFocus();
    }
  });

  // ---- Clipboard-API-first copyCode ----
  // navigator.clipboard.writeText() is the primary path.
  // The textarea/execCommand fallback is ONLY for non-secure contexts
  // (e.g., file:// protocol or HTTP without localhost).
  // document.execCommand is deprecated per MDN and being removed by browsers.
  window.copyCode = async function copyCode(btn) {
    const codeWin = btn.closest(".code-win");
    if (!codeWin) return;

    // Try to get the code text from the data attribute first (set by CodeWindow component)
    let text = btn.getAttribute("data-code");

    // Fallback: extract from the Shiki-rendered <pre> element
    if (!text) {
      const codeEl =
        codeWin.querySelector("pre code") ||
        codeWin.querySelector(".shiki code");
      if (codeEl) {
        text = codeEl.innerText;
      }
    }

    if (!text) return;

    try {
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(text);
        triggerCopySuccess(btn);
        return;
      }
      throw new Error("Clipboard API unavailable");
    } catch (err) {
      // Legacy fallback for non-secure contexts only
      const textArea = document.createElement("textarea");
      textArea.value = text;
      textArea.style.cssText =
        "position:fixed;top:0;left:0;width:2em;height:2em;padding:0;border:none;outline:none;box-shadow:none;background:transparent;";
      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();
      try {
        const successful = document.execCommand("copy");
        if (successful) {
          triggerCopySuccess(btn);
        } else {
          console.warn("Fallback copy command was unsuccessful");
        }
      } catch (fallbackErr) {
        console.error("Fallback: unable to copy", fallbackErr);
      }
      document.body.removeChild(textArea);
    }
  };

  function triggerCopySuccess(btn) {
    const originalHTML = btn.innerHTML;
    btn.innerHTML =
      '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><polyline points="20 6 9 17 4 12"></polyline></svg> Copied';
    btn.classList.add("copied");
    setTimeout(() => {
      btn.innerHTML = originalHTML;
      btn.classList.remove("copied");
    }, 1800);
  }
})();
