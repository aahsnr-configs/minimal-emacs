// Desktop Sidebar Toggle
const sb = document.getElementById("sb");
document.getElementById("sbToggle").addEventListener("click", () => {
  sb.classList.toggle("collapsed");
});

// Mobile Menu Handling
const mobileMenuBtn = document.getElementById("mobileMenuBtn");
const overlay = document.getElementById("overlay");

mobileMenuBtn.addEventListener("click", () => {
  sb.classList.add("mobile-open");
  overlay.classList.add("active");
});

overlay.addEventListener("click", () => {
  sb.classList.remove("mobile-open");
  overlay.classList.remove("active");
});

// Accordion Logic (with ARIA state management)
document.querySelectorAll(".acc-head").forEach((head) => {
  head.addEventListener("click", () => {
    const body = head.nextElementSibling;
    const isExpanded = head.getAttribute("aria-expanded") === "true";
    head.classList.toggle("open");
    body.classList.toggle("open");
    head.setAttribute("aria-expanded", !isExpanded);
  });
});

// Focus Mode (Hides UI, doesn't force accordion states)
const fb = document.getElementById("focusBtn");
function toggleFocus() {
  document.body.classList.toggle("focus");
}
fb.addEventListener("click", toggleFocus);
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && document.body.classList.contains("focus")) {
    toggleFocus();
  }
});

// Robust Clipboard Copy (Iframe safe fallback)
function copyCode(btn) {
  const codeEl = btn.closest(".code-win").querySelector("code");
  const text = codeEl.innerText;

  // Modern Clipboard API preferred, fallback to execCommand for non-HTTPS/file://
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard
      .writeText(text)
      .then(() => {
        triggerCopySuccess(btn);
      })
      .catch((err) => {
        console.error("Clipboard API failed, falling back", err);
        fallbackCopy(text, btn);
      });
  } else {
    fallbackCopy(text, btn);
  }
}

function fallbackCopy(text, btn) {
  const textArea = document.createElement("textarea");
  textArea.value = text;
  textArea.style.position = "fixed";
  textArea.style.top = "0";
  textArea.style.left = "0";
  textArea.style.width = "2em";
  textArea.style.height = "2em";
  textArea.style.padding = "0";
  textArea.style.border = "none";
  textArea.style.outline = "none";
  textArea.style.boxShadow = "none";
  textArea.style.background = "transparent";
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
  } catch (err) {
    console.error("Fallback: Oops, unable to copy", err);
  }
  document.body.removeChild(textArea);
}

function triggerCopySuccess(btn) {
  const originalHTML = btn.innerHTML;
  btn.innerHTML = `<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><polyline points="20 6 9 17 4 12"></polyline></svg> Copied`;
  btn.classList.add("copied");
  setTimeout(() => {
    btn.innerHTML = originalHTML;
    btn.classList.remove("copied");
  }, 1800);
}