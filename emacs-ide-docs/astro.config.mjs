import { defineConfig } from "astro/config";

// Astro 5.x Configuration
// MDX integration is intentionally REMOVED.
// Standard Markdown (.md) is mathematically correct for Elisp-heavy documentation
// because it uses `remark` and treats fenced code blocks as literal raw text,
// completely bypassing the JSX parser that chokes on `<escape>` and `<mouse-1>`.
export default defineConfig({
  // Enforce strict trailing slashes for SSG consistency
  trailingSlash: "always",

  // Astro includes built-in support for standard Markdown files [[42]].
  // Astro provides built-in support for Shiki and Prism for syntax highlighting [[33]].
  markdown: {
    shikiConfig: {
      // Use a theme that closely matches Tokyo Night for code blocks
      theme: "tokyo-night",
      wrap: true,
    },
  },
});
