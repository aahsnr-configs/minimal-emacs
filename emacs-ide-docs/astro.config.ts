import { defineConfig } from "astro/config";
import mdx from "@astrojs/mdx";
import cloudflare from "@astrojs/cloudflare";

export default defineConfig({
  output: "static",
  adapter: cloudflare(),
  integrations: [mdx()],

  experimental: {
    fonts: [
      {
        // Astro 6 built-in Fonts API — replaces the jsdelivr CDN <link>
        // from the tactical HTML. Self-hosts JetBrains Mono with generated
        // subsets and fallbacks.
        provider: "fontsource",
        name: "JetBrains Mono",
        cssVariable: "--font-jetbrains-mono",
        weights: [400, 500, 600, 700],
        styles: ["normal", "italic"],
      },
    ],
  },

  markdown: {
    shikiConfig: {
      // Native Shiki — uses the tokyo-night theme to match the tactical HTML
      theme: "tokyo-night",
      wrap: true,
      // emacs-lisp is a built-in Shiki language (TextMate grammar from VS Code)
      // It correctly handles declare, interactive, quote/backtick/splice forms,
      // and defun/defvar/lambda keyword-only-at-head rules.
      langs: ["emacs-lisp", "bash", "json", "yaml", "markdown"],
    },
  },
});
