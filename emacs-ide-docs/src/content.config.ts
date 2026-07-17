import { defineCollection, z } from "astro:content";
import { glob } from "astro/loaders";

// Astro 5.x Content Layer API configuration
// The glob() loader creates entries from directories of files on the filesystem [[8]].
const docs = defineCollection({
  // Load standard Markdown (.md) files to bypass MDX JSX parsing errors with Elisp angle brackets
  loader: glob({ pattern: "**/*.md", base: "./src/content/docs" }),
  schema: z.object({
    title: z.string(),
    category: z.string(),
    status: z.string(),
    parity: z.string(),
  }),
});

export const collections = { docs };
