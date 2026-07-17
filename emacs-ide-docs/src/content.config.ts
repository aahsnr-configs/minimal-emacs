import { defineCollection, z } from "astro:content";
import { glob } from "astro/loaders";

// Zod schema imported from astro:content in Astro 6.x
// (NOT astro:schema or astro:content — those were removed)

const features = defineCollection({
  loader: glob({ pattern: "**/*.{md,mdx}", base: "./src/content/features" }),
  schema: z.object({
    title: z.string(),
    category: z.string(),
    slug: z.string(),
    status: z.enum(["working", "partial", "planned"]).default("working"),

    // VS Code parity summary
    vscode_parity: z.array(z.string()).default([]),

    // LSP protocol methods this feature uses
    lsp_methods: z.array(z.string()).default([]),

    // Routing chain (e.g., "eglot → capf → cape → corfu")
    routing: z.string().default(""),

    // Implementation stack cards
    stack: z
      .array(
        z.object({
          name: z.string(),
          role: z.string(),
          desc: z.string(),
          color: z
            .enum([
              "blue",
              "purple",
              "cyan",
              "green",
              "red",
              "yellow",
              "orange",
            ])
            .default("blue"),
        }),
      )
      .default([]),

    // Ecosystem integration cards
    ecosystem: z
      .array(
        z.object({
          name: z.string(),
          sub: z.string(),
          desc: z.string(),
          color: z
            .enum([
              "blue",
              "purple",
              "cyan",
              "green",
              "red",
              "yellow",
              "orange",
            ])
            .default("blue"),
        }),
      )
      .default([]),

    // Command table rows
    commands: z
      .array(
        z.object({
          action: z.string(),
          cmd: z.string(),
          key: z.string(),
          notes: z.string().optional().default(""),
        }),
      )
      .default([]),

    // Emacs 31 enhancement cards
    enhancements: z
      .array(
        z.object({
          title: z.string(),
          desc: z.string(),
          color: z.enum(["g", "p", "y"]).default("g"),
        }),
      )
      .default([]),

    // Parity matrix (VS Code ↔ Emacs behavior table)
    parity_matrix: z
      .array(
        z.object({
          vscode: z.string(),
          emacs: z.string(),
        }),
      )
      .default([]),

    // Configuration code filename
    config_filename: z.string().default("init.el"),

    // Rejected alternative
    rejected_alternative: z
      .object({
        name: z.string(),
        reasons: z.array(
          z.object({
            label: z.string(),
            value: z.string(),
          }),
        ),
      })
      .optional(),
  }),
});

export const collections = { features };
