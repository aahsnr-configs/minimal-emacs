# The Truly Exhaustive Denote Ecosystem (July 2026)

## 1. Core Engine

| Package  | Repository | Description                                                                                           |
| -------- | ---------- | ----------------------------------------------------------------------------------------------------- |
| `denote` | GNU ELPA   | The foundational file-naming and retrieval engine. Currently at version 4.2.0 (released 2026-05-20) . |

---

## 2. Official Extensions (Maintained by Protesilaos Stavrou)

Following the "Great Denote Split" of February 2025 , these are all standalone GNU ELPA packages:

| Package           | Description                                                                          |
| ----------------- | ------------------------------------------------------------------------------------ |
| `denote-org`      | Deep Org-mode integration: dynamic blocks, heading links, subtree splitting .        |
| `denote-journal`  | Calendar integration and daily entry management .                                    |
| `denote-silo`     | Management of isolated, localized `denote-directory` contexts .                      |
| `denote-sequence` | Folgezettel (Zettelkasten branching) alphanumeric sequencing .                       |
| `denote-markdown` | Markdown-specific linking and front-matter parsing .                                 |
| `denote-merge`    | Utilities for safely merging contents between two Denote notes (released Nov 2025) . |
| `consult-denote`  | Glue code integrating Denote with `consult` for live-preview filtering .             |

---

## 3. Third-Party Packages (MELPA, GitHub, Codeberg)

### Navigation, Search & Filtering

| Package         | Author          | Repository | Description                                                                                                        |
| --------------- | --------------- | ---------- | ------------------------------------------------------------------------------------------------------------------ |
| `denote-search` | Lucas Quintana  | GNU ELPA   | Xref-based regex search across all Denote note contents with a navigable results buffer.                           |
| `denote-menu`   | Suliman/namilus | GNU ELPA   | Replaces Dired with a tabulated list buffer for viewing/filtering Denote files by timestamp, title, and keywords . |

### Academic & Bibliographic

| Package                 | Author          | Repository | Description                                                                                                                    |
| ----------------------- | --------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `citar-denote`          | Peter Prevos    | MELPA      | Bridges `citar` bibliography manager with Denote for automatic literature note creation tied to BibTeX keys .                  |
| `denote-citar-sections` | Samuel W. Flint | MELPA      | Universal Sidecar sections specifically for `citar-denote`, displaying formatted abstracts and citation metadata in sidebars . |

### Analytics & Visualization

| Package            | Author                     | Repository | Description                                                                                                             |
| ------------------ | -------------------------- | ---------- | ----------------------------------------------------------------------------------------------------------------------- |
| `denote-explore`   | Peter Prevos               | MELPA      | Statistical dashboards, network graphs (JavaScript/GraphViz/GEXF formats), random walks, and tag distribution metrics . |
| `denote-wordcloud` | Alexander Kuzmin (treflip) | MELPA      | Generates clickable keyword clouds showing word frequencies in a separate buffer .                                      |

### UI & Sidebar Integrations

| Package           | Author            | Repository | Description                                                                                                      |
| ----------------- | ----------------- | ---------- | ---------------------------------------------------------------------------------------------------------------- |
| `denote-refs`     | Akib Azmain Turja | Codeberg   | Automatically injects a visual list of outgoing links and incoming backlinks directly below the front matter .   |
| `denote-sections` | Samuel W. Flint   | MELPA      | Integrates with `universal-sidecar` to render Denote metadata, backlinks, and citations in a dedicated sidebar . |
| `ekg-denote`      | Andrew Hyatt      | GitHub     | Bridges the `ekg` (Emacs Knowledge Graph) SQLite-backed system with Denote's file-naming scheme.                 |

### Agenda & Calendar Integration

| Package         | Author    | Repository | Description                                                                                                                                                                                                                                     |
| --------------- | --------- | ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `denote-agenda` | Community | MELPA      | **NEW FIND** - Simple integration between Denote and Org-Agenda. Aware of `denote-journal` and provides configuration options to surface Denote notes in agenda views . Added to MELPA in February 2025 .                                       |
| `denote-review` | mattof    | Codeberg   | **NEW FIND** - Implements a spaced-repetition review process for Denote notes using a `reviewdate` front matter property. Creates tabulated lists of notes due for review . Currently pending GNU ELPA acceptance (under discussion Jan 2026) . |

### Zettelkasten & Specialized Workflows

| Package                   | Author               | Repository | Description                                                                                                 |
| ------------------------- | -------------------- | ---------- | ----------------------------------------------------------------------------------------------------------- |
| `denote-zettel-interface` | Kristoffer Balintona | GitHub     | Represents the folgezettel index in a `tabulated-list-mode` format for navigating Luhmann-style sequences . |
| `denote-roam`             | BardofSprites        | GitHub     | A bridge package between Denote and `org-roam` for users migrating or maintaining both systems .            |

### Publishing & Export

| Package          | Author           | Repository | Description                                                                                                                                                                            |
| ---------------- | ---------------- | ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `denote-publish` | Vedang Manerikar | GitHub     | Exports Denote files to Markdown with preserved metadata, customizable YAML front matter, and support for Denote-style internal links . Designed for static site generators like Hugo. |

---

## 4. Glue Code Patterns (No Standalone Package)

| Pattern                       | Description                                                                                                       |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| `org-transclusion` + Denote   | Community standard uses `denote-get-path-by-id` inside `org-transclusion` directives to dynamically embed notes . |
| Obsidian Web Clipper + Denote | Workflow hack using Obsidian's web clipper configured to save in Denote's format .                                |
| macOS Safari Capture          | OS-level service using AppleScript to capture Safari selections into Denote files .                               |

---

## Summary: What Was Missing From Previous Documents

The following packages were **not** in your previous verification files:

1. **`denote-agenda`** - Org-Agenda integration (MELPA, Feb 2025)
2. **`denote-citar-sections`** - Universal Sidecar sections for citar-denote (MELPA, Jun 2024)
3. **`denote-review`** - Spaced-repetition review system (Codeberg, pending ELPA)
4. **`denote-zettel-interface`** - Tabulated folgezettel navigation (GitHub)
5. **`denote-roam`** - Bridge between Denote and org-roam (GitHub)
6. **`denote-publish`** - Markdown export with YAML front matter for Hugo/Jekyll (GitHub)

The total count is now **22 distinct packages** (1 core + 7 official extensions + 14 third-party packages), plus 3 documented glue-code patterns.
