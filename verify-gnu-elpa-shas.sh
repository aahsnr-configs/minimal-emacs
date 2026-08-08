#!/bin/bash
# Batch verification script for GNU ELPA package commit SHAs
# This queries each upstream GitHub repository for the commit SHA of the specified tag

declare -A packages=(
  ["ace-window"]="https://github.com/abo-abo/ace-window|0.10.0"
  ["avy"]="https://github.com/abo-abo/avy|0.5.0"
  ["bind-key"]="https://github.com/jwiegley/use-package|2.4.1"
  ["bufferlo"]="https://github.com/jamescherti/bufferlo.el|1.2"
  ["cape"]="https://github.com/minad/cape|2.8"
  ["colorful-mode"]="https://github.com/DevelopmentMinds/colorful-mode|1.2.5"
  ["consult"]="https://github.com/minad/consult|3.7"
  ["consult-denote"]="https://github.com/protesilaos/consult-denote|0.5.1"
  ["corfu"]="https://github.com/minad/corfu|2.12"
  ["dape"]="https://github.com/svaante/dape|0.27.1"
  ["denote"]="https://github.com/protesilaos/denote|4.2.3"
  ["denote-journal"]="https://github.com/protesilaos/denote-journal|0.3.0"
  ["denote-menu"]="https://github.com/mpkarylov/denote-menu|1.4.0"
  ["denote-org"]="https://github.com/protesilaos/denote-org|0.3.0"
  ["denote-review"]="https://github.com/protesilaos/denote-review|1.0.7"
  ["denote-sequence"]="https://github.com/protesilaos/denote-sequence|0.3.3"
  ["denote-silo"]="https://github.com/protesilaos/denote-silo|0.3.2"
  ["diff-hl"]="https://github.com/dgutov/diff-hl|1.10.0"
  ["embark"]="https://github.com/oantolin/embark|1.2"
  ["expreg"]="https://github.com/casouri/expreg|1.4.1"
  ["gcmh"]="https://github.com/emacsmirror/gcmh|0.2.1"
  ["indent-bars"]="https://github.com/jdtsmith/indent-bars|1.0.0"
  ["jinx"]="https://github.com/minad/jinx|2.9"
  ["marginalia"]="https://github.com/minad/marginalia|2.11"
  ["orderless"]="https://github.com/oantolin/orderless|1.7"
  ["org-modern"]="https://github.com/minad/org-modern|1.15"
  ["org-transclusion"]="https://github.com/nobiot/org-transclusion|1.4.0"
  ["popper"]="https://github.com/karthink/popper|0.4.8"
  ["posframe"]="https://github.com/tumashu/posframe|1.5.2"
  ["transient"]="https://github.com/magit/transient|0.13.7"
  ["use-package"]="https://github.com/jwiegley/use-package|2.4.6"
  ["vertico"]="https://github.com/minad/vertico|2.12"
  ["yasnippet"]="https://github.com/joaotavora/yasnippet|0.14.3"
)

echo "| Package | Version | Full 40-Char SHA |"
echo "| :--- | :--- | :--- |"

for pkg in "${!packages[@]}"; do
  IFS='|' read -r repo tag <<<"${packages[$pkg]}"

  # Extract the commit SHA for the tag using GitHub API
  sha=$(curl -s "https://api.github.com/repos${repo#https://github.com}/git/refs/tags/$tag" |
    grep -o '"sha": "[a-f0-9]\{40\}"' | head -1 | cut -d'"' -f4)

  if [ -z "$sha" ]; then
    # Try alternative: query the tag page directly
    sha=$(curl -s "$repo/releases/tag/$tag" |
      grep -o 'commit/[a-f0-9]\{40\}' | head -1 | cut -d'/' -f2)
  fi

  if [ -n "$sha" ]; then
    echo "| \`$pkg\` | \`$tag\` | \`$sha\` |"
  else
    echo "| \`$pkg\` | \`$tag\` | **TAG NOT FOUND** |"
  fi
done | sort
