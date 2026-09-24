#!/usr/bin/env bash
# Build the research-vs paper.
# Run: ./build.sh
# Inputs: risk_vs.tex and its local dependencies in this directory
# Outputs: risk_vs.pdf and exhibit-number comments
set -euo pipefail

cd "$(dirname "$0")"
ROOT="$PWD"

# Each paper directory holds its own zimstyle.sty and bib_<paper>.bib, so no
# search path is needed. A relative TEXINPUTS ("../latex-common:") would work
# here -- only an ABSOLUTE one breaks, because kpathsea cannot parse the comma
# in "Live,Risk-vs". Self-contained dirs are used anyway: cmd+S runs LaTeX
# Workshop's own recipe, not this script, and it inherits no TEXINPUTS.

build_one() {
  local dir="$1" main="$2"
  cd "$ROOT/$dir"

  find . -maxdepth 1 -type f -name '*.synctex(busy)' -delete

  # this rm may cause vscode to stop watching
  # rm -f "$main.pdf"

  pdflatex -interaction=nonstopmode -halt-on-error -synctex=1 "$main.tex"
  biber "$main"
  pdflatex -interaction=nonstopmode -halt-on-error -synctex=1 "$main.tex"
  pdflatex -interaction=nonstopmode -halt-on-error -synctex=1 "$main.tex"

  # Nudge VS Code Remote's file watcher: publish the PDF through a rename event.
  local tmp_pdf
  tmp_pdf="$(mktemp "$main.pdf.XXXXXX")"
  cp "$main.pdf" "$tmp_pdf"
  mv -f "$tmp_pdf" "$main.pdf"
  touch "$main.pdf"
}

build_one . risk_vs
python3 "$ROOT/update_exhibit_numbers.py" research-vs --from-aux
