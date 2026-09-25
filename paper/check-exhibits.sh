#!/usr/bin/env bash
# Verify this paper directory's exhibits/ folder matches what the paper references.
#   ./check-exhibits.sh          check the research-vs paper
#   ./check-exhibits.sh --sync   also refresh stale/missing files from ../Results
# Reports:
#   MISSING  referenced by the paper but not in exhibits/  -> paper will not build
#   ORPHAN   in exhibits/ but referenced by nothing        -> clutter, safe to delete
#   STALE    differs from the copy in ../Results           -> paper shows old numbers
# Exit status is 1 if anything is reported.
set -uo pipefail

cd "$(dirname "$0")"
RESULTS="../Results"
sync=0
[ "${1:-}" = "--sync" ] && sync=1
status=0

paper=.
  echo "== research-vs"
  used=$(mktemp); have=$(mktemp)

  # Strip LaTeX comments, then collect exhibit paths.
  sed -E 's/(^|[^\\])%.*/\1/' "$paper"/research_vs.tex "$paper"/sections/*.tex \
    | grep -oE '(\\exhibitspath|exhibits)/[^}]*' \
    | sed -E 's#^(\\exhibitspath|exhibits)/##' \
    | while read -r r; do case "$r" in *.tex|*.pdf) echo "$r";; *) echo "$r.pdf";; esac; done \
    | sort -u > "$used"
  find "$paper/exhibits" -type f 2>/dev/null | sed "s|^$paper/exhibits/||" | sort > "$have"

  while read -r f; do
    [ -z "$f" ] && continue
    if [ ! -f "$paper/exhibits/$f" ]; then
      if [ "$sync" = 1 ] && [ -f "$RESULTS/$f" ]; then
        mkdir -p "$paper/exhibits/$(dirname "$f")"; cp "$RESULTS/$f" "$paper/exhibits/$f"
        echo "   synced   $f"
      else
        echo "   MISSING  $f"; status=1
      fi
    elif [ -f "$RESULTS/$f" ] && ! cmp -s "$paper/exhibits/$f" "$RESULTS/$f"; then
      if [ "$sync" = 1 ]; then
        cp "$RESULTS/$f" "$paper/exhibits/$f"; echo "   synced   $f"
      else
        echo "   STALE    $f"; status=1
      fi
    fi
  done < "$used"

  comm -13 "$used" "$have" | while read -r f; do
    [ -n "$f" ] && echo "   ORPHAN   $f"
  done
  comm -13 "$used" "$have" | grep -q . && status=1

  echo "   $(wc -l < "$used") referenced, $(find "$paper/exhibits" -type f | wc -l) present"
  rm -f "$used" "$have"
[ "$status" = 0 ] && echo "OK: research-vs is self-contained and in sync with $RESULTS"
exit $status
