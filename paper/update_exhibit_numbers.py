#!/usr/bin/env python3
"""Compile the research-vs paper and annotate each LaTeX float with its PDF number.

Run: python3 update_exhibit_numbers.py [research-vs] [--from-aux]
Inputs: research_vs.tex and its included sources, exhibits,
and bibliography (pdflatex, biber, pdfinfo required).
With --from-aux, read existing AUX files after a successful build; do not compile.
Outputs: EXHIBIT-NUMBER comments in the paper's LaTeX sources; builds are temporary.
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parent
PAPERS = (
    ROOT / "research_vs.tex",
)

FLOAT_TOKEN_RE = re.compile(
    r"\\(?P<action>begin|end)\s*\{(?P<env>figure\*?|table\*?|sidewaysfigure|sidewaystable)\}"
)
INPUT_RE = re.compile(r"\\(?:input|include)\s*\{([^{}]+)\}")
LABEL_RE = re.compile(r"\\label\s*\{([^{}]+)\}")
MANAGED_RE = re.compile(r"^(\s*)%\s*EXHIBIT-NUMBER:\s*(?:Table|Figure)\s+\S+\s*$")
LEGACY_RE = re.compile(r"^(\s*)%\s*(?:Table|Figure)\s+(?:IA\.)?(?:[A-Z]\.)?\d+\s*$")
AUX_LABEL_RE = re.compile(r"\\newlabel\{([^{}@]+)\}\{\{([^{}]+)\}")


class ExhibitError(RuntimeError):
    pass


@dataclass(frozen=True)
class Float:
    path: Path
    line_index: int
    kind: str
    label: str


def uncommented(text: str) -> str:
    """Replace LaTeX comments with spaces while preserving offsets and newlines."""
    output: list[str] = []
    for line in text.splitlines(keepends=True):
        comment_at = None
        for match in re.finditer(r"%", line):
            backslashes = 0
            pos = match.start() - 1
            while pos >= 0 and line[pos] == "\\":
                backslashes += 1
                pos -= 1
            if backslashes % 2 == 0:
                comment_at = match.start()
                break
        if comment_at is None:
            output.append(line)
        else:
            ending = "\n" if line.endswith("\n") else ""
            body_length = len(line) - len(ending)
            output.append(line[:comment_at] + " " * (body_length - comment_at) + ending)
    return "".join(output)


def resolve_input(name: str, project: Path, exhibitspath: str) -> Path:
    candidate = project / name.strip().replace(r"\exhibitspath", exhibitspath)
    if candidate.suffix == "":
        candidate = candidate.with_suffix(".tex")
    return candidate.resolve()


def load_document_sources(main_file: Path) -> tuple[list[Path], dict[Path, str]]:
    ordered: list[Path] = []
    sources: dict[Path, str] = {}
    visiting: set[Path] = set()
    main_text = uncommented(main_file.read_text(encoding="utf-8"))
    exhibit_match = re.search(r"\\newcommand\s*\{\\exhibitspath\}\s*\{([^{}]+)\}", main_text)
    exhibitspath = exhibit_match.group(1) if exhibit_match else r"\exhibitspath"

    def visit(path: Path) -> None:
        path = path.resolve()
        if path in sources:
            return
        if path in visiting:
            raise ExhibitError(f"recursive input involving {path.relative_to(ROOT)}")
        if not path.is_file():
            raise ExhibitError(f"included file does not exist: {path}")
        visiting.add(path)
        text = path.read_text(encoding="utf-8")
        sources[path] = text
        ordered.append(path)
        for match in INPUT_RE.finditer(uncommented(text)):
            visit(resolve_input(match.group(1), main_file.parent, exhibitspath))
        visiting.remove(path)

    visit(main_file)
    return ordered, sources


def inventory_floats(ordered: list[Path], sources: dict[Path, str]) -> list[Float]:
    floats: list[Float] = []
    labels_seen: dict[str, Path] = {}

    for path in ordered:
        clean = uncommented(sources[path])
        stack: list[tuple[str, int, int]] = []
        for token in FLOAT_TOKEN_RE.finditer(clean):
            env = token.group("env")
            if token.group("action") == "begin":
                stack.append((env, token.start(), clean.count("\n", 0, token.start())))
                continue
            if not stack or stack[-1][0] != env:
                raise ExhibitError(f"unbalanced {env} environment in {path.relative_to(ROOT)}")
            _, start, line_index = stack.pop()
            if stack:
                continue

            block = clean[start : token.end()]
            kind = "Figure" if "figure" in env else "Table"
            prefix = "fig:" if kind == "Figure" else "tab:"
            candidates = [label for label in LABEL_RE.findall(block) if label.startswith(prefix)]
            if not candidates:
                raise ExhibitError(
                    f"{path.relative_to(ROOT)}:{line_index + 1}: {kind.lower()} has no {prefix} label"
                )
            label = candidates[0]
            if label in labels_seen:
                other = labels_seen[label].relative_to(ROOT)
                raise ExhibitError(f"duplicate label {label!r} in {other} and {path.relative_to(ROOT)}")
            labels_seen[label] = path
            floats.append(Float(path, line_index, kind, label))

        if stack:
            env, _, line_index = stack[-1]
            raise ExhibitError(
                f"{path.relative_to(ROOT)}:{line_index + 1}: unclosed {env} environment"
            )

    if not floats:
        raise ExhibitError("no figure or table environments found")
    return floats


def run(command: list[str], cwd: Path) -> None:
    result = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        env=os.environ.copy(),
    )
    if result.returncode:
        tail = "\n".join(result.stdout.splitlines()[-40:])
        raise ExhibitError(f"command failed: {' '.join(command)}\n\n{tail}")


def compile_paper(main_file: Path, build_dir: Path) -> Path:
    # Build from the paper directory so relative external assets resolve correctly.
    # All generated build files go to the temporary output directory.
    build_project = main_file.parent
    output_dir = build_dir / "output"
    output_dir.mkdir()

    latex = [
        "pdflatex",
        "-interaction=nonstopmode",
        "-halt-on-error",
        "-file-line-error",
        f"-output-directory={output_dir}",
        main_file.name,
    ]
    run(latex, build_project)
    run(
        [
            "biber",
            f"--input-directory={output_dir}",
            f"--output-directory={output_dir}",
            main_file.stem,
        ],
        build_project,
    )
    run(latex, build_project)
    run(latex, build_project)

    pdf = output_dir / f"{main_file.stem}.pdf"
    aux = output_dir / f"{main_file.stem}.aux"
    if not pdf.is_file() or not aux.is_file():
        raise ExhibitError("LaTeX completed without producing both the PDF and AUX files")
    run(["pdfinfo", str(pdf)], output_dir)
    return aux


def read_numbers(aux: Path) -> dict[str, str]:
    numbers: dict[str, str] = {}
    for label, number in AUX_LABEL_RE.findall(aux.read_text(encoding="utf-8", errors="replace")):
        if label in numbers and numbers[label] != number:
            raise ExhibitError(f"compiled AUX contains conflicting values for {label!r}")
        numbers[label] = number
    return numbers


def update_sources(
    sources: dict[Path, str], floats: list[Float], numbers: dict[str, str]
) -> tuple[dict[Path, str], int]:
    missing = [float_.label for float_ in floats if float_.label not in numbers]
    if missing:
        preview = ", ".join(missing[:8])
        suffix = " ..." if len(missing) > 8 else ""
        raise ExhibitError(f"compiled AUX is missing {len(missing)} exhibit label(s): {preview}{suffix}")

    updated = dict(sources)
    changes = 0
    by_path: dict[Path, list[Float]] = {}
    for float_ in floats:
        by_path.setdefault(float_.path, []).append(float_)

    for path, path_floats in by_path.items():
        lines = updated[path].splitlines(keepends=True)
        for float_ in sorted(path_floats, key=lambda item: item.line_index, reverse=True):
            begin_line = lines[float_.line_index]
            indent = re.match(r"\s*", begin_line).group(0)
            desired = f"{indent}% EXHIBIT-NUMBER: {float_.kind} {numbers[float_.label]}\n"
            previous = float_.line_index - 1
            if previous >= 0 and (MANAGED_RE.match(lines[previous]) or LEGACY_RE.match(lines[previous])):
                if lines[previous] != desired:
                    lines[previous] = desired
                    changes += 1
            else:
                lines.insert(float_.line_index, desired)
                changes += 1
        updated[path] = "".join(lines)
    return updated, changes


def write_atomically(original: dict[Path, str], updated: dict[Path, str]) -> list[Path]:
    changed_paths = [path for path in updated if updated[path] != original[path]]
    for path in changed_paths:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", newline="", dir=path.parent, delete=False
        ) as handle:
            handle.write(updated[path])
            temporary = Path(handle.name)
        os.replace(temporary, path)
    return changed_paths


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("target", nargs="?", default="research-vs", choices=("research-vs",))
    parser.add_argument(
        "--from-aux", action="store_true",
        help="use existing AUX files; caller must first complete a successful build",
    )
    args = parser.parse_args()
    papers = list(PAPERS)
    try:
        prepared = []
        for main_file in papers:
            print(f"Verifying {main_file.relative_to(ROOT)} ...", flush=True)
            ordered, sources = load_document_sources(main_file)
            floats = inventory_floats(ordered, sources)
            if args.from_aux:
                numbers = read_numbers(main_file.with_suffix(".aux"))
            else:
                with tempfile.TemporaryDirectory(prefix=f"{main_file.stem}-exhibit-numbers-") as directory:
                    aux = compile_paper(main_file, Path(directory))
                    numbers = read_numbers(aux)

            updated, changes = update_sources(sources, floats, numbers)

            # Prove idempotence before writing: parsing the proposed files and applying
            # the same transformation must produce no further edits.
            second_floats = inventory_floats(ordered, updated)
            second_updated, second_changes = update_sources(updated, second_floats, numbers)
            if second_changes or second_updated != updated:
                raise ExhibitError("internal idempotence check failed; no files were changed")
            prepared.append((main_file, sources, updated, changes, len(floats)))

        # Validate the paper before changing its sources.
        for main_file, sources, updated, changes, count in prepared:
            changed_paths = write_atomically(sources, updated)
            print(
                f"{main_file.relative_to(ROOT)}: "
                f"Updated {changes} exhibit comment(s) across {len(changed_paths)} file(s); "
                f"{count} exhibits verified."
            )
        return 0
    except (ExhibitError, OSError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
