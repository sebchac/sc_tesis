#!/usr/bin/env python3
"""
Minimal WAF-like build runner for the thesis pipeline.
No external Python dependencies — stdlib only.

Usage:
    python waf.py configure
    python waf.py build [--force] [--from=PATTERN]
    python waf.py clean
"""

import argparse
import importlib.util
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).parent


# ── Context objects ───────────────────────────────────────────────────────────

class ConfigContext:
    def find_program(self, name: str) -> str:
        import shutil
        path = shutil.which(name)
        if path is None:
            print(f"[waf] ERROR: '{name}' not found in PATH", file=sys.stderr)
            sys.exit(1)
        print(f"[waf] found  {name}: {path}")
        return path


class BuildContext:
    def __init__(self):
        self._tasks: list[dict] = []

    def add_task(self, *, name: str, rule: str,
                 source: list[str] | None = None,
                 target: list[str] | None = None,
                 only_if_missing: bool = False,
                 always: bool = False):
        """Register one build task.

        Parameters
        ----------
        name            : Human-readable label shown in log output.
        rule            : Shell command to execute.
        source          : Input files used for up-to-date checks.
        target          : Output files used for up-to-date checks.
        only_if_missing : Skip task when ALL target files already exist
                          (useful for slow one-time steps like climate scripts).
        always          : Always run (equivalent to --force for this task).
        """
        self._tasks.append({
            "name":            name,
            "rule":            rule,
            "source":          source or [],
            "target":          target or [],
            "only_if_missing": only_if_missing,
            "always":          always,
        })


# ── Helpers ───────────────────────────────────────────────────────────────────

def _mtime(path: str) -> float:
    try:
        return Path(path).stat().st_mtime
    except FileNotFoundError:
        return 0.0


def needs_rebuild(sources: list[str], targets: list[str]) -> bool:
    """True when any target is absent OR older than the newest source."""
    for t in targets:
        if not Path(t).exists():
            return True
    if not sources:
        return False
    newest_src = max(_mtime(s) for s in sources)
    oldest_tgt = min(_mtime(t) for t in targets)
    return newest_src > oldest_tgt


def run_task(task: dict):
    t0 = __import__("time").monotonic()
    print(f"[waf] running  – {task['name']}")
    result = subprocess.run(task["rule"], shell=True, cwd=ROOT)
    elapsed = __import__("time").monotonic() - t0
    if result.returncode != 0:
        print(
            f"[waf] FAILED (exit {result.returncode}, {elapsed:.1f}s)"
            f" – {task['name']}",
            file=sys.stderr,
        )
        sys.exit(result.returncode)
    print(f"[waf] done ({elapsed:.1f}s) – {task['name']}")


# ── Load wscript ──────────────────────────────────────────────────────────────

def _load_wscript():
    from importlib.machinery import SourceFileLoader
    loader = SourceFileLoader("wscript", str(ROOT / "wscript"))
    spec = importlib.util.spec_from_loader("wscript", loader)
    ws = importlib.util.module_from_spec(spec)
    loader.exec_module(ws)
    return ws


# ── Commands ──────────────────────────────────────────────────────────────────

def cmd_configure():
    ws = _load_wscript()
    ctx = ConfigContext()
    ws.configure(ctx)
    print("[waf] configure OK")


def cmd_build(force: bool = False, from_pattern: str | None = None):
    ws = _load_wscript()
    ctx = BuildContext()
    ws.build(ctx)
    tasks = ctx._tasks

    # ── --from: skip all tasks before the first one matching PATTERN ──────────
    start = 0
    if from_pattern:
        pat = from_pattern.lower()
        start = next(
            (
                i for i, t in enumerate(tasks)
                if pat in (t["name"] or "").lower()
                or any(pat in Path(s).name.lower() for s in t["source"])
                or any(pat in Path(g).name.lower() for g in t["target"])
            ),
            None,
        )
        if start is None:
            print(
                f"[waf] ERROR: --from={from_pattern!r} matched no task",
                file=sys.stderr,
            )
            sys.exit(1)
        skipped = [tasks[i]["name"] for i in range(start)]
        print(f"[waf] --from={from_pattern!r}  →  skipping: {skipped}")

    print(f"[waf] build  --from={from_pattern or '(start)'}")

    for i, task in enumerate(tasks):
        if i < start:
            continue

        if task["always"] or force:
            run_task(task)
            continue

        if task["only_if_missing"]:
            missing = [t for t in task["target"] if not Path(t).exists()]
            if missing:
                run_task(task)
            else:
                print(f"[waf] skip (outputs present) – {task['name']}")
            continue

        if needs_rebuild(task["source"], task["target"]):
            run_task(task)
        else:
            print(f"[waf] up-to-date – {task['name']}")

    print("[waf] build finished")


def cmd_clean():
    ws = _load_wscript()
    ctx = BuildContext()
    ws.build(ctx)
    for task in ctx._tasks:
        if task.get("only_if_missing"):
            continue          # never delete slow climate outputs
        for t in task["target"]:
            p = Path(t)
            if p.exists():
                p.unlink()
                print(f"[waf] removed {t}")
    print("[waf] clean finished")


# ── CLI ───────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Minimal WAF-like build runner (no external dependencies)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python waf.py configure               # check Rscript + lualatex are available
  python waf.py build                   # run full pipeline (skip up-to-date tasks)
  python waf.py build --force           # rebuild everything unconditionally
  python waf.py build --from=data       # skip climate steps, start from data.R
  python waf.py build --from=estimation # start from estimation_y.R
  python waf.py build --from=stats      # start from stats scripts
  python waf.py clean                   # delete all non-climate outputs
""",
    )
    sub = parser.add_subparsers(dest="command")

    sub.add_parser("configure", help="Verify required programs are available")

    build_p = sub.add_parser("build", help="Run the build pipeline")
    build_p.add_argument(
        "--force", action="store_true",
        help="Rebuild all tasks unconditionally",
    )
    build_p.add_argument(
        "--from", dest="from_pat", metavar="PATTERN",
        help="Skip tasks until the first one whose name/source/target contains PATTERN",
    )

    sub.add_parser("clean", help="Delete all non-climate generated files")

    args = parser.parse_args()

    if args.command == "configure":
        cmd_configure()
    elif args.command == "build":
        cmd_build(force=args.force, from_pattern=args.from_pat)
    elif args.command == "clean":
        cmd_clean()
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
