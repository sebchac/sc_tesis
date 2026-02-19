#!/usr/bin/env python3
"""
Minimal WAF-compatible build runner for sc_tesis.

Usage:
    python waf.py configure       # check tool availability
    python waf.py build           # smart rebuild (skips up-to-date targets)
    python waf.py build --force   # rebuild everything unconditionally
    python waf.py clean           # remove all declared targets
"""

import os
import sys
import subprocess
import time
from pathlib import Path


# ─── Context objects (mirror WAF's API) ───────────────────────────────────────

class ConfigContext:
    """Passed to the configure() function in wscript."""
    def __init__(self):
        self.env = {}
        self._ok = True

    def find_program(self, name, mandatory=True):
        import shutil
        path = shutil.which(name)
        if path:
            print(f"  [ok] {name} -> {path}")
            self.env[name] = path
        else:
            tag = "[!!]" if mandatory else "[--]"
            note = "REQUIRED – build may fail" if mandatory else "optional"
            print(f"  {tag} {name} not found ({note})")
            if mandatory:
                self._ok = False
        return path


class BuildContext:
    """Passed to the build() function in wscript."""
    def __init__(self, force=False):
        self.tasks = []
        self.force = force

    def add_task(self, *, rule, source=None, target=None, name=None,
                 always=False, only_if_missing=False):
        """Register a build task.

        Args:
            rule:           Shell command string. Use ${SRC} for first source.
            source:         List[str] – inputs that trigger rebuild when changed.
            target:         List[str] – outputs used for up-to-date checks.
            name:           Human-readable label shown in progress output.
            always:         If True, always run (ignore timestamps).
            only_if_missing:If True, only run when a target file is absent.
                            Use for slow tasks whose raw inputs are not stored
                            in the repo (e.g. large NetCDF climate files).
        """
        self.tasks.append({
            "rule":            rule,
            "source":          [str(s) for s in (source or [])],
            "target":          [str(t) for t in (target or [])],
            "name":            name,
            "always":          always or self.force,
            "only_if_missing": only_if_missing,
        })


# ─── Dependency logic ─────────────────────────────────────────────────────────

def _mtime(path: str):
    p = Path(path)
    return p.stat().st_mtime if p.exists() else None


def needs_rebuild(task: dict):
    """Return (bool, reason_str)."""
    if task["always"]:
        return True, "forced"

    targets = task["target"]
    sources = task["source"]

    missing = [t for t in targets if not Path(t).exists()]
    if missing:
        return True, f"missing output: {Path(missing[0]).name}"

    # only_if_missing: skip timestamp check (raw inputs may not be in repo)
    if task.get("only_if_missing"):
        return False, "up to date (only_if_missing)"

    oldest_target = min((_mtime(t) or 0.0) for t in targets)
    for s in sources:
        mt = _mtime(s)
        if mt is not None and mt > oldest_target:
            return True, f"source newer: {Path(s).name}"

    return False, "up to date"


# ─── Task execution ───────────────────────────────────────────────────────────

def run_task(task: dict, idx: int, total: int):
    rule    = task["rule"]
    sources = task["source"]
    targets = task["target"]
    name    = task["name"] or (
        Path(targets[0]).name if targets else
        Path(sources[0]).name if sources else
        rule[:60]
    )

    # Expand ${SRC} placeholder
    if "${SRC}" in rule:
        rule = rule.replace("${SRC}", " ".join(sources))

    width = 62
    print(f"\n{'─' * width}")
    print(f"  [{idx}/{total}] {name}")
    print(f"  $ {rule}")
    print(f"{'─' * width}")

    # Make sure output directories exist
    for t in targets:
        Path(t).parent.mkdir(parents=True, exist_ok=True)

    t0 = time.time()
    rc = subprocess.run(rule, shell=True).returncode
    elapsed = time.time() - t0

    if rc != 0:
        print(f"\n[waf] FAILED (exit {rc}, {elapsed:.1f}s) – {name}")
        sys.exit(rc)

    print(f"\n[waf] done ({elapsed:.1f}s) – {name}")


# ─── Top-level commands ───────────────────────────────────────────────────────

def _load_wscript():
    p = Path("wscript")
    if not p.exists():
        sys.exit("Error: wscript not found. Run from the repository root.")
    ns = {"__file__": str(p.resolve())}
    exec(p.read_text(encoding="utf-8"), ns)  # noqa: S102
    return ns


def cmd_configure():
    print("[waf] configure\n")
    ns  = _load_wscript()
    ctx = ConfigContext()
    if "configure" in ns:
        ns["configure"](ctx)
    status = "all tools found" if ctx._ok else "WARNING: some tools missing"
    print(f"\n[waf] configure: {status}")


def cmd_build(force=False, from_pattern=None):
    flags = []
    if force:        flags.append("--force")
    if from_pattern: flags.append(f"--from={from_pattern}")
    print(f"[waf] build{'  ' + ' '.join(flags) if flags else ''}\n")

    ns = _load_wscript()
    if "build" not in ns:
        sys.exit("Error: wscript has no build() function")

    ctx = BuildContext(force=force)
    ns["build"](ctx)

    # ── --from: skip all tasks before the first one whose name matches ────────
    tasks = ctx.tasks
    if from_pattern:
        pat = from_pattern.lower()
        start = next(
            (i for i, t in enumerate(tasks)
             if pat in (t["name"] or "").lower()
             or any(pat in Path(s).name.lower() for s in t["source"])
             or any(pat in Path(g).name.lower() for g in t["target"])),
            None,
        )
        if start is None:
            sys.exit(f"[waf] --from={from_pattern!r}: no matching task found")
        skipped_names = [t["name"] or f"task-{i+1}" for i, t in enumerate(tasks[:start])]
        print(f"  --from: skipping {start} task(s): {', '.join(skipped_names)}\n")
        tasks = tasks[start:]

    total   = len(ctx.tasks)          # keep original numbering for display
    offset  = len(ctx.tasks) - len(tasks)
    built   = 0
    skipped = 0

    for i, task in enumerate(tasks, offset + 1):
        rebuild, reason = needs_rebuild(task)
        label = task["name"] or (
            Path(task["target"][0]).name if task["target"] else f"task-{i}"
        )
        if rebuild:
            run_task(task, i, total)
            built += 1
        else:
            print(f"  [{i}/{total}] up to date  {label}")
            skipped += 1

    print(f"\n{'=' * 62}")
    print(f"[waf] build complete – {built} rebuilt, {skipped} skipped")
    print(f"{'=' * 62}")


def cmd_clean():
    print("[waf] clean\n")
    ns  = _load_wscript()
    ctx = BuildContext()
    if "build" in ns:
        ns["build"](ctx)

    removed = 0
    for task in ctx.tasks:
        for t in task["target"]:
            p = Path(t)
            if p.exists():
                p.unlink()
                print(f"  removed: {t}")
                removed += 1

    print(f"\n[waf] clean complete – {removed} files removed")


# ─── Entry point ─────────────────────────────────────────────────────────────

COMMANDS = {
    "configure": cmd_configure,
    "build":     cmd_build,
    "clean":     cmd_clean,
}

if __name__ == "__main__":
    # Always run relative to the directory that contains waf.py
    os.chdir(Path(__file__).parent)

    argv = sys.argv[1:]

    # Parse flags
    force = "--force" in argv
    from_pattern = next(
        (a.split("=", 1)[1] for a in argv if a.startswith("--from=")),
        None,
    )
    argv = [a for a in argv if not a.startswith("--")]

    cmd = argv[0] if argv else "build"
    if cmd not in COMMANDS:
        print(f"Unknown command: {cmd!r}")
        print(f"Available: {', '.join(COMMANDS)}")
        print("\nFlags for 'build':")
        print("  --force          Rebuild all targets unconditionally")
        print("  --from=PATTERN   Skip tasks until the first one matching PATTERN")
        print("                   Examples: --from=data.R  --from=estimation  --from=stats")
        sys.exit(1)

    if cmd == "build":
        cmd_build(force=force, from_pattern=from_pattern)
    else:
        COMMANDS[cmd]()
