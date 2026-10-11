"""
99_run_all.py

Driver — runs the adaptation module scripts in order. Idempotent.
Run from the share-package root.

    python code/99_run_all.py
"""
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
SCRIPTS = ROOT / "code"

steps = [
    "50_build_adapt_panel.py",
    "51_acute_regressions.py",
    "52_drought_regressions.py",
    "53_event_study.py",
    "54_diagnostics.py",
    "55_channel_stratified.py",
]

for s in steps:
    print(f"\n{'='*80}\n  Running {s}\n{'='*80}")
    rc = subprocess.call([sys.executable, str(SCRIPTS / s)], cwd=str(ROOT))
    if rc != 0:
        print(f"  !! {s} failed with exit code {rc}")
        sys.exit(rc)
print("\nDone.")
