"""Example usage for ArdyMotorFileRead.py and MotoTrakFileRead.py.

Run from the Python folder, for example:
    python example_usage.py --ardymotor "C:/data/session.ArdyMotor"
    python example_usage.py --mototrak "C:/data/session.MotoTrak"
    python example_usage.py --ardymotor "..." --mototrak "..."
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Any, Dict, List

from ArdyMotorFileRead import ArdyMotorFileRead
from MotoTrakFileRead import MotoTrakFileRead


def _safe_len(value: Any) -> int:
    if isinstance(value, list):
        return len(value)
    return 0


def _print_ardymotor_summary(file_path: str) -> None:
    data, version = ArdyMotorFileRead(file_path)
    trials: List[Dict[str, Any]] = data.get("trial", [])

    print("ArdyMotor Summary")
    print(f"  File: {file_path}")
    print(f"  Version: {version}")
    print(f"  Subject: {data.get('subject', '')}")
    print(f"  Device: {data.get('device', '')}")
    print(f"  Stage: {data.get('stage', '')}")
    print(f"  Trials: {_safe_len(trials)}")
    print(f"  Pauses: {_safe_len(data.get('pauses'))}")
    print(f"  Manual feeds: {_safe_len(data.get('manual_feeds'))}")
    if trials:
        print(f"  First trial start: {trials[0].get('starttime')}")
    print()


def _print_mototrak_summary(file_path: str) -> None:
    data = MotoTrakFileRead(file_path)
    trials: List[Dict[str, Any]] = data.get("trial", [])

    print("MotoTrak Summary")
    print(f"  File: {file_path}")
    print(f"  Version: {data.get('version')}")
    print(f"  Subject: {data.get('subject', '')}")
    print(f"  Device: {data.get('device', '')}")
    print(f"  Stage: {data.get('stage', '')}")
    print(f"  Trials: {_safe_len(trials)}")
    print(f"  Timestamped notes: {_safe_len(data.get('timestamped_notes'))}")
    print(f"  Manual feeds: {_safe_len(data.get('manual_feeds'))}")
    if trials:
        print(f"  First trial start: {trials[0].get('start_time')}")
    print()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Load ArdyMotor and MotoTrak files and print summaries."
    )
    parser.add_argument(
        "--ardymotor",
        type=str,
        help="Path to an .ArdyMotor file",
    )
    parser.add_argument(
        "--mototrak",
        type=str,
        help="Path to a .MotoTrak or .MOTOTRAK file",
    )
    args = parser.parse_args()

    if not args.ardymotor and not args.mototrak:
        parser.error("Provide at least one of --ardymotor or --mototrak.")

    if args.ardymotor:
        ap = Path(args.ardymotor)
        if not ap.exists():
            raise FileNotFoundError(f"ArdyMotor file not found: {args.ardymotor}")
        _print_ardymotor_summary(str(ap))

    if args.mototrak:
        mp = Path(args.mototrak)
        if not mp.exists():
            raise FileNotFoundError(f"MotoTrak file not found: {args.mototrak}")
        _print_mototrak_summary(str(mp))


if __name__ == "__main__":
    main()
