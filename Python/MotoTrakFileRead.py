"""Python port of MotoTrakFileRead.m.

Reads MotoTrak 2.x session files (versions -5 and -6) into a Python dict.
"""

from __future__ import annotations

import math
import struct
from pathlib import Path
from typing import BinaryIO, Dict, List


def _read_exact(fid: BinaryIO, n: int) -> bytes:
    b = fid.read(n)
    if len(b) != n:
        raise EOFError("Unexpected end of file")
    return b


def _read_i8(fid: BinaryIO) -> int:
    return struct.unpack("<b", _read_exact(fid, 1))[0]


def _read_u8(fid: BinaryIO) -> int:
    return struct.unpack("<B", _read_exact(fid, 1))[0]


def _read_u16(fid: BinaryIO) -> int:
    return struct.unpack("<H", _read_exact(fid, 2))[0]


def _read_i32(fid: BinaryIO) -> int:
    return struct.unpack("<i", _read_exact(fid, 4))[0]


def _read_u32(fid: BinaryIO) -> int:
    return struct.unpack("<I", _read_exact(fid, 4))[0]


def _read_f32(fid: BinaryIO) -> float:
    return struct.unpack("<f", _read_exact(fid, 4))[0]


def _read_f64(fid: BinaryIO) -> float:
    return struct.unpack("<d", _read_exact(fid, 8))[0]


def _read_chars(fid: BinaryIO, n: int) -> str:
    return _read_exact(fid, n).decode("latin-1", errors="ignore")


def _read_f32_array(fid: BinaryIO, n: int) -> List[float]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 4 * n)
    return list(struct.unpack("<" + "f" * n, raw))


def _read_f64_array(fid: BinaryIO, n: int) -> List[float]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 8 * n)
    return list(struct.unpack("<" + "d" * n, raw))


def _read_trial(fid: BinaryIO, num_streams: int, version: int) -> Dict:
    trial: Dict = {}
    trial_number = _read_u32(fid)
    trial["trial_number"] = trial_number

    trial["start_time"] = _read_f64(fid)

    result = chr(_read_u8(fid))
    trial["result"] = result

    if result == "P":
        trial["end_time"] = _read_f64(fid)
    else:
        trial["end_time"] = math.nan

    trial["hit_window_duration"] = _read_f32(fid)
    trial["pre_trial_duration"] = _read_f32(fid)
    trial["post_trial_duration"] = _read_f32(fid)
    trial["post_trial_timeout"] = _read_f32(fid)
    trial["position"] = _read_f32(fid)

    n_params = _read_u8(fid)
    trial["parameters"] = [_read_f32(fid) for _ in range(n_params)]

    trial["nominal_parameters"] = []
    if version == -6:
        n_nominal = _read_u8(fid)
        for _ in range(n_nominal):
            n_chars = _read_u8(fid)
            trial["nominal_parameters"].append(_read_chars(fid, n_chars))

    n_hits = _read_u8(fid)
    trial["hit_times"] = _read_f64_array(fid, n_hits)

    n_triggers = _read_u8(fid)
    trial["output_trigger_times"] = _read_f64_array(fid, n_triggers)

    n_samples = _read_u32(fid)
    signals = []
    for _ in range(num_streams):
        signals.append(_read_f32_array(fid, n_samples))
    trial["signal"] = signals

    # Match MATLAB post-processing for non-relative sample time vectors.
    if signals and signals[0] and signals[0][0] > 0:
        times = signals[0]
        if len(times) > 1:
            diffs = [times[i + 1] - times[i] for i in range(len(times) - 1)]
            diffs_sorted = sorted(diffs)
            med = diffs_sorted[len(diffs_sorted) // 2]
            sample_period = round(med / 1000.0)
            if sample_period != 0:
                idx = int((1000.0 * trial["pre_trial_duration"] / sample_period) + 1)
                if 1 <= idx <= len(times):
                    ref = times[idx - 1]
                    signals[0] = [t - ref for t in times]

        if signals[0] and signals[0][-1] > 1000.0 * (
            trial["pre_trial_duration"]
            + trial["hit_window_duration"]
            + trial["post_trial_duration"]
        ):
            signals[0] = [t / 1000.0 for t in signals[0]]

    return trial


def MotoTrakFileRead(file: str) -> Dict:
    """Read a MotoTrak binary session file (versions -5 and -6)."""
    data: Dict = {}

    block_trial = 0
    block_manual_feed = 1
    block_pause_start = 2
    block_pause_finish = 3
    block_timestamped_note = 4
    block_general_note = 5
    block_session_end = 6

    with Path(file).open("rb") as fid:
        fid.seek(0)
        version = _read_i8(fid)
        if version not in (-5, -6):
            raise ValueError("Incorrect file version. Cannot read this file.")

        data["version"] = version
        data["start_time"] = _read_f64(fid)

        n = _read_u8(fid)
        data["subject"] = _read_chars(fid, n)

        n = _read_u8(fid)
        data["booth"] = _read_chars(fid, n)

        n = _read_u8(fid)
        data["stage"] = _read_chars(fid, n)

        n = _read_u8(fid)
        data["device"] = _read_chars(fid, n)

        n_cal = _read_u8(fid)
        data["calibration_coefficients"] = _read_f32_array(fid, n_cal)

        n_streams = _read_u8(fid)
        data_streams = []
        for _ in range(n_streams):
            n_descr = _read_u8(fid)
            descr = _read_chars(fid, n_descr)
            n_units = _read_u8(fid)
            units = _read_chars(fid, n_units)
            data_streams.append({"stream_description": descr, "stream_units": units})
        data["data_streams"] = data_streams

        n_params = _read_u32(fid)
        data["parameters"] = []
        for _ in range(n_params):
            n_name = _read_u8(fid)
            data["parameters"].append(_read_chars(fid, n_name))

        data["nominal_parameters"] = []
        if version == -6:
            n_nominal = _read_u32(fid)
            for _ in range(n_nominal):
                n_name = _read_u8(fid)
                data["nominal_parameters"].append(_read_chars(fid, n_name))

        data["trial"] = []
        data["manual_feeds"] = []
        data["pause_start_times"] = []
        data["pause_end_times"] = []
        data["session_notes"] = ""
        data["timestamped_notes"] = []

        while True:
            block_id_raw = fid.read(4)
            if len(block_id_raw) < 4:
                break
            block_id = struct.unpack("<i", block_id_raw)[0]

            try:
                if block_id == block_trial:
                    data["trial"].append(_read_trial(fid, n_streams, version))
                elif block_id == block_manual_feed:
                    data["manual_feeds"].append(_read_f64(fid))
                elif block_id == block_pause_start:
                    data["pause_start_times"].append(_read_f64(fid))
                elif block_id == block_pause_finish:
                    data["pause_end_times"].append(_read_f64(fid))
                elif block_id == block_timestamped_note:
                    ts = _read_f64(fid)
                    note_len = _read_u16(fid)
                    txt = _read_chars(fid, note_len)
                    data["timestamped_notes"].append({"timestamp": ts, "text": txt})
                elif block_id == block_general_note:
                    note_len = _read_u16(fid)
                    data["session_notes"] = _read_chars(fid, note_len)
                elif block_id == block_session_end:
                    data["end_time"] = _read_f64(fid)
                else:
                    # Unknown block ID: stop to avoid desynchronizing the stream.
                    break
            except EOFError:
                break

    return data


if __name__ == "__main__":
    import argparse
    import json

    parser = argparse.ArgumentParser(description="Read .MotoTrak files.")
    parser.add_argument("file", help="Path to the .MotoTrak file")
    args = parser.parse_args()

    session = MotoTrakFileRead(args.file)
    print(json.dumps(session, indent=2, default=str))