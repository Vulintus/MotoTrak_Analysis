"""Python port of ArdyMotorFileRead.m.

This module reads legacy MotoTrak/ArdyMotor binary files and returns a
dictionary shaped similarly to the MATLAB output structure.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
import struct
from typing import BinaryIO, Dict, List, Optional, Tuple


def _matlab_datenum_from_unix(ts: float) -> float:
    return ts / 86400.0 + 719529.0


def _read_exact(fid: BinaryIO, n: int) -> bytes:
    b = fid.read(n)
    if len(b) != n:
        raise EOFError("Unexpected end of file")
    return b


def _read_u8(fid: BinaryIO) -> int:
    return struct.unpack("<B", _read_exact(fid, 1))[0]


def _read_i8(fid: BinaryIO) -> int:
    return struct.unpack("<b", _read_exact(fid, 1))[0]


def _read_u16(fid: BinaryIO) -> int:
    return struct.unpack("<H", _read_exact(fid, 2))[0]


def _read_i16(fid: BinaryIO) -> int:
    return struct.unpack("<h", _read_exact(fid, 2))[0]


def _read_u32(fid: BinaryIO) -> int:
    return struct.unpack("<I", _read_exact(fid, 4))[0]


def _read_f32(fid: BinaryIO) -> float:
    return struct.unpack("<f", _read_exact(fid, 4))[0]


def _read_f64(fid: BinaryIO) -> float:
    return struct.unpack("<d", _read_exact(fid, 8))[0]


def _read_chars(fid: BinaryIO, n: int) -> str:
    return _read_exact(fid, n).decode("latin-1", errors="ignore")


def _read_f64_array(fid: BinaryIO, n: int) -> List[float]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 8 * n)
    return list(struct.unpack("<" + "d" * n, raw))


def _read_f32_array(fid: BinaryIO, n: int) -> List[float]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 4 * n)
    return list(struct.unpack("<" + "f" * n, raw))


def _read_u16_array(fid: BinaryIO, n: int) -> List[int]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 2 * n)
    return list(struct.unpack("<" + "H" * n, raw))


def _read_i16_array(fid: BinaryIO, n: int) -> List[int]:
    if n <= 0:
        return []
    raw = _read_exact(fid, 2 * n)
    return list(struct.unpack("<" + "h" * n, raw))


def _ardymotorfileread_get_daycode(date_timestamp: float) -> int:
    base = datetime(1, 1, 1)
    dt = base + timedelta(days=date_timestamp - 1)
    return dt.timetuple().tm_yday


def ArdyMotorFileRead(file: str) -> Tuple[Dict, int]:
    """Read an .ArdyMotor file.

    Returns:
        (data, version)
    """
    file_path = Path(file)
    file_datenum = _matlab_datenum_from_unix(file_path.stat().st_mtime)

    data: Dict = {}
    trials_by_number: Dict[int, Dict] = {}

    with file_path.open("rb") as fid:
        fid.seek(0)
        version = _read_i8(fid)
        data["version"] = version

        if version < 0:
            if version == -1 or version <= -3:
                data["daycode"] = _read_u16(fid)
                if data["daycode"] > 366:
                    fid.seek(2, 0)
                    data["daycode"] = _read_u16(fid)
                    if data["daycode"] > 366:
                        fid.seek(2, 0)
                        data["daycode"] = _read_u8(fid)

            data["booth"] = _read_u8(fid)
            n = _read_u8(fid)
            data["subject"] = _read_chars(fid, n)
            data["position"] = _read_f32(fid)
            n = _read_u8(fid)
            data["stage"] = _read_chars(fid, n)
            n = _read_u8(fid)
            data["device"] = _read_chars(fid, n)

            data["cal"] = [1.0, 0.0]
            if file_datenum < 735088:
                data["device"] = "Wheel"
                data["cal"][0] = 0.5

            device_lc = data["device"].lower()
            if version <= -3:
                if device_lc in {"pull", "knob", "lever"}:
                    data["cal"] = [_read_f32(fid), _read_f32(fid)]
                elif device_lc == "wheel":
                    data["cal"][0] = _read_f32(fid)
            else:
                if device_lc == "pull":
                    data["cal"] = [_read_f32(fid), _read_f32(fid)]
                elif device_lc in {"wheel", "knob"} and file_datenum > 735088:
                    data["cal"][0] = _read_f32(fid)

            n = _read_u8(fid)
            data["constraint"] = _read_chars(fid, n)
            n = _read_u8(fid)
            data["threshtype"] = _read_chars(fid, n)
            if version == -1 or version <= -3:
                data["pre_trial_sampling_dur"] = 1000.0
            else:
                data["pre_trial_sampling_dur"] = _read_f32(fid)

            data["pauses"] = []
            data["manual_feeds"] = []

            while True:
                trial_num_raw = fid.read(4)
                if len(trial_num_raw) < 4:
                    break
                trial_num = struct.unpack("<I", trial_num_raw)[0]

                try:
                    starttime = _read_f64(fid)
                    outcome = chr(_read_u8(fid))
                except EOFError:
                    break

                if outcome == "P":
                    try:
                        endtime = _read_f64(fid)
                    except EOFError:
                        break
                    data["pauses"].append([starttime, endtime])
                    continue

                if outcome == "F":
                    data["manual_feeds"].append(starttime)
                    continue

                if file_datenum < 735122.5980:
                    fid.seek(-1, 1)

                try:
                    trial = {
                        "starttime": starttime,
                        "hitwin": _read_f32(fid),
                        "init": _read_f32(fid),
                        "thresh": _read_f32(fid),
                    }
                    if version == -4:
                        trial["ceiling"] = _read_f32(fid)

                    n_hits = _read_u8(fid)
                    trial["hittime"] = _read_f64_array(fid, n_hits)

                    if file_datenum < 735122.5980:
                        outcome = "H" if n_hits > 1 else "M"
                    trial["outcome"] = outcome

                    n_stim = _read_u8(fid)
                    trial["stimtime"] = _read_f64_array(fid, n_stim)

                    buffsize = _read_u32(fid)
                    sample_times = _read_u16_array(fid, buffsize)
                    trial["signal"] = _read_f32_array(fid, buffsize)
                    trial["ir"] = _read_i16_array(fid, buffsize)

                    if sample_times and all(t == 0 for t in sample_times):
                        prev = trials_by_number.get(trial_num - 1)
                        if prev is not None and "sample_times" in prev:
                            trial["sample_times"] = list(prev["sample_times"])
                        else:
                            pre = data["pre_trial_sampling_dur"]
                            trial["sample_times"] = [
                                int(10 * (i + 1) - pre) for i in range(len(trial["signal"]))
                            ]
                    else:
                        pre = data["pre_trial_sampling_dur"]
                        trial["sample_times"] = [int(t - pre) for t in sample_times]

                    trials_by_number[trial_num] = trial
                except EOFError:
                    break

            if version < -1 and trials_by_number:
                first_trial_num = min(trials_by_number)
                st = trials_by_number[first_trial_num].get("starttime")
                if st is not None:
                    data["daycode"] = _ardymotorfileread_get_daycode(st)

        else:
            # Read a first pass for calibration logic (matches MATLAB flow).
            data["session_start"] = _read_f64(fid)
            data["booth"] = _read_u8(fid)
            n = _read_u8(fid)
            data["subject"] = _read_chars(fid, n)
            data["position"] = _read_f32(fid)
            n = _read_u8(fid)
            data["stage"] = _read_chars(fid, n)
            n = _read_u8(fid)
            data["device"] = _read_chars(fid, n)

            data["cal"] = [1.0, 0.0]
            if file_datenum < 735088:
                data["device"] = "Wheel"
                data["cal"][0] = 0.5

            device_lc = data["device"].lower()
            if version <= -3:
                if device_lc in {"pull", "knob", "lever"}:
                    data["cal"] = [_read_f32(fid), _read_f32(fid)]
                elif device_lc == "wheel":
                    data["cal"][0] = _read_f32(fid)
            else:
                if device_lc == "pull":
                    data["cal"] = [_read_f32(fid), _read_f32(fid)]
                elif device_lc in {"wheel", "knob"} and file_datenum > 735088:
                    data["cal"][0] = _read_f32(fid)

            fid.seek(0, 0)
            data["version"] = 1
            data["daycode"] = _read_u16(fid)
            data["booth"] = _read_u8(fid)
            n = _read_u8(fid)
            data["subject"] = _read_chars(fid, n)
            data["position"] = _read_u8(fid)
            data["responsewindow"] = _read_u8(fid)

            first_stage_char = _read_chars(fid, 1)
            temp = [_read_chars(fid, 1)]
            while temp[-1] != first_stage_char:
                temp.append(_read_chars(fid, 1))

            data["stage"] = first_stage_char + "".join(temp[:-1])
            if first_stage_char == "P":
                data["device"] = temp[-1] + _read_chars(fid, 3)
            else:
                data["device"] = temp[-1] + _read_chars(fid, 4)

            data["bin"] = _read_u8(fid)
            numparams = _read_u8(fid)
            params = []
            for _ in range(numparams):
                n = _read_u16(fid)
                params.append({"name": _read_chars(fid, n)})
            data["param"] = params

            trial_counter = 0
            while True:
                # Preserve MATLAB behavior by stopping on partial/corrupt trial.
                trial_counter += 1
                trial = {}
                try:
                    threshold_raw = fid.read(2)
                    if len(threshold_raw) < 2:
                        break
                    trial["threshold"] = struct.unpack("<H", threshold_raw)[0]
                    trial["starttime"] = _read_f64(fid)
                    trial["hittime"] = _read_f64(fid)
                    trial["outcome"] = _read_f64(fid)

                    ir1 = []
                    for _ in range(3):
                        num_ir = _read_u32(fid)
                        ir1.append({"times": _read_f64_array(fid, num_ir)})
                    trial["IR1"] = ir1

                    num_bins = _read_u32(fid)
                    trial["signal"] = _read_f64_array(fid, num_bins)
                except EOFError:
                    break

                trials_by_number[trial_counter] = trial

    if trials_by_number:
        ordered = [trials_by_number[k] for k in sorted(trials_by_number)]
        data["trial"] = [t for t in ordered if t.get("signal")]
        if data["trial"] and "starttime" in data["trial"][0]:
            data["daycode"] = int(data["trial"][0]["starttime"])
    else:
        data["trial"] = []

    return data, version


if __name__ == "__main__":
    import argparse
    import json

    parser = argparse.ArgumentParser(description="Read .ArdyMotor files.")
    parser.add_argument("file", help="Path to the .ArdyMotor file")
    args = parser.parse_args()

    session, ver = ArdyMotorFileRead(args.file)
    print(json.dumps({"version": ver, "data": session}, indent=2, default=str))