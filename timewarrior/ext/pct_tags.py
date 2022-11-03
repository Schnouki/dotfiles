#!/usr/bin/python3

from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime, timezone
import json
import sys
from typing import Any, Optional, Sequence


@dataclass(frozen=True)
class Interval:
    start: datetime
    end: datetime
    tags: set[str]

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Interval":
        start = parse_date(data["start"])
        end = parse_date(data["end"]) if data.get("end") else datetime.now()
        tags = set(data["tags"])
        return cls(start, end, tags)

    @property
    def duration(self) -> int:
        """Interval duration, in seconds"""
        total = (self.end - self.start).total_seconds()
        return int(total)


@dataclass(frozen=True)
class TwData:
    config: dict[str, str]
    intervals: list[Interval]

    @classmethod
    def from_file(cls, fd) -> "TwData":
        cfg = {}
        while line := next(fd).strip():
            key, value = line.split(":", 1)
            cfg[key] = value.strip()

        raw_data = json.load(fd)
        intervals = [Interval.from_dict(entry) for entry in raw_data]
        return cls(cfg, intervals)


def parse_date(s: str) -> datetime:
    return (
        datetime.strptime(s, "%Y%m%dT%H%M%SZ")
        .replace(tzinfo=timezone.utc)
        .astimezone(tz=None)
        .replace(tzinfo=None)
    )


def format_time(sec: int) -> str:
    mm, ss = divmod(sec, 60)
    hh, mm = divmod(mm, 60)

    if hh:
        return f"{hh}:{mm:02d}:{ss:02d}"
    else:
        return f"{mm}:{ss:02d}"


def format_pct(value: int, total: int) -> str:
    pct = 100.0 * value / total
    return f"{pct:.1f}%"


def print_table(
    rows: list[Optional[Sequence[str]]],
    columns: Sequence[str],
    spaces: int = 8,
    right_aligned: Optional[set[int]] = None,
):
    right_aligned_idxs = right_aligned or set()

    col_width = [len(col) for col in columns]
    for row in rows:
        if not row:
            continue
        for idx, value in enumerate(row):
            col_width[idx] = max(col_width[idx], len(str(value)))

    def _print_row(row: Sequence[str], ralign=True):
        line = ""
        for idx, val in enumerate(row):
            spc = " " * (col_width[idx] - len(val))
            if ralign and idx in right_aligned_idxs:
                line += spc + val
            else:
                line += val + spc
            line += " " * spaces
        print(line.rstrip())

    def _print_sep(c: str):
        line = c * (sum(col_width) + spaces * (len(columns) - 1))
        print(line)

    # Title
    _print_row(columns, ralign=False)
    _print_sep("=")

    # Data
    for row in rows:
        if row:
            _print_row(row)
        else:
            _print_sep("-")


def main() -> None:
    data = TwData.from_file(sys.stdin)
    if len(data.intervals) == 0:
        print("No data!")
        return

    # Prepare for data processing
    top_level_tags = set(data.config.get("pct_tags.top_level_tags", "").split(","))
    ignore_tags = set(data.config.get("pct_tags.ignore_tags", "").split(","))
    durations: dict[str, dict[tuple[str, ...], int]] = defaultdict(
        lambda: defaultdict(int)
    )

    # Sum durations by tags
    total_duration = 0
    for interval in data.intervals:
        if interval.tags & ignore_tags:
            continue
        top = interval.tags & top_level_tags
        if len(top) == 0:
            top_tag, tags = "", interval.tags
        elif len(top) == 1:
            top_tag = top.pop()
            tags = interval.tags - {top_tag}
        else:
            raise ValueError(f"Bad top tag: {top}")
        tags_t = tuple(sorted(tags))
        durations[top_tag][tags_t] += interval.duration
        total_duration += interval.duration

    # Now put that into a table
    # Columns: category, time, pct category, pct total
    lines: list[Optional[Sequence[str]]] = []
    for top_tag, tag_durations in durations.items():
        tag_total_duration = sum(tag_durations.values())

        if top_tag:
            lines.append(None)
            lines.append(
                (
                    top_tag,
                    format_time(tag_total_duration),
                    "",
                    format_pct(tag_total_duration, total_duration),
                )
            )
            prefix = "  "
        else:
            prefix = ""

        for tags_t, duration in tag_durations.items():
            if len(tags_t) == 0:
                tag = top_tag
            elif len(tags_t) == 1:
                tag = tags_t[0]
            else:
                # raise ValueError(f"Too many tags: {tags_t}. Should one of those be top-level?")
                tag = ", ".join(sorted(tags_t))
            lines.append(
                (
                    prefix + tag,
                    format_time(duration),
                    format_pct(duration, tag_total_duration),
                    format_pct(duration, total_duration),
                )
            )

    while not lines[0]:
        lines = lines[1:]

    # Metadata
    if data.config.get("temp.report.start"):
        start_date = parse_date(data.config["temp.report.start"])
    else:
        start_date = min(interval.start for interval in data.intervals)

    if data.config.get("temp.report.end"):
        end_date = parse_date(data.config["temp.report.end"])
    else:
        end_date = max(interval.end for interval in data.intervals)

    # Nice display
    header = ["Report", f"  from: {start_date}", f"  to:   {end_date}"]
    # TODO: include temp.report.tags

    print("\n".join(header))
    print()

    print_table(
        lines, ("Category", "Time", "% cat.", "% total"), right_aligned={1, 2, 3}
    )


if __name__ == "__main__":
    main()
