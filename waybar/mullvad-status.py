#!/usr/bin/env python

import asyncio
import json
import subprocess as subp
import sys

VALID_STATUSES = {"connected", "connecting", "disconnected", "disconnecting"}


def parse_line(line: str) -> tuple[str, str]:
    status = line.split()[0].rstrip(".")
    status_l = status.lower()
    if status_l not in VALID_STATUSES:
        return "", ""
    return status, status_l


async def mullvad_listen():
    proc = await asyncio.create_subprocess_exec(
        "mullvad", "status", "listen", stdout=asyncio.subprocess.PIPE
    )

    async for line in proc.stdout:
        line = line.strip().decode()
        status, alt = parse_line(line)
        if not status:
            continue

        output = {"text": status, "alt": alt, "tooltip": f"Mullvad: {line}"}
        print(json.dumps(output), flush=True)

    await proc.wait()


def mullvad_toggle():
    cp = subp.run(["mullvad", "status"], capture_output=True, check=True)
    _, alt = parse_line(cp.stdout.strip().decode())

    cmd = "disconnect" if alt.startswith("connect") else "connect"
    subp.run(["mullvad", cmd], check=True)


def main():
    if len(sys.argv) == 1:
        asyncio.run(mullvad_listen())
    elif sys.argv[1] == "toggle":
        mullvad_toggle()
    else:
        raise ValueError(sys.argv[1])


if __name__ == "__main__":
    main()
