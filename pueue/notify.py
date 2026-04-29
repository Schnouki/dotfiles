#!/usr/bin/env python3

import os
import subprocess
import sys


def main():
    id_, command, path, result, exit_code, group, output, start, end = sys.argv[1:10]
    time = int(end) - int(start)
    command_name = os.path.basename(command.split()[0])

    if result == "Success" and (group == "git" or command_name == "sync-notes"):
        return

    subprocess.run(
        [
            "notify-send",
            f"Pueue: task {id_}: {result}",
            f"{command_name} finished with code {exit_code} in {time} seconds.",
        ]
    )


if __name__ == "__main__":
    main()
