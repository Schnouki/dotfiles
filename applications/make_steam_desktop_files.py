#!/usr/bin/env python3

from pathlib import Path
import functools
import re
import shlex
import shutil

from PIL import Image
import slugify


STEAM_BASE_DIR = "~/.local/share/Steam"
DESKTOP_TEMPLATE = """[Desktop Entry]
Name={name}
Comment=Play {name} on Steam
Exec={wrapper}steam steam://rungameid/{appid}
Icon={icon_path}
Terminal=false
Type=Application
Categories=Game;
"""
NAME_BLOCKLIST_RE = re.compile(
    r"""(?:
    ^Proton
    |^Steamworks
    |^Steam\ Linux\ Runtime
)""",
    re.VERBOSE,
)


def main():
    app_dir = Path(".").resolve()
    steam_dir = Path(STEAM_BASE_DIR).expanduser()

    acf_files = (steam_dir / "steamapps").glob("appmanifest_*.acf")
    for acf_file in acf_files:
        process_acf(app_dir, steam_dir, acf_file)

    cleanup_steam_desktop_files(app_dir)


@functools.cache
def get_wrapper() -> str:
    if w := shutil.which("prime-run"):
        return "prime-run "
    return ""


def process_acf(app_dir: Path, steam_dir: Path, acf_file: Path):
    acf_data = parse_acf(acf_file)
    appid = acf_data["appid"]
    name = acf_data["name"]
    if NAME_BLOCKLIST_RE.search(name):
        return
    slug_name = slugify.slugify(name, separator="_")
    icon_path = steam_dir / "appcache" / "librarycache" / f"{appid}_icon.jpg"
    png_icon_path = app_dir / "steam_icons" / f"{slug_name}.png"
    if not png_icon_path.is_file():
        png_icon_path.parent.mkdir(parents=True, exist_ok=True)
        with Image.open(icon_path) as im:
            im.save(png_icon_path)

    file_name = f"steamgame_{slug_name}.desktop"
    file_content = DESKTOP_TEMPLATE.format(
        appid=appid,
        name=name,
        icon_path=png_icon_path,
        wrapper=get_wrapper(),
    )

    file_path = app_dir / file_name
    if not file_path.is_file():
        file_path.write_text(file_content)


def parse_acf(acf_file: Path) -> dict[str, str]:
    data = {}
    started = False
    for line in acf_file.read_text().splitlines():
        if line == "{":
            started = True
            continue
        if started:
            line = line.strip()
            if line == "{":
                break
            tab = shlex.split(line)
            if len(tab) == 2:
                data[tab[0]] = tab[1]
    return data


def cleanup_steam_desktop_files(app_dir: Path):
    for file_path in app_dir.glob("*.desktop"):
        if file_path.name.startswith("steamgame_"):
            continue
        # Steam desktop files are executable
        if file_path.stat().st_mode & 0o700 != 0o700:
            continue
        if "steam steam://rungameid/" in file_path.read_text():
            file_path.unlink()


if __name__ == "__main__":
    main()
