import hashlib
import getpass

from fabric import task as fab_task
from invoke.exceptions import UnexpectedExit

task = fab_task(hosts=["ks"])


def _ensure_sudo(c):
    if c.config["sudo"]["password"] is None:
        c.config["sudo"]["password"] = getpass.getpass("Remote sudo password: ")


@task
def update(c):
    changed = upload(c)
    if changed:
        restart(c)


@task
def upload(c, uid=1000, gid=1000):
    _ensure_sudo(c)
    c.sudo("mkdir -p /srv/imapfilter", hide="stderr")
    c.sudo("chmod 0700 /srv/imapfilter", hide="stderr")
    c.sudo(f"chown {uid}:{gid} /srv/imapfilter", hide="stderr")

    with open("config.lua", "rb") as cfg:
        m = hashlib.sha256()
        m.update(cfg.read())
    local_sha = m.hexdigest()

    try:
        res = c.sudo("sha256sum /srv/imapfilter/config.lua", hide="both")
        remote_sha = res.stdout.split()[0]
    except UnexpectedExit:
        remote_sha = None

    if remote_sha == local_sha:
        print("Already up-to-date!")
        return False

    print("Updating the config file…")
    res = c.run("mktemp", hide="stdout")
    fn = res.stdout.strip()
    try:
        c.put("config.lua", fn)
        c.sudo(f"mv {fn} /srv/imapfilter/config.lua", hide="stderr")
    finally:
        c.run(f"rm -f {fn}")
    c.sudo("chmod 0400 /srv/imapfilter/config.lua", hide="stderr")
    c.sudo(f"chown {uid}:{gid} /srv/imapfilter/config.lua", hide="stderr")

    return True


@task
def restart(c):
    _ensure_sudo(c)
    print("Updating the image…")
    c.sudo("docker pull schnouki/imapfilter")
    print("Restarting the container…")
    c.sudo("docker service update imapfilter "
           "--image schnouki/imapfilter:latest "
           "--force")  #, hide="both")
