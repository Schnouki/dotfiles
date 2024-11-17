function is_remote_fs
    # Simple heuristic: for remote filesystems, the mount source contains ':'.
    # For example, `sshfs some_server:/path ~/mnt`. Probably not very accurate,
    # but should be enough.
    return (df --output=source . | tail -1 | string match -q '*:*')
end
