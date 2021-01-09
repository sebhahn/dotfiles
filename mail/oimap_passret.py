import os
import subprocess

home = os.environ['HOME']

def mailpasswd(acct):
    path = home + "{}.gpg".format(os.path.basename(acct))
    args = ["gpg2", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""
