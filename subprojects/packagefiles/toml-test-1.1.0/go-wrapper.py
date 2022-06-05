#!/usr/bin/env python3

if __name__ == "__main__":

    import sys, subprocess

    out, err = subprocess.Popen(
        sys.argv[2:],
        cwd=sys.argv[1],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).communicate()
