#!/usr/bin/env python3

import subprocess
import sys


if __name__ == "__main__":
    completed = subprocess.run(sys.argv[2:], cwd=sys.argv[1])
    raise SystemExit(completed.returncode)
