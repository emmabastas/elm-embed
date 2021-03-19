#!/usr/bin/env python3

import sys
import os
from os.path import abspath, join
import shutil
import subprocess
import importlib
import tempfile
import filecmp

def main():

    print()

    elmEmbedPath = None
    if len(sys.argv) == 1:
        elmEmbedPath = "elm-embed"
    elif len(sys.argv) == 2:
        elmEmbedPath = sys.argv[1]
    else:
        print("USAGE: characterization-test.py [path to elm-embed]")
        exit(0)

    try:
        p = subprocess.Popen([elmEmbedPath, "--version"], stdout=subprocess.PIPE)
        out, _ = p.communicate()
        print("elm-embed: " + out.decode("utf-8"), end="")
    except FileNotFoundError:
        print("Could not find the elm-embed binary in PATH.")
        print("Specify the location of elm-embed like this:")
        print("    characterization-test.py /path/to/elm-embed")

    print()
    error_count = 0
    for line in open("characterization-tests/order.txt", "r").readlines():
        if line == "" or line.isspace() or line.startswith("#"):
            continue

        n = line[:-1]

        m = importlib.import_module(".." + n, "characterization-tests.subpkg")

        snapshotDir = "characterization-tests/" + n
        if not os.path.isdir(snapshotDir):
            print("Snapshot directory for " + n + "does not exist")
            exit(0)

        wd = os.getcwd()
        outputDir = tempfile.mkdtemp(suffix="-"+n)
        m.test(elmEmbedPath, outputDir)
        os.chdir(wd)

        diff = filecmp.dircmp(snapshotDir, outputDir)
        diffs = gatherDiffs(diff)

        if len(diffs) == 0:
            print(bcolors.OK + "OK " + bcolors.ENDC + n)
            shutil.rmtree(outputDir)
            continue

        diffs = sorted(diffs, key=lambda t: t[1])

        print()
        print(bcolors.ERR + "ERR! " + bcolors.ENDC + n)
        print("The output and snapshot doesn't match.")
        print()
        print("Keys:")
        print(bcolors.RED + "    -" + bcolors.ENDC + " = "
                + "file/dir pressent in snapshot but not in output")
        print(bcolors.GREEN + "    +" + bcolors.ENDC + " = "
                + "file/dir pressent in output but not in snapshot")
        print(bcolors.CYAN + "    m" + bcolors.ENDC + " = file content missmatch")

        print()
        print("Diff:")
        for [ t, p ] in diffs:
            if t == "removed":
                print(bcolors.RED + "- " + bcolors.ENDC, end="")
            elif t == "added":
                print(bcolors.GREEN + "+ " + bcolors.ENDC, end="")
            else:
                print(bcolors.CYAN + "m " + bcolors.ENDC, end="")
            print(p)

        print()
        print("Output dir: " + outputDir)
        print("Snapshot dir: " + abspath(snapshotDir))

        print()
        print("To make current output be the snapshot, run:")
        print("     rm -r " + abspath(snapshotDir)
                + " && mv " + outputDir + " " + abspath(snapshotDir))


    print()
    print("Done. " + str(error_count) + " error(s)")

def gatherDiffs(diff):
    diffs = []

    diffs += [ [ "removed", i ] for i in diff.left_only ]
    diffs += [ [ "added", i ] for i in diff.right_only ]
    diffs += [ [ "modified", i ] for i in diff.diff_files ]

    for [ base, subdiff ] in diff.subdirs.items():
        subdiffs = gatherDiffs(subdiff)
        diffs += [ [ t, join(base, i) ] for [ t, i ] in subdiffs ]

    return diffs

class bcolors:
    HEADER = '\033[95m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    GREEN = '\033[92m'
    RED = '\033[91m'
    CYAN = '\033[96m'
    OK = GREEN
    ERR = RED
    ENDC = '\033[0m'


if __name__ == "__main__":
    main()
