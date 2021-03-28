#!/usr/bin/env python3

import sys
import os
from os.path import abspath, join
import shutil
import subprocess
from subprocess import PIPE
import importlib
import tempfile
import filecmp
import argparse

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument("--elm-embed-path",
            dest="elmEmbedPath",
            default="elm-embed"
        )
    parser.add_argument("--nodejs-path",
            dest="nodejsPath",
            default="node"
        )

    args = parser.parse_args()

    versionInfo = []
    try:
        p = subprocess.Popen([args.elmEmbedPath, "--version"], stdout=PIPE)
        out, _ = p.communicate()
        versionInfo.append( ("elm-embed", args.elmEmbedPath, out) )
    except FileNotFoundError:
        print("Could not find the elm-embed binary in your PATH.")
        print("Specify the location of elm-embed like this:")
        print("    characterization-test.py /path/to/elm-embed")
        exit(0)

    try:
        p = subprocess.Popen([args.nodejsPath, "--version"], stdout=PIPE)
        out, _ = p.communicate()
        versionInfo.append( ("nodejs", args.nodejsPath, out) )
    except FileNotFoundError:
        print("Could not find the nodejs binary in PATH.")
        print("Specify the location of nodejs like this:")
        print("    characterization-test.py /path/to/nodejs")
        exit(0)

    print()
    for name, path, version in versionInfo:
        print(name)
        print("    path: " + path)
        print("    version: " + version.decode("utf-8"))

    print()
    print("Running tests...")
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
        m.test(args.elmEmbedPath, outputDir)
        os.chdir(wd)

        diff = filecmp.dircmp(snapshotDir, outputDir)
        diffs = gatherDiffs(diff)

        if len(diffs) == 0:
            print(bcolors.OK + "OK " + bcolors.ENDC + n)
            shutil.rmtree(outputDir)
            continue

        error_count += 1

        diffs = sorted(diffs, key=lambda t: t[1])

        print()
        print(bcolors.ERR + "ERR! " + bcolors.ENDC + n)
        print("The output and snapshot doesn't match.")
        print()
        print("Diff:")
        for [ t, p ] in diffs:
            if t == "removed":
                print(bcolors.RED + "    - " + bcolors.ENDC, end="")
            elif t == "added":
                print(bcolors.GREEN + "    + " + bcolors.ENDC, end="")
            else:
                print(bcolors.CYAN + "    m " + bcolors.ENDC, end="")
            print(p)

        print()
        print("Output dir: " + outputDir)

        print()
        print("To make current output be the snapshot, run:")
        print("     rm -r " + abspath(snapshotDir)
                + " && mv " + outputDir + " " + abspath(snapshotDir))
        print()

    if error_count == 0:
        print ("Done. 0 errors")
        exit(0)

    print()
    print("Keys:")
    print(bcolors.RED + "    -" + bcolors.ENDC + " = "
            + "file/dir pressent in snapshot but not in output")
    print(bcolors.GREEN + "    +" + bcolors.ENDC + " = "
            + "file/dir pressent in output but not in snapshot")
    print(bcolors.CYAN + "    m" + bcolors.ENDC + " = file content missmatch")
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
