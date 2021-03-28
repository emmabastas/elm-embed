from subprocess import Popen, PIPE
import os
from os.path import join
import shutil

def test(elmEmbedPath, outputDir):
    wd = os.getcwd()
    os.chdir(outputDir)

    # elm init
    p = Popen(["elm", "init"], stdout=PIPE, stderr=PIPE, stdin=PIPE)
    p.communicate(input="y".encode())

    # elm-embed init
    p = Popen([elmEmbedPath, "init"], stdout=PIPE, stderr=PIPE, stdin=PIPE)
    p.communicate(input="y".encode())

    # copy characterization-tests/data-elm-source/src/DummyTest.elm to outputDir
    shutil.copyfile(join(wd, "characterization-tests/data-elm-source/elm-embed-scripts/DummyTest.elm"), "elm-embed-scripts/DummyTest.elm")

    # elm-embed run
    p = Popen([elmEmbedPath, "run"], stdout=PIPE, stderr=PIPE, stdin=PIPE)
    p.communicate(input="y".encode())

    # remove elm-stuff
    os.remove("elm.json")
    shutil.rmtree("src")
    shutil.rmtree("elm-embed-scripts")
    shutil.rmtree("elm-stuff")
