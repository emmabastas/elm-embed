from subprocess import Popen, PIPE
import os
from os.path import join
import shutil

def test(elmEmbedPath, outputDir):
    wd = os.getcwd()

    # elm init
    os.chdir(outputDir)
    out = open(join(outputDir, "0_INIT.out"), "w")
    err = open(join(outputDir, "0_INIT.err"), "w")
    p = Popen(["elm", "init"], stdout=out, stderr=err, stdin=PIPE)
    p.communicate(input="y".encode())

    # elm-embed init
    os.chdir(outputDir)
    out = open(join(outputDir, "1_EMBED_INIT.out"), "w")
    err = open(join(outputDir, "1_EMBED_INIT.err"), "w")
    p = Popen([elmEmbedPath, "init"], stdout=out, stderr=err, stdin=PIPE)
    p.communicate(input="y".encode())

    # copy characterization-tests/data-elm-source/src/BasicEmbedders.elm to outputDir
    os.chdir(wd)
    shutil.copyfile("characterization-tests/data-elm-source/elm-embed-scripts/BasicEmbedders.elm", join(outputDir, "elm-embed-scripts/BasicEmbedders.elm"))

    # elm-embed init
    os.chdir(outputDir)
    out = open(join(outputDir, "2_EMBED_RUN.out"), "w")
    err = open(join(outputDir, "2_EMBED_RUN.err"), "w")
    p = Popen([elmEmbedPath, "run"], stdout=out, stderr=err, stdin=PIPE)
    p.communicate(input="y".encode())

    # remove elm-stuff
    shutil.rmtree(join(outputDir, "elm-stuff"))
