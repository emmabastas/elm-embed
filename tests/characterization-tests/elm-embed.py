import subprocess
from os.path import join

def test(elmEmbedPath, outputDir):
    out = open(join(outputDir, "STDOUT"), "w")
    err = open(join(outputDir, "STDERR"), "w")
    subprocess.call([elmEmbedPath], stdout=out, stderr=err)
