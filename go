#!/bin/sh
mkdir build 2>>/dev/null
ghc -XExistentialQuantification -odir build -hidir build -o build/ttt src/main/hs/*.hs