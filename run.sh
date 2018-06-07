#!/bin/bash

#chmod 775
#stack build --executable-profiling --library-profiling --ghc-options=" -fprof-auto -rtsopts=all -caf-all -O2" &&
#/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/bin/fly-plane-fly +RTS -s -sstderr

stack build && stack exec fly-plane-fly
