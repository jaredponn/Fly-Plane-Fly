#!/bin/bash

rm log.txt
touch log.txt

stack build &&
stack exec bird

chmod 755 log.txt

#  stack build --ghc-options -rtsopts --library-profiling --executable-profiling  --ghc-options -fprof-cafs --ghc-options -fforce-recomp
