#!/bin/bash

rm log.txt
touch log.txt

stack build
stack exec bird

chmod 755 log.txt
