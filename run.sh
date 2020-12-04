#!/usr/bin/env bash

rdmd src/simpleparser.d dlang.ebnf | tee test/dlang-ebnf-output.txt
