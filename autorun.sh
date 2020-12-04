#!/usr/bin/env bash

./run.sh && when-changed *.sh src/**.d -c "clear; ./run.sh"
