#!/bin/bash

MODE="${1:-debug}"

echo "Running in $MODE mode..."
nimble run --hints:off --verbosity:0 --gc:orc -d:$MODE -- $1
