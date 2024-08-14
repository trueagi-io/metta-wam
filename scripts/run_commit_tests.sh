#!/bin/bash

# This script generates the input file used by the Python script.
# Replace the following lines with the actual commands to generate the input file.

#echo "| ANTI-REGRESSION.BC-COMP.01 | PASS |(https://example.com/test-report) | (assertEqualToResult (add-atom &kb (: axiom (nums 2 3)))) | (()) | (()) |" > /tmp/SHARED.UNITS
cat ./scripts/SHARED.UNITS.PREV.md > /tmp/SHARED.UNITS
# You can add more lines or commands to generate additional input data
