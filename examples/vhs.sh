#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Iterate over each child directory
for dir in "$SCRIPT_DIR"/*/; do
  if [ -d "$dir" ]; then
    if [ -f $dir/demo.tape ]; then
        echo "Entering directory: $dir"
        (cd "$dir" && vhs demo.tape)
    fi
  fi
done
