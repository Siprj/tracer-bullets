#!/usr/bin/env bash
set -euo pipefail

# Find the project root using git
ROOT_DIR="$(git rev-parse --show-toplevel)"

# Extract route paths from Rust source
ROUTES=$(grep -Po '<Route path=path!\([^\)]*' "$ROOT_DIR/html-themes/src/lib.rs" | sed 's/^.*"\/\(.*\)"/\1/g')

for route in $ROUTES; do
  # Copy index.html as the route file
  cp "$ROOT_DIR/html-themes/dist/index.html" "$ROOT_DIR/html-themes/dist/$route.html"
done
