#!/usr/bin/env bash
# Build the metasalmon source tarball into the repo root.
# This avoids the default devtools::build()/pkgbuild::build() behavior
# of writing the tarball to the parent directory when dest_path/path is NULL.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

VERSION="$(awk -F': ' '/^Version:/{print $2; exit}' DESCRIPTION)"
TARBALL="metasalmon_${VERSION}.tar.gz"

rm -f "$TARBALL"
R CMD build .

if [[ ! -f "$TARBALL" ]]; then
  echo "Expected tarball not found: $REPO_ROOT/$TARBALL" >&2
  exit 1
fi

printf '%s\n' "$REPO_ROOT/$TARBALL"
