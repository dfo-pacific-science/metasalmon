#!/usr/bin/env bash
# Install dependencies for metasalmon R package development
# This script runs on SessionStart via Claude Code hooks

set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

# Only run in remote (web) environments
if [ "$CLAUDE_CODE_REMOTE" != "true" ]; then
  echo "Skipping dependency installation (not in remote environment)"
  exit 0
fi

echo "Installing R and dependencies for metasalmon..."

# Install R if not already installed
if ! command -v Rscript &> /dev/null; then
  echo "Installing R..."
  apt-get update -qq
  apt-get install -y -qq --no-install-recommends \
    ca-certificates \
    r-base \
    r-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev
  rm -rf /var/lib/apt/lists/*
fi

# Install R packages needed for development
echo "Installing R packages..."
REPO="${METASALMON_CRAN_REPO:-https://cloud.r-project.org}"
case "$REPO" in
  https://*) ;;
  *)
    echo "METASALMON_CRAN_REPO must use https://"
    exit 1
    ;;
esac

Rscript -e "
  options(
    repos = c(CRAN = '${REPO}'),
    download.file.method = 'libcurl',
    timeout = max(300, getOption('timeout'))
  )

  # Keep bootstrap surface small; remotes handles DESCRIPTION dependency install.
  bootstrap <- c('remotes', 'testthat', 'roxygen2')
  for (pkg in bootstrap) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
    }
  }

  # Install package dependencies
  if (file.exists('DESCRIPTION')) {
    remotes::install_deps(dependencies = TRUE, upgrade = 'never', quiet = TRUE)
  }

  cat('R packages installed successfully\n')
"

echo "R setup complete!"
exit 0
