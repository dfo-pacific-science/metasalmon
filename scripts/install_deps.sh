#!/bin/bash
# Install dependencies for metasalmon R package development
# This script runs on SessionStart via Claude Code hooks

set -e

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
  apt-get install -y -qq r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
fi

# Install R packages needed for development
echo "Installing R packages..."
Rscript -e "
  options(repos = c(CRAN = 'https://cloud.r-project.org'))

  # Install devtools and testthat for package development
  if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')
  if (!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat')
  if (!requireNamespace('roxygen2', quietly = TRUE)) install.packages('roxygen2')

  # Install package dependencies
  if (file.exists('DESCRIPTION')) {
    devtools::install_deps(dependencies = TRUE, upgrade = 'never')
  }

  cat('R packages installed successfully\n')
"

echo "R setup complete!"
exit 0
