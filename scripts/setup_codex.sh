#!/usr/bin/env bash
#
# This script sets up the openai/codex-universal docker image
# with everything needed to run quickr tests.
# Run this script as the "Setup script" when configuring an environment in
# https://chatgpt.com/codex/settings/environments,
#  ./scripts/setup_codex.sh
#
# More info:
# https://platform.openai.com/docs/codex/overview#default-universal-image

set -eo pipefail


if ! grep -q 'NOT_CRAN=true' ~/.bashrc; then
  echo 'export NOT_CRAN=true' >> ~/.bashrc
fi

export DEBIAN_FRONTEND=noninteractive

### --- setup instructions from https://cran.r-project.org/bin/linux/ubuntu/#root ----
# update indices
apt update -qq
# install two helper packages we need
apt install -y --no-install-recommends software-properties-common dirmngr
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the repo from CRAN -- lsb_release adjusts to 'noble' or 'jammy' or ... as needed
add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
# install R itself
sudo apt install -y --no-install-recommends r-base
### --- end setup instructions from https://cran.r-project.org/bin/linux/ubuntu/#root ----


### --- https://raw.githubusercontent.com/eddelbuettel/r2u/master/inst/scripts/add_cranapt_noble.sh ---
### lightly modified to avoid using gpg, which doesn't use the codex proxy
### correctly and errors
#!/bin/bash

## See the README.md of 'r2u' for details on these steps
##
## This script has been tested on a plain and minimal ubuntu:24.04
##
## On a well-connected machine this script should take well under one minute
##
## Note that you need to run this as root, or run the whole script via sudo
## To run individual commands as root, prefix each command with sudo and use
## 'echo | sudo tee file' as the command before the EOF redirect statement

set -eu

## First: update apt and get keys
apt update -qq && apt install --yes --no-install-recommends ca-certificates gnupg
## use gpg directly instead of the now-deprecated apt-key command
# gpg --homedir /tmp --no-default-keyring --keyring /usr/share/keyrings/r2u.gpg --keyserver keyserver.ubuntu.com --recv-keys A1489FE2AB99A21A 67C2D66C4B1D4339 51716619E084DAB9
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
    | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc

## Second: add the repo -- here we use the well-connected mirror
echo > /etc/apt/sources.list.d/r2u.sources <<EOF
Types: deb
URIs: https://r2u.stat.illinois.edu/ubuntu
Suites: noble
Components: main
Arch: amd64, arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF

## Third: ensure current R is used
echo > /etc/apt/sources.list.d/cran.sources <<EOF
Types: deb
URIs: https://cloud.r-project.org/bin/linux/ubuntu
Suites: noble-cran40/
Components:
Arch: amd64, arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF
apt update -qq
DEBIAN_FRONTEND=noninteractive apt install --yes --no-install-recommends r-base-core

## Fourth: add pinning to ensure package sorting
echo > /etc/apt/preferences.d/99cranapt <<EOF
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
EOF

## Fifth: install bspm (and its Python requirements) and enable it
## If needed (in bare container, say) install python tools for bspm and R itself
apt install --yes --no-install-recommends python3-{dbus,gi,apt} make
## Then install bspm (as root) and enable it, and enable a speed optimization
Rscript -e 'install.packages("bspm")'
echo >> /etc/R/Rprofile.site <<EOF
suppressMessages(bspm::enable())
options(bspm.version.check=FALSE)
EOF

# Done!

### ---- END https://raw.githubusercontent.com/eddelbuettel/r2u/master/inst/scripts/add_cranapt_noble.sh ---

### Now setup the actual dependencies for the quickr package

apt-get install -y gfortran gcc
apt-get install -y --no-install-recommends r-cran-{glue,testthat}
# apt-get install -y --no-install-recommends r-cran-{dotty,s7}  ## not available on r2u noble ??
R -q -e 'install.packages(c("dotty", "S7"))'
apt-get install -y --no-install-recommends r-cran-devtools  # not strictly necessary, but pulls in load_all(), cli, ...



echo >> ~/.Rprofile <<'EOF'
options(
  testthat.use_colours = FALSE,
  # testthat.summary.max_reports: The maximum number of detailed test reports printed for the summary reporter (default: 10).
  testthat.summary.omit_dots = TRUE
)
EOF

# R -e 'devtools::test()'
