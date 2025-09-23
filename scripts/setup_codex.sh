curl -L https://rig.r-pkg.org/deb/rig.gpg -o /etc/apt/trusted.gpg.d/rig.gpg
echo "deb http://rig.r-pkg.org/deb rig main" > /etc/apt/sources.list.d/rig.list
apt-get update
apt-get install -y r-rig
rig add release


# Interactively asked codex how many cores are available
# it ran nproc and got back 5
cat >> ~/.Renviron <<'EOF'
NOT_CRAN=true
TESTTHAT_CPUS=5
EOF

cat >> ~/.Rprofile <<'EOF'
options(
  repos = c(CRAN = sprintf(
    "https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s",
    R.version["arch"], substr(getRversion(), 1, 3)
  )),
  Ncpus = 4L,
  testthat.use_colours = FALSE,
  testthat.summary.omit_dots = TRUE
)
EOF


Rscript - <<'EOF'

desc <- read.dcf("DESCRIPTION")
pkgs <- desc[1, c("Depends", "Imports", "Suggests")] |>
  strsplit(",", fixed = TRUE) |>
  unlist(use.names = FALSE) |>
  trimws() |>
  sub("\\s*\\(.*\\)", "", x = _) |>
  setdiff(unname(c(
    "R",
    installed.packages(priority = c("base", "recommended"))[, "Package"]
  )))
install.packages(c("remotes", "devtools", pkgs))
remotes::install_local()

EOF
