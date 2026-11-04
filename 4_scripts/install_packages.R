# =============================================================================
# install_packages.R: install the R packages Host Map needs.
# On the Shiny Server, run as a user who can write to the site library that the
# 'shiny' user reads (usually: sudo Rscript 4_scripts/install_packages.R).
# On Linux it uses Posit Package Manager binaries (fast, no compilation).
# =============================================================================

needed <- c(
  # app
  shiny = "1.8.1", bslib = "0.7.0", DT = "0.30", leaflet = "2.2.0", htmltools = "0.5.7",
  markdown = "1.0", viridisLite = "0.4.2", writexl = "1.4",
  # data access
  rgbif = "3.7.9", httr2 = "1.0.0", jsonlite = "1.8.0", dplyr = "1.1.0",
  memoise = "2.0.1", cachem = "1.0.8", rlang = "1.1.0",
  # background searches
  future = "1.33.0", promises = "1.3.0"
)
optional_dev <- c("testthat", "shinytest2", "callr", "httpuv", "ggplot2", "irr", "scales")  # tests + validation scripts

repos <- getOption("repos")
if (.Platform$OS.type == "unix" && file.exists("/etc/os-release")) {
  os <- readLines("/etc/os-release")
  codename <- sub("^VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", os, value = TRUE))
  if (length(codename) && nzchar(codename) && any(grepl("ubuntu|debian", os, ignore.case = TRUE))) {
    repos <- c(P3M = sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename))
    options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
                                    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
  }
}
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")

# Check installed versions without loading packages (loading in the same session
# that installs them can fail spuriously).
too_old <- function(p, v) {
  have <- suppressWarnings(tryCatch(utils::packageDescription(p, fields = "Version"), error = function(e) NA))
  is.na(have) || package_version(have) < package_version(v)
}
todo <- names(needed)[mapply(too_old, names(needed), needed)]
if ("--dev" %in% commandArgs(TRUE)) todo <- c(todo, optional_dev[vapply(optional_dev, too_old, TRUE, v = "0")])
if (length(todo)) {
  message("Installing: ", paste(todo, collapse = ", "))
  install.packages(todo, repos = repos)
}
bad <- names(needed)[mapply(too_old, names(needed), needed)]
if (length(bad)) stop("Still missing or too old: ", paste(bad, collapse = ", "))
message("All Host Map packages are installed (R ", getRversion(), ").")
