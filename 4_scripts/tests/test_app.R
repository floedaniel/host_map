# End-to-end app tests (headless Chrome via shinytest2). Need network + EPPO_API_KEY.
# HOSTMAP_APP_URL: test an already running app (e.g. the app started on Linux
# under WSL, or on the Shiny Server) instead of starting one locally.
library(testthat)
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
app_target <- function() { u <- Sys.getenv("HOSTMAP_APP_URL"); if (nzchar(u)) u else root }
skip_live <- function() {
  skip_if(identical(Sys.getenv("HOSTMAP_OFFLINE"), "true") || !nzchar(Sys.getenv("EPPO_API_KEY")))
  skip_if_not_installed("shinytest2")
}
# Leaflet draws markers on a canvas (preferCanvas), so count layers in the map
n_markers <- function(app) app$get_js(
  "(function(){var w=HTMLWidgets.find('#hostmap'); if(!w) return 0; var n=0;
    w.getMap().eachLayer(function(l){ if (l instanceof L.CircleMarker) n++; }); return n;})()")
search <- function(app, pest, timeout = 400000) {
  app$set_inputs(pest = pest, wait_ = FALSE)
  app$click("go", wait_ = FALSE)
  app$wait_for_js("document.querySelector('#summary_boxes .hm-pest-name') !== null ||
                   (document.querySelector('#messages') && document.querySelector('#messages').innerText.length > 0)",
                  timeout = timeout)
  app$wait_for_idle(2000, timeout = timeout)
}

test_that("app searches a pest, fills the table and maps points", {
  skip_live()
  app <- shinytest2::AppDriver$new(app_target(), name = "hostmap", load_timeout = 60000, timeout = 240000)
  on.exit(app$stop(), add = TRUE)
  search(app, "Agrilus planipennis")
  expect_match(app$get_html("#summary_boxes"), "Agrilus planipennis")
  cats <- app$get_value(input = "cats")
  expect_true("Major host" %in% cats)
  expect_equal(app$get_value(input = "map_cats"), "Major host")   # map default
  app$set_inputs(tabs = "Map"); Sys.sleep(5)
  expect_gt(n_markers(app), 0)                                     # map drawn automatically after search
  app$set_inputs(tabs = "Host plants")
  html <- app$get_html("#tbl")
  expect_match(html, "Fraxinus excelsior")
  expect_match(html, "Low uncertainty|Moderate uncertainty|High uncertainty")
  # category filter: drop all but Major host
  app$set_inputs(cats = "Major host")
  app$wait_for_idle(5000)
  expect_equal(app$get_value(input = "cats"), "Major host")
  app$click("load_map")
  app$wait_for_idle(10000, timeout = 120000)
  expect_match(app$get_html("#hostmap"), "leaflet")
  # downloads
  csv <- app$get_download("dl_csv")
  d <- read.csv(csv)
  expect_true(all(c("host_plant", "eppo_category", "n_records", "uncertainty_class", "ref_status") %in% names(d)))
  expect_true(all(d$eppo_category == "Major host"))
  xl <- app$get_download("dl_xlsx")
  expect_gt(file.size(xl), 1000)
  if (!nzchar(Sys.getenv("HOSTMAP_APP_URL")))
    expect_false(any(grepl("^Warning: Error", app$get_logs()$message)))
})

test_that("missing taxon gives a warning, not a crash", {
  skip_live()
  app <- shinytest2::AppDriver$new(app_target(), name = "hostmap-missing", load_timeout = 60000, timeout = 60000)
  on.exit(app$stop(), add = TRUE)
  search(app, "Nosuchius fakeus", timeout = 60000)
  expect_match(app$get_html("#messages"), "No EPPO code found")
})

test_that("a long search does not block other users of the same app process", {
  skip_live()
  target <- app_target()
  if (!nzchar(Sys.getenv("HOSTMAP_APP_URL"))) {
    # Start ONE app process and connect two browser sessions to it, as on Shiny Server
    port <- httpuv::randomPort()
    proc <- callr::r_bg(function(dir, port) shiny::runApp(dir, port = port, launch.browser = FALSE),
                        args = list(root, port), env = c(callr::rcmd_safe_env(), EPPO_API_KEY = Sys.getenv("EPPO_API_KEY")))
    on.exit(proc$kill(), add = TRUE)
    target <- sprintf("http://127.0.0.1:%d", port)
    for (i in 1:60) { ok <- tryCatch({ suppressWarnings(readLines(target, warn = FALSE)); TRUE }, error = function(e) FALSE); if (ok) break; Sys.sleep(1) }
  }
  a <- shinytest2::AppDriver$new(target, name = "user-a", load_timeout = 60000, timeout = 400000)
  b <- shinytest2::AppDriver$new(target, name = "user-b", load_timeout = 60000, timeout = 60000)
  on.exit({ a$stop(); b$stop() }, add = TRUE)
  # user A: a large host list (several hundred hosts; slow when not cached)
  a$set_inputs(pest = "Spodoptera frugiperda", wait_ = FALSE); a$click("go", wait_ = FALSE)
  Sys.sleep(4)
  # user B: the app must still answer quickly while A's search runs
  t0 <- Sys.time()
  b$set_inputs(pest = "Nosuchius fakeus", wait_ = FALSE); b$click("go", wait_ = FALSE)
  b$wait_for_js("document.querySelector('#messages') && document.querySelector('#messages').innerText.length > 0", timeout = 60000)
  waited <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_match(b$get_html("#messages"), "No EPPO code found")
  expect_lt(waited, 20)
  # and user A's search still completes
  a$wait_for_js("document.querySelector('#summary_boxes .hm-pest-name') !== null", timeout = 600000)
  expect_match(a$get_html("#summary_boxes"), "Spodoptera frugiperda")
})
