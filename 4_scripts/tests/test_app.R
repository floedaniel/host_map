# End-to-end app test (headless Chrome via shinytest2). Needs network + EPPO_API_KEY.
library(testthat)
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

test_that("app searches a pest, fills the table and maps points", {
  skip_if(identical(Sys.getenv("HOSTMAP_OFFLINE"), "true") || !nzchar(Sys.getenv("EPPO_API_KEY")))
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(root, name = "hostmap", load_timeout = 60000, timeout = 240000)
  on.exit(app$stop(), add = TRUE)
  app$set_inputs(pest = "Agrilus planipennis")
  app$click("go")
  app$wait_for_value(output = "summary_boxes", timeout = 240000)
  cats <- app$get_value(input = "cats")
  expect_true("Major host" %in% cats)
  app$wait_for_idle(3000, timeout = 120000)
  expect_equal(app$get_value(input = "map_cats"), "Major host")   # map default
  app$set_inputs(tabs = "Map"); Sys.sleep(5)   # leaflet draws paths only when visible
  n_auto <- app$get_js("document.querySelectorAll('#hostmap path.leaflet-interactive').length")
  app$set_inputs(tabs = "Host plants")
  expect_gt(n_auto, 0)   # map drawn automatically after search
  html <- app$get_html("#tbl")
  expect_match(html, "Fraxinus excelsior")
  expect_match(html, "Low uncertainty|Moderate uncertainty|High uncertainty")
  # category filter: drop all but Major host
  app$set_inputs(cats = "Major host")
  app$wait_for_idle(5000)
  expect_equal(app$get_value(input = "cats"), "Major host")
  # map with default host selection
  app$click("load_map")
  app$wait_for_idle(10000, timeout = 120000)
  expect_match(app$get_html("#hostmap"), "leaflet")
  expect_true(any(grepl("Error", app$get_logs()$message)) == FALSE)
  # downloads
  csv <- app$get_download("dl_csv")
  d <- read.csv(csv)
  expect_true(all(c("host_plant", "eppo_category", "n_records", "uncertainty_class", "ref_status") %in% names(d)))
  expect_true(all(d$eppo_category == "Major host"))
  xl <- app$get_download("dl_xlsx")
  expect_gt(file.size(xl), 1000)
})

test_that("missing taxon gives a warning, not a crash", {
  skip_if(identical(Sys.getenv("HOSTMAP_OFFLINE"), "true") || !nzchar(Sys.getenv("EPPO_API_KEY")))
  app <- shinytest2::AppDriver$new(root, name = "hostmap-missing", load_timeout = 60000, timeout = 60000)
  on.exit(app$stop(), add = TRUE)
  app$set_inputs(pest = "Nosuchius fakeus")
  app$click("go")
  app$wait_for_value(output = "messages", timeout = 60000)
  expect_match(app$get_html("#messages"), "No EPPO code found")
})
