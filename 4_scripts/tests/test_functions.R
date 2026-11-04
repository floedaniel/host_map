# Unit + live API tests. Run:  Rscript -e 'testthat::test_dir("4_scripts/tests")'  (from project root)
library(testthat)
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "4_scripts", "hostmap_functions.R"))
hm_init_cache(file.path(root, "2_processed_data", "cache"))
live <- !identical(Sys.getenv("HOSTMAP_OFFLINE"), "true")
key <- Sys.getenv("EPPO_API_KEY")

test_that("count classification follows thresholds", {
  x <- classify_count(c(0, 1, 10, 11, 50, 51, NA), 10, 50)
  expect_equal(as.character(x), c(COUNT_CLASS_LEVELS[c(1, 2, 2, 3, 3, 4)], NA))
  expect_equal(as.character(classify_count(c(5, 6), 5, 6)), COUNT_CLASS_LEVELS[c(2, 3)])
})

test_that("revised classification downgrades poorly spread high counts", {
  x <- classify_revised(c(500, 500, 500, 30, 0), c(30, 3, 30, 30, 0), c(10, 10, 1, 10, 0))
  expect_equal(as.character(x), COUNT_CLASS_LEVELS[c(4, 3, 3, 3, 1)])
})

test_that("summary_row respects basisOfRecord filter", {
  s <- list(total = 100L, basis = c(HUMAN_OBSERVATION = 60L, PRESERVED_SPECIMEN = 30L, LIVING_SPECIMEN = 10L),
            years = c(1990L, 2000L, 2010L), n_counties = 3L)
  r <- summary_row(s, c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
  expect_equal(r$n_records, 90L); expect_equal(r$n_living, 10L); expect_equal(r$n_years, 3L)
  expect_equal(r$n_years_since_2000, 2L)
})

test_that("clean_points de-duplicates and filters precision", {
  d <- data.frame(decimalLatitude = c(60, 60, 61, 62, 63), decimalLongitude = c(10, 10, 11, 12, 13),
                  year = c(2000, 2000, 2001, 2002, 2003), recordedBy = c("a", "a", "b", "c", "d"),
                  coordinateUncertaintyInMeters = c(10, 10, 5000, NA, 100), basisOfRecord = "HUMAN_OBSERVATION")
  expect_equal(nrow(clean_points(d, 1000, TRUE)), 3)
  expect_equal(nrow(clean_points(d, 1000, FALSE)), 2)
  expect_equal(nrow(clean_points(d, 1000, TRUE, max_points = 1)), 1)
})

test_that("strip_name removes html and hybrid signs", {
  expect_equal(strip_name("<i>Malus </i>×<i>domestica</i>"), "Malus domestica")
  expect_equal(strip_name("Citrus x aurantium"), "Citrus aurantium")
})

test_that("reference lookup maps names to status", {
  ref <- readRDS(file.path(root, "2_processed_data", "artsdatabanken_reference.rds"))
  r <- lookup_reference(ref, c("Pyrus communis", "Coffea arabica", "Notaname"), c(NA, NA, "Malus domestica"))
  expect_equal(as.character(r$ref_status[1]), "Established")
  expect_true(is.na(r$ref_status[2]))
  expect_false(is.na(r$ref_status[3]))
})

test_that("GBIF resolves synonyms and refuses higher-rank fallbacks", {
  skip_if_not(live)
  m <- gbif_match("Agrilus marcopoli")
  expect_equal(m$status, "SYNONYM"); expect_equal(m$gbif_canonical, "Agrilus planipennis")
  p <- gbif_match("Picea excelsa", kingdom = "Plantae")
  expect_equal(p$gbif_canonical, "Picea abies")
  f <- gbif_match("Nosuchius fakeus", kingdom = "Plantae")
  expect_true(is.na(f$acceptedKey))
})

test_that("GBIF Norway summary returns counts and facets", {
  skip_if_not(live)
  s <- gbif_no_summary("3001244")   # Malus domestica
  expect_gt(s$total, 1000); expect_gt(length(s$years), 20); expect_gt(s$n_counties, 5)
})

test_that("EPPO pipeline works and reports problems clearly", {
  skip_if_not(live && nzchar(key))
  p <- get_pest_hosts("Erwinia amylovora", key)
  expect_equal(p$eppo$code, "ERWIAM"); expect_gt(nrow(p$hosts), 20)
  expect_true("Major host" %in% p$hosts$class_label)
  expect_error(eppo_hosts("ERWIAM", "invalid-key-xyz"), "rejected")
  expect_error(.eppo_get("/x", ""), "missing")
  nf <- get_pest_hosts("Nosuchius fakeus", key)
  expect_true(is.na(nf$eppo$code)); expect_true(length(nf$messages) > 0)
  s <- summarise_hosts(head(p$hosts, 3), ref = NULL)
  expect_equal(nrow(s), 3); expect_true(all(c("uncertainty_class", "gbif_link", "artskart_link") %in% names(s)))
})
