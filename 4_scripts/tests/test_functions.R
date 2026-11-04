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
  # defaults: 1-15 high, 16-40 moderate, >= 41 low
  y <- classify_count(c(0, 15, 16, 40, 41))
  expect_equal(as.character(y), COUNT_CLASS_LEVELS[c(1, 2, 3, 3, 4)])
  expect_false("LIVING_SPECIMEN" %in% GBIF_BASIS_COUNTED)
})

test_that("bulk JSON parsers match the rgbif path", {
  skip_if_not(live)
  for (nm in c("Malus domestica", "Picea excelsa", "Prunus dulcis", "Agrilus marcopoli", "Nosuchius fakeus")) {
    txt <- httr2::resp_body_string(httr2::req_perform(.gbif_req("/species/match", list(name = nm, verbose = "true"))))
    a <- .gbif_match_from_json(nm, NULL, txt); b <- .gbif_match_impl(nm, NULL)
    expect_equal(a$acceptedKey, b$acceptedKey, info = nm)
    expect_equal(a$gbif_canonical, b$gbif_canonical, info = nm)
  }
  # summary counts only the counted record types, and years/counties refer to the same records
  s <- .gbif_no_summary_impl("3001244")
  expect_false(any(c("LIVING_SPECIMEN", "FOSSIL_SPECIMEN") %in% names(s$basis)))
  expect_equal(s$total, sum(s$basis))
  all_types <- rgbif::occ_search(taxonKey = 3001244, country = "NO", limit = 0)$meta$count
  expect_lt(s$total, all_types)
  expect_gt(length(s$years), 20); expect_gt(s$n_counties, 5)
})

test_that("points query ORs basisOfRecord and respects the cap", {
  skip_if_not(live)
  p <- .gbif_points_impl("3001244", 350, c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
  expect_lte(nrow(p$data), 350); expect_gt(p$n_available, 1000)
  expect_true(all(p$data$basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")))
})

test_that("EPPO overview gives the preferred name", {
  skip_if_not(live && nzchar(key))
  o <- eppo_overview("ERWIAM", key); expect_equal(o$prefname, "Erwinia amylovora")
  p <- get_pest_hosts("ERWIAM", key)          # code entered: name comes from EPPO, GBIF matched on it
  expect_equal(p$eppo$prefname, "Erwinia amylovora"); expect_equal(p$gbif$gbif_canonical, "Erwinia amylovora")
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
  p <- gbif_match("Picea excelsa", kingdom = "Plantae")      # homonyms -> never guessed
  expect_equal(p$matchType, "AMBIGUOUS"); expect_true(is.na(p$acceptedKey))
  d <- gbif_match("Prunus dulcis", kingdom = "Plantae")      # almond vs. an old cherry homonym
  expect_equal(resolve_with_author(d, "(Miller) Webb")$gbif_canonical, "Prunus amygdalus")
  expect_true(is.na(resolve_with_author(d, "Nobody")$acceptedKey))
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
