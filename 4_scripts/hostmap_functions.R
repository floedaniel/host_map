# =============================================================================
# hostmap_functions.R
# Shared functions for the Host Map Shiny app and the validation scripts.
# All network calls are wrapped in memoise() with a disk cache so repeated
# queries (same pest, same host, same session or a later one) are not re-sent.
# =============================================================================

suppressPackageStartupMessages({
  library(rgbif)
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(memoise)
  library(cachem)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- Cache -------------------------------------------------------------------
# Call hm_init_cache() once (the app and validation scripts do this) to set the
# cache directory and max age. Functions below are memoised lazily against it.
.hm <- new.env(parent = emptyenv())

hm_init_cache <- function(dir = file.path(tempdir(), "hostmap_cache"),
                          max_age_days = 30) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  .hm$cache <- cachem::cache_disk(dir, max_age = max_age_days * 86400)
  .hm$memo <- list()
  invisible(.hm$cache)
}

# memoise wrapper that is created on first use against the current cache.
# Errors are never cached (memoise only stores successful returns).
hm_memo <- function(fname, f) {
  function(...) {
    if (is.null(.hm$cache)) hm_init_cache()
    if (is.null(.hm$memo[[fname]])) .hm$memo[[fname]] <- memoise::memoise(f, cache = .hm$cache)
    .hm$memo[[fname]](...)
  }
}

# ---- Constants ---------------------------------------------------------------
GBIF_BASIS_ALL <- c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "OCCURRENCE",
                    "MACHINE_OBSERVATION", "MATERIAL_SAMPLE", "OBSERVATION",
                    "MATERIAL_CITATION", "LIVING_SPECIMEN", "FOSSIL_SPECIMEN")

EPPO_CATEGORY_LEVELS <- c("Major host", "Host", "Minor host", "Experimental",
                          "Incidental", "Artificial", "Wild/Weed",
                          "Doubtful host", "Unclassified")

# Records counted for the uncertainty class (validated choice): everything except
# living specimens (botanic gardens) and fossils.
GBIF_BASIS_COUNTED <- setdiff(GBIF_BASIS_ALL, c("LIVING_SPECIMEN", "FOSSIL_SPECIMEN"))

COUNT_CLASS_LEVELS <- c("Not documented in Norway", "High uncertainty",
                        "Moderate uncertainty", "Low uncertainty")

# ---- GBIF name resolution ----------------------------------------------------

.empty_match <- function(name, note) {
  data.frame(input_name = name, usageKey = NA_character_, acceptedKey = NA_character_,
             gbif_name = NA_character_, gbif_canonical = NA_character_,
             rank = NA_character_, status = NA_character_, matchType = "NONE",
             kingdom = NA_character_, note = note, stringsAsFactors = FALSE)
}

.is_binomial <- function(x) grepl("^[A-Z][a-z-]+ (x |×)?[a-z-]+$", trimws(x))

# Core matching logic, shared by the rgbif path (single names, e.g. the pest)
# and the bulk JSON path (host lists). `b` is the backbone match (one row),
# `alt` the verbose alternatives (data frame, may be empty).
.gbif_match_logic <- function(name, kingdom, b, alt, usage_lookup) {
  g <- function(col) if (col %in% names(b) && length(b[[col]])) as.character(b[[col]][1]) else NA_character_
  mt <- g("matchType") %||% "NONE"
  note <- ""
  # GBIF falls back to a higher rank when a name is an ambiguous synonym (e.g.
  # "Picea excelsa" -> Pinaceae). Inspect the exact-match alternatives then.
  if (mt %in% c("HIGHERRANK", "NONE")) {
    if (NROW(alt) > 0 && "matchType" %in% names(alt)) {
      alt <- alt[alt$matchType %in% "EXACT", , drop = FALSE]
      if (!is.null(kingdom) && "kingdom" %in% names(alt)) alt <- alt[alt$kingdom %in% kingdom, , drop = FALSE]
    }
    if (NROW(alt) > 0) {
      acc <- if ("acceptedUsageKey" %in% names(alt)) ifelse(is.na(alt$acceptedUsageKey), alt$usageKey, alt$acceptedUsageKey) else alt$usageKey
      # For a binomial input, roll infraspecific accepted taxa up to the species
      if (.is_binomial(name) && "speciesKey" %in% names(alt))
        acc <- ifelse(is.na(alt$speciesKey), acc, alt$speciesKey)
      acc <- as.character(acc)
      top <- names(sort(table(acc), decreasing = TRUE))[1]
      pick <- alt[which(acc == top)[1], , drop = FALSE]
      acc_info <- usage_lookup(top)
      note <- if (length(unique(acc)) > 1)
        sprintf("Ambiguous synonym: %d candidate accepted taxa; chose the most frequent (%s)",
                length(unique(acc)), acc_info$scientificName %||% top) else "Resolved via alternative exact match"
      return(data.frame(
        input_name = name, usageKey = as.character(pick$usageKey), acceptedKey = top,
        gbif_name = acc_info$scientificName %||% NA_character_,
        gbif_canonical = acc_info$canonicalName %||% NA_character_,
        rank = as.character(pick$rank), status = as.character(pick$status),
        matchType = if (length(unique(acc)) > 1) "AMBIGUOUS" else "EXACT",
        kingdom = as.character(pick$kingdom %||% NA), note = note, stringsAsFactors = FALSE))
    }
    if (mt == "NONE") return(.empty_match(name, "No GBIF match"))
    # A higher-rank fallback (e.g. a misspelt binomial matched to its genus or to
    # the kingdom) must not be counted as the host: leave the key empty.
    out <- .empty_match(name, sprintf("Only matched at rank %s (%s); not counted", g("rank"), g("scientificName")))
    out$matchType <- "HIGHERRANK"; out$rank <- g("rank")
    return(out)
  }
  status <- g("status")
  accepted_key <- if (identical(status, "SYNONYM") && !is.na(g("acceptedUsageKey"))) g("acceptedUsageKey") else g("usageKey")
  gbif_name <- if (identical(status, "SYNONYM")) g("acceptedScientificName") %||% g("species") %||% g("scientificName") else g("scientificName")
  canon <- g("canonicalName")
  if (identical(status, "SYNONYM")) {
    note <- paste0("Synonym of ", gbif_name)
    if (.is_binomial(name) && !is.na(g("speciesKey"))) {
      accepted_key <- g("speciesKey"); canon <- g("species") %||% canon
    } else canon <- sub("^(\\S+ \\S+( (subsp|var|f)\\. \\S+)?).*$", "\\1", gbif_name)
  }
  data.frame(input_name = name, usageKey = g("usageKey"), acceptedKey = accepted_key,
             gbif_name = gbif_name, gbif_canonical = canon, rank = g("rank"),
             status = status, matchType = mt, kingdom = g("kingdom"), note = note,
             stringsAsFactors = FALSE)
}

.usage_lookup <- function(key) tryCatch(rgbif::name_usage(key = key)$data, error = function(e) NULL)

# rgbif path (used for single names such as the pest, and as fallback)
.gbif_match_impl <- function(name, kingdom = NULL) {
  name <- trimws(name)
  if (!nzchar(name)) return(.empty_match(name, "Empty name"))
  b <- tryCatch(as.data.frame(rgbif::name_backbone(name = name, kingdom = kingdom)),
                error = function(e) stop("GBIF name lookup failed: ", conditionMessage(e)))
  mt <- if ("matchType" %in% names(b)) b$matchType[1] else "NONE"
  alt <- data.frame()
  if (mt %in% c("HIGHERRANK", "NONE")) {
    v <- tryCatch(rgbif::name_backbone_verbose(name = name, kingdom = kingdom), error = function(e) NULL)
    if (!is.null(v)) alt <- as.data.frame(v$alternatives)
  }
  .gbif_match_logic(name, kingdom, b, alt, .usage_lookup)
}

# Parse a GBIF v1 /species/match?verbose=true JSON response (bulk path)
.gbif_match_from_json <- function(name, kingdom, txt) {
  j <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  alt <- if (!is.null(j$alternatives)) as.data.frame(j$alternatives) else data.frame()
  j$alternatives <- NULL
  b <- as.data.frame(lapply(j, function(x) if (length(x)) x[1] else NA), stringsAsFactors = FALSE)
  .gbif_match_logic(trimws(name), kingdom, b, alt, .usage_lookup)
}

# ---- Key-value cache helpers (shared by single and bulk paths) ---------------
.ckey <- function(...) paste0("k", rlang::hash(list(...)))
.cache_get <- function(...) {
  if (is.null(.hm$cache)) hm_init_cache()
  v <- .hm$cache$get(.ckey(...)); if (cachem::is.key_missing(v)) NULL else v
}
.cache_set <- function(value, ...) { if (is.null(.hm$cache)) hm_init_cache(); .hm$cache$set(.ckey(...), value); value }

gbif_match <- function(name, kingdom = NULL) {
  name <- trimws(name)
  v <- .cache_get("match", name, kingdom); if (!is.null(v)) return(v)
  .cache_set(.gbif_match_impl(name, kingdom), "match", name, kingdom)
}

# ---- EPPO Global Database API (v2) -------------------------------------------
EPPO_BASE <- "https://api.eppo.int/gd/v2"

.eppo_get <- function(path, api_key, query = list()) {
  if (is.null(api_key) || !nzchar(api_key))
    stop("EPPO API key is missing. Set EPPO_API_KEY at the top of app.R (or the EPPO_API_KEY environment variable). Register for a free key at https://data.eppo.int/")
  req <- request(paste0(EPPO_BASE, path)) |>
    req_headers(`X-Api-Key` = api_key) |>
    req_url_query(!!!query) |>
    req_timeout(60) |>
    req_retry(max_tries = 3, is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503)) |>
    req_error(is_error = function(r) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) stop("EPPO API request failed: ", conditionMessage(e)))
  st <- resp_status(resp)
  if (st == 401 || st == 403) stop("EPPO API rejected the key (HTTP ", st, "): check EPPO_API_KEY.")
  if (st == 400 || st == 404) return(NULL)  # unknown code / route: treat as no data
  if (st >= 400) stop("EPPO API error HTTP ", st)
  fromJSON(resp_body_string(resp), simplifyVector = TRUE)
}

.eppo_name2codes_impl <- function(name, api_key) {
  x <- .eppo_get("/tools/name2codes", api_key, list(name = name))
  if (is.null(x) || length(x) == 0 || NROW(x) == 0) return(character(0))
  x <- as.data.frame(x)
  if ("preferred" %in% names(x)) x <- x[order(!x$preferred), , drop = FALSE]
  unique(x$eppocode)
}
eppo_name2codes <- hm_memo("eppo_name2codes", .eppo_name2codes_impl)

.eppo_hosts_impl <- function(code, api_key) {
  x <- .eppo_get(paste0("/taxons/taxon/", utils::URLencode(code), "/hosts"), api_key)
  if (is.null(x) || NROW(x) == 0)
    return(data.frame(eppocode = character(), prefname = character(), class_id = integer(),
                      class_label = character(), bibref = character()))
  x <- as.data.frame(x)
  for (col in c("eppocode", "prefname", "class_label", "bibref")) if (!col %in% names(x)) x[[col]] <- NA_character_
  x$class_label[is.na(x$class_label) | x$class_label == ""] <- "Unclassified"
  x[, c("eppocode", "prefname", "class_id", "class_label", "bibref")]
}
eppo_hosts <- hm_memo("eppo_hosts", .eppo_hosts_impl)

#' Resolve a pest to an EPPO code. Accepts an EPPO code directly, otherwise tries
#' the name as entered, then the GBIF accepted name(s).
eppo_resolve <- function(input, gbif = NULL, api_key) {
  input <- trimws(input)
  if (grepl("^[A-Z0-9]{5,6}$", input)) return(list(code = input, via = "EPPO code entered"))
  cands <- unique(na.omit(c(input, gbif$gbif_canonical, sub(" [A-Z(].*$", "", gbif$gbif_name %||% NA))))
  for (nm in cands) {
    codes <- eppo_name2codes(nm, api_key)
    if (length(codes)) return(list(code = codes[1], via = paste0("EPPO name2codes('", nm, "')"), all = codes))
  }
  list(code = NA_character_, via = "No EPPO code found")
}

# ---- GBIF Norway occurrence summary ------------------------------------------
# One request per taxon (limit = 0 => no records, only counts + facets).
.summary_from_parts <- function(total, basis, years, n_counties)
  list(total = as.integer(total %||% 0), basis = basis, years = years, n_counties = as.integer(n_counties))

.gbif_no_summary_impl <- function(taxonKey, country = "NO") {
  r <- rgbif::occ_search(taxonKey = taxonKey, country = country, limit = 0,
                         facet = c("basisOfRecord", "year", "gadmLevel1Gid"),
                         facetLimit = 500)
  f <- r$facets
  basis <- if (!is.null(f$basisOfRecord) && nrow(f$basisOfRecord)) setNames(f$basisOfRecord$count, f$basisOfRecord$name) else integer(0)
  yrs <- if (!is.null(f$year) && nrow(f$year)) as.integer(f$year$name) else integer(0)
  gadm <- f[[grep("gadm", names(f), value = TRUE)[1] %||% "none"]]
  .summary_from_parts(r$meta$count, basis, yrs, if (is.null(gadm)) 0L else nrow(gadm))
}

# Same summary from the raw GBIF v1 occurrence/search JSON (bulk path)
.summary_from_json <- function(txt) {
  j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  fc <- list(); for (f in j$facets) fc[[f$field]] <- f$counts
  nm <- function(x) vapply(x, function(e) as.character(e$name), "")
  ct <- function(x) vapply(x, function(e) as.numeric(e$count), 0)
  b <- fc$BASIS_OF_RECORD
  .summary_from_parts(j$count, if (length(b)) setNames(as.integer(ct(b)), nm(b)) else integer(0),
                      as.integer(nm(fc$YEAR)), length(fc$GADM_LEVEL_1_GID))
}

gbif_no_summary <- function(taxonKey, country = "NO") {
  v <- .cache_get("nosum", as.character(taxonKey), country); if (!is.null(v)) return(v)
  .cache_set(.gbif_no_summary_impl(taxonKey, country), "nosum", as.character(taxonKey), country)
}

#' Flatten a summary into a one-row data frame given the selected basisOfRecord set.
summary_row <- function(s, basis_keep = GBIF_BASIS_ALL) {
  b <- s$basis
  sel <- sum(b[names(b) %in% basis_keep])
  data.frame(
    n_total_all = s$total,
    n_records = as.integer(sel),
    n_human = as.integer(sum(b[names(b) %in% c("HUMAN_OBSERVATION", "OBSERVATION", "MACHINE_OBSERVATION")])),
    n_preserved = as.integer(sum(b[names(b) %in% c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE", "MATERIAL_CITATION")])),
    n_living = as.integer(sum(b[names(b) == "LIVING_SPECIMEN"])),
    n_years = length(s$years),
    n_years_since_2000 = sum(s$years >= 2000),
    first_year = if (length(s$years)) min(s$years) else NA_integer_,
    last_year = if (length(s$years)) max(s$years) else NA_integer_,
    n_counties = as.integer(s$n_counties)
  )
}

# ---- Classification ----------------------------------------------------------

#' Uncertainty class for documented occurrence, from the record count.
#' Default thresholds are the calibrated values from the validation
#' (5_outputs/REPORT.md): P(host really present in Norway) = 0.95 at the
#' high/moderate boundary and 0.99 at the moderate/low boundary.
HM_T1 <- 12L   # 1..12 records   -> high uncertainty
HM_T2 <- 37L   # 13..37 records  -> moderate uncertainty; >= 38 -> low uncertainty
classify_count <- function(n, t1 = HM_T1, t2 = HM_T2) {
  out <- ifelse(is.na(n), NA_character_,
         ifelse(n == 0, COUNT_CLASS_LEVELS[1],
         ifelse(n <= t1, COUNT_CLASS_LEVELS[2],
         ifelse(n <= t2, COUNT_CLASS_LEVELS[3], COUNT_CLASS_LEVELS[4]))))
  factor(out, levels = COUNT_CLASS_LEVELS)
}

# ---- Artsdatabanken reference ("established in Norwegian nature") -------------
# Separate concept from GBIF documented occurrence. Built from Artsdatabanken's
# open OData API: Norsk rødliste 2021 (vascular plants, mainland Norway) and
# Fremmedartslista 2023 (vascular plants, mainland Norway).

.odata_all <- function(entity, select, filter) {
  req <- request(paste0("https://lister.artsdatabanken.no/odata/v1/", entity)) |>
    req_url_query(`$select` = select, `$filter` = filter) |> req_timeout(120)
  out <- list(); i <- 0
  repeat {
    r <- req_perform(req) |> resp_body_string() |> fromJSON(flatten = TRUE)
    i <- i + 1; out[[i]] <- r$value
    nl <- r$`@odata.nextLink`
    if (is.null(nl)) break
    req <- request(nl) |> req_timeout(120)
  }
  bind_rows(out)
}

strip_name <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("×\\s*|\\bx\\s+", "", x)   # hybrid sign
  x <- gsub("\\s+", " ", trimws(x))
  x
}

#' Map Artsdatabanken codes to a three-level reference status.
#' Absent < Present, not established < Established
build_reference <- function(raw = NULL, path = NULL) {
  if (is.null(raw)) {
    rl <- .odata_all("SpeciesAssessment2021",
      "scientificName,category,assessmentInitialSubClassificationCode,speciesGroup,assessmentArea,taxonRank",
      "speciesGroup eq 'Karplanter' and assessmentArea eq 'N'")
    fab <- .odata_all("AlienSpeciesAssessment2023",
      "scientificName,speciesStatus,alienSpeciesCategory,category,expertGroup,evaluationContext",
      "expertGroup eq 'Karplanter' and evaluationContext eq 'N'")
    raw <- list(redlist = rl, alien = fab)
  }
  rl <- raw$redlist
  rl_map <- c(EtablertBestandINorge = "Established", `EtablertFør1800` = "Established",
              IkkeDokumentertForekomstNA = "Absent",
              ObservertReproduktivIkkeEtablertNA = "Present, not established",
              ObservertReproduserendeIkkeEtablertNA = "Present, not established",
              IkkeReproduserendeGjestNA = "Present, not established",
              UregelmessigGjestIkkeReproduserendeNA = "Present, not established",
              IkkeReproduserendeINorge = "Present, not established")
  rl_ref <- data.frame(name = strip_name(rl$scientificName),
                       source = "Red List 2021",
                       detail = rl$assessmentInitialSubClassificationCode,
                       ref_status = unname(rl_map[rl$assessmentInitialSubClassificationCode]),
                       stringsAsFactors = FALSE)
  fab <- raw$alien
  fab_map <- c(Abroad = "Absent", B1 = "Present, not established", B2 = "Present, not established",
               C0 = "Present, not established", C1 = "Present, not established",
               C2 = "Established", C3 = "Established")
  fab_ref <- data.frame(name = strip_name(fab$scientificName.scientificNameFormatted),
                        source = "Alien Species List 2023",
                        detail = paste0(fab$alienSpeciesCategory, ":", fab$speciesStatus),
                        ref_status = unname(fab_map[fab$speciesStatus]),
                        stringsAsFactors = FALSE)
  ref <- bind_rows(fab_ref, rl_ref) |> filter(!is.na(ref_status), nzchar(name))
  # Alien list takes precedence over the Red List's generic "FremmedArtNA"; when
  # a name appears twice with conflicting status keep the most "present" one.
  ref <- ref |>
    mutate(ord = match(ref_status, c("Absent", "Present, not established", "Established"))) |>
    group_by(name) |> slice_max(ord, n = 1, with_ties = FALSE) |> ungroup() |> select(-ord)
  ref$ref_status <- factor(ref$ref_status, levels = c("Absent", "Present, not established", "Established"))
  if (!is.null(path)) saveRDS(ref, path)
  ref
}

#' Look up reference status for host names (tries EPPO name, then GBIF canonical).
lookup_reference <- function(ref, eppo_name, gbif_canonical) {
  if (is.null(ref)) return(data.frame(ref_status = factor(rep(NA, length(eppo_name)), levels = c("Absent", "Present, not established", "Established")), ref_source = NA_character_, ref_detail = NA_character_))
  i <- match(strip_name(eppo_name), ref$name)
  j <- match(strip_name(gbif_canonical), ref$name)
  k <- ifelse(is.na(i), j, i)
  data.frame(ref_status = ref$ref_status[k], ref_source = ref$source[k], ref_detail = ref$detail[k])
}

# ---- Artsdatabanken taxon IDs / Artskart links -------------------------------
.nortaxa_id_impl <- function(name) {
  resp <- tryCatch(
    request("https://nortaxa.artsdatabanken.no/api/v1/TaxonName/Search") |>
      req_url_query(Search = name) |> req_timeout(20) |> req_perform(),
    error = function(e) NULL)
  if (is.null(resp)) return(NA_integer_)
  x <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  if (!length(x)) return(NA_integer_)
  # prefer an exact accepted-name match
  for (t in x) if (identical(t$acceptedScientificName$name, name)) return(as.integer(t$taxonId))
  as.integer(x[[1]]$taxonId)
}
nortaxa_id <- hm_memo("nortaxa_id", .nortaxa_id_impl)

gbif_url <- function(key) ifelse(is.na(key), NA_character_,
  sprintf("https://www.gbif.org/occurrence/search?taxon_key=%s&country=NO", key))

artskart_url <- function(taxon_id, name) {
  ifelse(!is.na(taxon_id),
    paste0("https://artskart.artsdatabanken.no/app/#map/427864,7623020/3/background/greyMap/filter/",
           utils::URLencode(sprintf('{"TaxonIds":[%s],"IncludeSubTaxonIds":true,"Found":[2],"NotRecovered":[2],"Style":1}', taxon_id), reserved = TRUE)),
    paste0("https://artsdatabanken.no/search?q=", utils::URLencode(name, reserved = TRUE)))
}

# ---- Occurrence points for the map ------------------------------------------
# One paged query per host with all selected basisOfRecord values OR-ed together
# (GBIF accepts repeated parameters); pages are fetched concurrently.
.gbif_points_impl <- function(taxonKey, max_points = 1000, basis = GBIF_BASIS_ALL, country = "NO", max_active = 3) {
  q <- list(taxonKey = taxonKey, country = country, hasCoordinate = "true", hasGeospatialIssue = "false",
            basisOfRecord = basis)
  first <- req_perform(.gbif_req("/occurrence/search", c(q, limit = 0)))
  if (resp_status(first) != 200) stop("GBIF occurrence search failed (HTTP ", resp_status(first), ")")
  n_avail <- jsonlite::fromJSON(resp_body_string(first))$count %||% 0
  n_get <- min(n_avail, max_points)
  if (n_get == 0) return(list(n_available = n_avail, data = NULL))
  offs <- seq(0, n_get - 1, by = 300)
  reqs <- lapply(offs, function(o) .gbif_req("/occurrence/search", c(q, limit = min(300, n_get - o), offset = o)))
  resp <- httr2::req_perform_parallel(reqs, on_error = "continue", max_active = max_active, progress = FALSE)
  keep <- c("key", "decimalLatitude", "decimalLongitude", "year", "recordedBy",
            "basisOfRecord", "coordinateUncertaintyInMeters", "scientificName")
  d <- bind_rows(lapply(resp, function(r) {
    txt <- .ok_body(r); if (is.null(txt)) return(NULL)
    x <- jsonlite::fromJSON(txt, simplifyVector = TRUE)$results
    if (!NROW(x)) return(NULL)
    for (k in keep) if (!k %in% names(x)) x[[k]] <- NA
    x <- x[, keep]
    x$recordedBy <- as.character(x$recordedBy); x$year <- as.integer(x$year)
    x$coordinateUncertaintyInMeters <- as.numeric(x$coordinateUncertaintyInMeters)
    x
  }))
  list(n_available = n_avail, data = d)
}

gbif_points <- function(taxonKey, max_points = 1000, basis = GBIF_BASIS_ALL, country = "NO") {
  basis <- sort(basis)
  v <- .cache_get("points", as.character(taxonKey), max_points, basis, country); if (!is.null(v)) return(v)
  .cache_set(.gbif_points_impl(taxonKey, max_points, basis, country), "points", as.character(taxonKey), max_points, basis, country)
}

#' Clean, precision-filter and de-duplicate fetched points.
clean_points <- function(d, max_uncertainty_m = 1000, keep_missing_uncertainty = TRUE, max_points = 2000) {
  if (is.null(d) || !nrow(d)) return(d)
  for (col in c("decimalLatitude", "decimalLongitude", "year", "recordedBy", "coordinateUncertaintyInMeters", "basisOfRecord"))
    if (!col %in% names(d)) d[[col]] <- NA
  d <- d |>
    mutate(coordinateUncertaintyInMeters = suppressWarnings(as.numeric(coordinateUncertaintyInMeters))) |>
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude),
           (is.na(coordinateUncertaintyInMeters) & keep_missing_uncertainty) |
             (!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= max_uncertainty_m)) |>
    distinct(decimalLatitude, decimalLongitude, year, recordedBy, .keep_all = TRUE)
  utils::head(d, max_points)
}

# ---- Main pipeline -----------------------------------------------------------

#' Resolve a pest and return its EPPO host list.
.eppo_overview_impl <- function(code, api_key) {
  ov <- .eppo_get(paste0("/taxons/taxon/", utils::URLencode(code), "/overview"), api_key)
  nm <- tryCatch(.eppo_get(paste0("/taxons/taxon/", utils::URLencode(code), "/names"), api_key), error = function(e) NULL)
  nm <- if (is.null(nm) || !NROW(nm)) data.frame() else as.data.frame(nm)
  pick <- function(langs) { if (!NROW(nm)) return(NA_character_)
    x <- nm[nm$lang_iso %in% langs, , drop = FALSE]; if (!nrow(x)) return(NA_character_)
    x <- x[order(!x$preferred), ]; x$fullname[1] }
  author <- if (NROW(nm)) { x <- nm[nm$lang_iso %in% "la" & nm$preferred %in% TRUE, ]; if (nrow(x)) x$author[1] else NA_character_ } else NA_character_
  list(prefname = ov$prefname %||% NA_character_, author = author %||% NA_character_,
       norwegian = pick(c("no", "nb", "nn")), english = pick("en"))
}
eppo_overview <- hm_memo("eppo_overview", .eppo_overview_impl)

#' Resolve a pest and return its EPPO host list. The displayed pest name is the
#' EPPO preferred name; GBIF is used for synonym resolution and the taxonKey.
get_pest_hosts <- function(pest_name, api_key) {
  msgs <- character(0)
  pest_name <- trimws(pest_name)
  is_code <- grepl("^[A-Z0-9]{5,6}$", pest_name)
  gb <- NULL
  if (!is_code) {
    gb <- tryCatch(gbif_match(pest_name), error = function(e) { msgs <<- c(msgs, conditionMessage(e)); NULL })
    if (is.null(gb) || gb$matchType == "NONE") msgs <- c(msgs, sprintf("GBIF could not resolve '%s'; trying EPPO with the name as entered.", pest_name))
    else if (gb$matchType == "HIGHERRANK") msgs <- c(msgs, sprintf("GBIF matched '%s' only at a higher rank (%s).", pest_name, gb$rank))
    else if (identical(gb$status, "SYNONYM")) msgs <- c(msgs, sprintf("'%s' is a GBIF synonym of %s.", pest_name, gb$gbif_name))
  }
  ep <- eppo_resolve(pest_name, gb, api_key)
  if (is.na(ep$code)) {
    msgs <- c(msgs, "No EPPO code found for this name (or its GBIF accepted name).")
    return(list(gbif = gb, eppo = ep, hosts = NULL, messages = msgs))
  }
  ov <- tryCatch(eppo_overview(ep$code, api_key), error = function(e) NULL)
  if (is.null(ov) || is.na(ov$prefname)) {
    msgs <- c(msgs, sprintf("EPPO code %s was not found.", ep$code))
    return(list(gbif = gb, eppo = ep, hosts = NULL, messages = msgs))
  }
  ep$prefname <- ov$prefname; ep$author <- ov$author; ep$norwegian <- ov$norwegian; ep$english <- ov$english
  if (is.null(gb) || gb$matchType %in% c("NONE", "HIGHERRANK"))   # e.g. an EPPO code was entered
    gb <- tryCatch(gbif_match(ov$prefname), error = function(e) NULL)
  h <- eppo_hosts(ep$code, api_key)
  if (!nrow(h)) msgs <- c(msgs, sprintf("EPPO has no host records for %s (%s).", ov$prefname, ep$code))
  list(gbif = gb, eppo = ep, hosts = h, messages = msgs)
}

#' For each host: GBIF match, Norway summary, classification, reference status.
#' progress: optional function(i, n, name) for UI progress.
summarise_hosts <- function(hosts, basis_keep = GBIF_BASIS_COUNTED, t1 = HM_T1, t2 = HM_T2,
                            ref = NULL, progress = NULL, links = TRUE) {
  if (is.null(hosts) || !nrow(hosts)) return(NULL)
  n <- nrow(hosts)
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    nm <- hosts$prefname[i]
    if (!is.null(progress)) progress(i, n, nm)
    m <- tryCatch(gbif_match(nm, kingdom = "Plantae"),
                  error = function(e) .empty_match(nm, conditionMessage(e)))
    s <- if (!is.na(m$acceptedKey)) tryCatch(summary_row(gbif_no_summary(m$acceptedKey), basis_keep),
                                             error = function(e) { m$note <<- paste(m$note, "| GBIF count failed:", conditionMessage(e)); NULL }) else NULL
    if (is.null(s)) s <- summary_row(list(total = NA, basis = integer(0), years = integer(0), n_counties = NA), basis_keep)
    if (is.na(m$acceptedKey) || is.na(s$n_total_all)) s$n_records <- NA_integer_
    rows[[i]] <- cbind(m, s)
  }
  g <- bind_rows(rows)
  out <- data.frame(host_plant = hosts$prefname, host_eppocode = hosts$eppocode,
                    eppo_category = factor(hosts$class_label, levels = union(EPPO_CATEGORY_LEVELS, unique(hosts$class_label))),
                    g[, setdiff(names(g), "input_name")], stringsAsFactors = FALSE)
  out$uncertainty_class <- classify_count(out$n_records, t1, t2)
  out <- cbind(out, lookup_reference(ref, out$host_plant, out$gbif_canonical))
  out$gbif_link <- gbif_url(out$acceptedKey)
  if (links) {
    nm_link <- ifelse(is.na(out$gbif_canonical), out$host_plant, out$gbif_canonical)
    tid <- vapply(seq_len(nrow(out)), function(i)
      if (!is.na(out$n_records[i]) && out$n_records[i] > 0) nortaxa_id(nm_link[i]) else NA_integer_, 0L)
    out$artskart_link <- artskart_url(tid, nm_link)
  }
  out
}

# ---- Bulk prefetch (light-weight concurrency for long host lists) -----------
# Concurrent HTTP requests from the single R process (httr2::req_perform_parallel):
# no extra R sessions, so it suits shinyapps.io. Results go into the same cache
# the per-host functions read, so summarise_hosts() afterwards only reads.
GBIF_API <- "https://api.gbif.org/v1"

.gbif_req <- function(path, query) {
  request(paste0(GBIF_API, path)) |> req_url_query(!!!query, .multi = "explode") |>
    req_user_agent("host_map shiny app (VKM)") |> req_timeout(60) |>
    req_retry(max_tries = 5, is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503),
              backoff = function(i) 2^i) |>
    req_error(is_error = function(r) FALSE)
}

.ok_body <- function(r) if (inherits(r, "httr2_response") && resp_status(r) == 200) resp_body_string(r) else NULL

.perform_bulk <- function(reqs, max_active, progress = NULL, label = "") {
  if (!length(reqs)) return(list())
  chunks <- split(seq_along(reqs), ceiling(seq_along(reqs) / 40))
  out <- vector("list", length(reqs)); done <- 0
  for (ch in chunks) {
    out[ch] <- httr2::req_perform_parallel(reqs[ch], on_error = "continue", max_active = max_active, progress = FALSE)
    done <- done + length(ch)
    if (!is.null(progress)) progress(done, length(reqs), label)
  }
  out
}

prefetch_gbif <- function(names, kingdom = "Plantae", max_active = 4, country = "NO", progress = NULL) {
  names <- unique(trimws(names))
  todo <- names[vapply(names, function(n) is.null(.cache_get("match", n, kingdom)), TRUE)]
  if (length(todo)) {
    reqs <- lapply(todo, function(n) .gbif_req("/species/match", list(name = n, kingdom = kingdom, verbose = "true")))
    resp <- .perform_bulk(reqs, max_active, progress, "names")
    for (i in seq_along(todo)) {
      txt <- .ok_body(resp[[i]]); if (is.null(txt)) next   # failures fall back to the rgbif path later
      m <- tryCatch(.gbif_match_from_json(todo[i], kingdom, txt), error = function(e) NULL)
      if (!is.null(m)) .cache_set(m, "match", todo[i], kingdom)
    }
  }
  keys <- unique(na.omit(vapply(names, function(n) .cache_get("match", n, kingdom)$acceptedKey %||% NA_character_, "")))
  keys <- keys[vapply(keys, function(k) is.null(.cache_get("nosum", k, country)), TRUE)]
  if (length(keys)) {
    reqs <- lapply(keys, function(k) .gbif_req("/occurrence/search", list(taxonKey = k, country = country, limit = 0,
                      facet = c("basisOfRecord", "year", "gadmLevel1Gid"), facetLimit = 500)))
    resp <- .perform_bulk(reqs, max_active, progress, "counts")
    for (i in seq_along(keys)) {
      txt <- .ok_body(resp[[i]]); if (is.null(txt)) next
      s <- tryCatch(.summary_from_json(txt), error = function(e) NULL)
      if (!is.null(s)) .cache_set(s, "nosum", keys[i], country)
    }
  }
  invisible(NULL)
}
