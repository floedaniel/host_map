# =============================================================================
# 01_validation_collect.R
# Collect a validation set for the uncertainty classification:
#   20 pests relevant to Norway -> EPPO host lists -> unique host species ->
#   GBIF Norway features -> reference status (Artsdatabanken).
# Run from the project root:  Rscript 4_scripts/01_validation_collect.R
# Output: 2_processed_data/validation_hosts.rds, 5_outputs/validation_hosts.csv
# =============================================================================
source("4_scripts/hostmap_functions.R")
CACHE_DIR <- file.path(getwd(), "2_processed_data", "cache")
hm_init_cache(CACHE_DIR)
key <- Sys.getenv("EPPO_API_KEY")
stopifnot(nzchar(key))

PESTS <- c(
  "Erwinia amylovora", "Popillia japonica", "Phytophthora ramorum", "Agrilus planipennis",
  "Xylella fastidiosa", "Anoplophora glabripennis", "Anoplophora chinensis",
  "Bursaphelenchus xylophilus", "Dendrolimus sibiricus", "Thaumetopoea processionea",
  "Lymantria dispar", "Leptinotarsa decemlineata", "Globodera rostochiensis",
  "Ralstonia solanacearum", "Synchytrium endobioticum", "Aromia bungii",
  "Phytophthora kernoviae", "Bactrocera dorsalis", "Spodoptera frugiperda",
  "Ceratocystis platani")

# ---- 1. pests -> hosts -------------------------------------------------------
hl <- lapply(PESTS, function(p) {
  r <- get_pest_hosts(p, key)
  message(sprintf("%-28s %-7s %4d hosts  %s", p, r$eppo$code, NROW(r$hosts), paste(r$messages, collapse = "; ")))
  if (is.null(r$hosts) || !nrow(r$hosts)) return(NULL)
  cbind(pest = p, pest_code = r$eppo$code, r$hosts[, c("eppocode", "prefname", "class_label")])
})
pest_hosts <- dplyr::bind_rows(hl)
saveRDS(pest_hosts, "2_processed_data/validation_pest_hosts.rds")

# ---- 2. unique hosts -> GBIF -------------------------------------------------
hosts <- unique(pest_hosts$prefname)
message(length(hosts), " unique host names; prefetching GBIF in parallel ...")
t0 <- Sys.time()
prefetch_gbif(hosts, CACHE_DIR, workers = 6,
              progress = function(d, n) message(sprintf("  %d/%d  %.1f min", d, n, as.numeric(difftime(Sys.time(), t0, units = "mins")))))

rows <- lapply(hosts, function(nm) {
  m <- tryCatch(gbif_match(nm, kingdom = "Plantae"), error = function(e) .empty_match(nm, conditionMessage(e)))
  s <- if (!is.na(m$acceptedKey)) tryCatch(gbif_no_summary(m$acceptedKey), error = function(e) NULL) else NULL
  if (is.null(s)) return(cbind(m, n_total_all = NA))
  b <- s$basis
  cbind(m, summary_row(s, GBIF_BASIS_ALL),
        n_default = sum(b[!names(b) %in% c("LIVING_SPECIMEN", "FOSSIL_SPECIMEN")]),
        n_occurrence = sum(b[names(b) == "OCCURRENCE"]),
        n_years_human = NA_integer_)
})
d <- dplyr::bind_rows(rows)

# ---- 3. reference status -----------------------------------------------------
ref <- readRDS("2_processed_data/artsdatabanken_reference.rds")
r1 <- lookup_reference(ref, d$input_name, d$gbif_canonical)
d <- cbind(d, r1)
# Re-match unlisted hosts through the Norwegian taxonomy (Artsnavnebasen/Nortaxa),
# which knows Norwegian synonyms (e.g. GBIF 'Aria edulis' vs Norwegian 'Sorbus aria').
.nortaxa_names_impl <- function(name) {
  resp <- tryCatch(httr2::request("https://nortaxa.artsdatabanken.no/api/v1/TaxonName/Search") |>
                     httr2::req_url_query(Search = name) |> httr2::req_timeout(20) |> httr2::req_perform(),
                   error = function(e) NULL)
  if (is.null(resp)) return(character(0))
  x <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
  unique(unlist(lapply(x, function(t) c(t$acceptedScientificName$name,
                                        vapply(t$scientificNameSynonyms, function(s) s$name, "")))))
}
nortaxa_names <- hm_memo("nortaxa_names", .nortaxa_names_impl)
unl <- which(is.na(d$ref_status) & !is.na(d$acceptedKey))
message("Re-matching ", length(unl), " unlisted hosts via Nortaxa ...")
for (i in unl) {
  nms <- strip_name(c(nortaxa_names(d$input_name[i]), if (!is.na(d$gbif_canonical[i])) nortaxa_names(d$gbif_canonical[i])))
  k <- match(nms, ref$name); k <- k[!is.na(k)]
  if (length(k)) {
    k <- k[which.max(as.integer(ref$ref_status[k]))]
    d$ref_status[i] <- ref$ref_status[k]; d$ref_source[i] <- paste(ref$source[k], "(via Nortaxa)"); d$ref_detail[i] <- ref$detail[k]
  }
}

# ---- 4. attach pest/category info and save ------------------------------------
cats <- pest_hosts |> dplyr::group_by(prefname) |>
  dplyr::summarise(n_pests = dplyr::n_distinct(pest), pests = paste(unique(pest), collapse = "; "),
                   best_category = class_label[which.min(match(class_label, EPPO_CATEGORY_LEVELS))], .groups = "drop")
d <- dplyr::left_join(d, cats, by = c("input_name" = "prefname"))
d$is_species <- d$rank %in% "SPECIES" & .is_binomial(d$input_name) | grepl(" x | ×", d$input_name) & d$rank %in% "SPECIES"
saveRDS(d, "2_processed_data/validation_hosts.rds")
dir.create("5_outputs", showWarnings = FALSE)
utils::write.csv(d, "5_outputs/validation_hosts.csv", row.names = FALSE, na = "")
message("Done: ", nrow(d), " hosts; species-rank resolved: ", sum(d$is_species & !is.na(d$n_total_all)),
        "; with reference status: ", sum(!is.na(d$ref_status)))
print(table(species = d$is_species, ref = d$ref_status, useNA = "ifany"))
