# Host Map

R Shiny app for exploring the **host plants of a plant pest** (from the EPPO Global Database) and how well each host is **documented in Norway** (GBIF occurrence records), kept separate from whether the host is **established in Norwegian nature** (Artsdatabanken: Red List 2021 and Alien Species List 2023).

## Quick start

```r
# 1. EPPO API key (free: https://data.eppo.int/), e.g. in ~/.Renviron:
#    EPPO_API_KEY=your-key
# 2. Packages
install.packages(c("shiny", "bslib", "DT", "leaflet", "rgbif", "httr2", "jsonlite",
                   "dplyr", "memoise", "cachem", "writexl", "future", "future.apply"))
# 3. Run from the project folder
shiny::runApp()
```

Type a pest name (synonyms are fine, e.g. *Agrilus marcopoli*) or an EPPO code (`ERWIAM`) and press **Search**.

## What it does

| Step | Source |
|---|---|
| Resolve the pest name and synonyms | GBIF Backbone (`rgbif::name_backbone`) |
| Pest → EPPO code → host list with categories | EPPO GD API v2 (`tools/name2codes`, `taxons/taxon/{code}/hosts`) |
| Host → GBIF taxonKey → Norwegian record count, distinct years, counties, basisOfRecord | `rgbif::occ_search(country = "NO", limit = 0, facet = ...)` |
| Uncertainty class for documented occurrence | count rule, or revised rule (count + years + counties); see `5_outputs/REPORT.md` |
| Status in Norwegian nature (a separate concept) | Artsdatabanken OData API |
| Map | GBIF points per host (capped, precision-filtered, de-duplicated) on key-free tiles |
| Export | CSV and Excel (with a settings sheet) |

All settings (API key, thresholds, basisOfRecord filter, coordinate precision, max points, parallel workers, cache) are at the top of `app.R`. All API calls are memoised to a disk cache in `2_processed_data/cache/`.

## Project layout

| Folder | Content |
|---|---|
| `app.R` | the Shiny app (configuration at the top) |
| `1_raw_data/` | raw Artsdatabanken download |
| `2_processed_data/` | reference status table, API cache (git-ignored) |
| `3_metadata/` | method description (shown in the app's *Method* tab) |
| `4_scripts/` | `hostmap_functions.R` (shared logic), `theme.R` (VKM theme), validation scripts, `tests/` |
| `5_outputs/` | validation results and `REPORT.md` (feasibility + statistical reassessment) |
| `www/` | `vkm.css`, logo |

## Tests

```r
Sys.setenv(NOT_CRAN = "true")       # enables the shinytest2 browser tests
testthat::test_dir("4_scripts/tests")
```

Set `HOSTMAP_OFFLINE=true` to run only the offline unit tests.

Contact: Daniel Flø (VKM)
