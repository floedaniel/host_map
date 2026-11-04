# Host Map

R Shiny app (part of the **FinnPRIO** project, VKM) for exploring the **host plants of a plant pest** (EPPO Global Database) and how well each host is **documented in Norway** (GBIF). This is kept separate from whether the host is **established in Norwegian nature** (Artsdatabanken: Red List 2021 and Alien Species List 2023).

Contact: Daniel Flø <Daniel.Flo@vkm.no>

## Quick start

```r
# 1. EPPO API key (free: https://data.eppo.int/), in ~/.Renviron:
#    EPPO_API_KEY=your-key
# 2. Packages
install.packages(c("shiny", "bslib", "DT", "leaflet", "rgbif", "httr2", "jsonlite",
                   "dplyr", "memoise", "cachem", "writexl", "rlang"))
# 3. Run from the project folder
shiny::runApp()
```

Type a pest name (synonyms are fine, e.g. *Agrilus marcopoli*) or an EPPO code (`ERWIAM`) and press **Search**. A walk-through is in [`3_metadata/vignette.md`](3_metadata/vignette.md).

## Deploying to shinyapps.io

The app runs in a single R process: no background workers, concurrent HTTP from one session, and at most 1,000 map points per host. It fits the free tier.

```r
# put EPPO_API_KEY=... in a file called .Renviron in the project folder (git-ignored)
rsconnect::deployApp(appFiles = c("app.R", ".Renviron", "4_scripts/hostmap_functions.R", "4_scripts/theme.R",
                                  "3_metadata/method.md", "2_processed_data/artsdatabanken_reference.rds",
                                  "www/vkm.css", "www/vkm_logo.svg"))
```

## What it does

| Step | Source |
|---|---|
| Resolve the pest name and synonyms | GBIF Backbone (`rgbif::name_backbone`) |
| Pest → EPPO code → EPPO preferred name → host list with categories | EPPO GD API v2 |
| Host → GBIF taxon → Norwegian record count, years, counties, basisOfRecord | GBIF `occurrence/search` (`limit = 0`, facets) |
| Uncertainty class for documented occurrence: 0 / 1–15 / 16–40 / ≥ 41 records | Calibrated in `5_outputs/REPORT.md` |
| Status in Norwegian nature (a separate concept) | Artsdatabanken (shipped table) |
| Map: key-free tiles (Esri light grey default) | GBIF points, capped, precision-filtered and de-duplicated |
| Export | CSV and Excel (with a settings sheet) |

The classification thresholds are fixed, statistically chosen constants, not user settings. Maintainer configuration (API key, map limits, cache) is at the top of `app.R`.

## Project layout

| Folder | Content |
|---|---|
| `app.R` | the Shiny app |
| `1_raw_data/` | raw Artsdatabanken download |
| `2_processed_data/` | reference status table, validation data, API cache (git-ignored) |
| `3_metadata/` | method text (shown in the app) and vignette |
| `4_scripts/` | `hostmap_functions.R` (shared logic), `theme.R` (VKM theme), `00_`–`02_` validation scripts, `tests/` |
| `5_outputs/` | `REPORT.md` (feasibility and statistical reassessment), figures, tables |
| `www/` | `vkm.css`, VKM logo |

## Reproducing the validation

```sh
Rscript 4_scripts/00_fetch_reference.R      # Artsdatabanken lists -> 1_raw_data/
Rscript 4_scripts/01_validation_collect.R   # 20 pests -> hosts -> GBIF features (about 10 min uncached)
Rscript 4_scripts/02_validation_analysis.R  # cross-tabs, CV, calibration -> 5_outputs/
```

## Tests

```r
Sys.setenv(NOT_CRAN = "true")       # enables the shinytest2 browser tests
testthat::test_dir("4_scripts/tests")
```

Set `HOSTMAP_OFFLINE=true` to run only the offline unit tests.
