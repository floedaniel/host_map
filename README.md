# Host Map

R Shiny app (part of the **FinnPRIO** project, VKM) for exploring the **host plants of a plant pest** (EPPO Global Database) and how well each host is **documented in Norway** (GBIF). This is kept separate from whether the host is **established in Norwegian nature** (Artsdatabanken: Red List 2021 and Alien Species List 2023).

Contact: Daniel Flø <Daniel.Flo@vkm.no>

## Quick start

```r
# 1. EPPO API key (free: https://data.eppo.int/), in ~/.Renviron:
#    EPPO_API_KEY=your-key
# 2. Packages
source("4_scripts/install_packages.R")      # or: Rscript 4_scripts/install_packages.R --dev (adds test packages)
# 3. Run from the project folder
shiny::runApp()
```

Type a pest name (synonyms are fine, e.g. *Agrilus marcopoli*) or an EPPO code (`ERWIAM`) and press **Search**. A walk-through is in [`3_metadata/vignette.md`](3_metadata/vignette.md).

## Deploying on Posit Shiny Server

Tested on Linux (Ubuntu 24.04, R 4.6) the way Shiny Server runs apps: R started in the app folder, the app folder read-only, the API key read from the app's `.Renviron`, and two browser sessions on the same R process. All browser and unit tests passed.

1. **R packages.** Install them as a user who can write to the site library that the `shiny` user reads. On Ubuntu/Debian this uses Posit Package Manager binaries, so it takes about a minute.

   ```sh
   sudo Rscript 4_scripts/install_packages.R
   ```

2. **Copy the app** to the Shiny Server app folder, e.g. `/srv/shiny-server/host_map/`. Required: `app.R`, `4_scripts/hostmap_functions.R`, `4_scripts/theme.R`, `3_metadata/method.md`, `2_processed_data/artsdatabanken_reference.rds` and `www/`. The rest of the repository (validation, tests) is optional.

3. **Settings in `/srv/shiny-server/host_map/.Renviron`.** R reads this file because Shiny Server starts R in the app folder. Make it readable by `shiny` only.

   ```
   EPPO_API_KEY=your-eppo-key
   HOSTMAP_CACHE_DIR=/var/lib/shiny-server/host_map_cache
   ```

   ```sh
   sudo mkdir -p /var/lib/shiny-server/host_map_cache
   sudo chown shiny:shiny /var/lib/shiny-server/host_map_cache /srv/shiny-server/host_map/.Renviron
   sudo chmod 600 /srv/shiny-server/host_map/.Renviron
   ```

   The cache folder must be writable by `shiny`. If it isn't, the app still works, but falls back to a temporary folder that is emptied whenever the R process stops (the log says so). With a persistent cache folder, repeated searches are instant, also after restarts.

4. **Network.** The server needs outbound HTTPS to `api.gbif.org`, `api.eppo.int` and `nortaxa.artsdatabanken.no`. The map tiles (`server.arcgisonline.com`, `tile.openstreetmap.org`) are loaded by the user's browser, not the server.

5. **Resources.** The app runs in one R process shared by all users (about 300 MB). Searches run in up to `SEARCH_WORKERS = 2` background R processes (about 150–250 MB each), so a long first search does not freeze the app for others. Further simultaneous searches queue. The workers end when Shiny Server stops the idle app. On a small server, set `SEARCH_WORKERS <- 1` at the top of `app.R`. To use no extra processes at all, set `BACKGROUND_SEARCH <- FALSE`; searches then block other users while they run.

6. **Check it.** Open the app, search *Erwinia amylovora*, and look for errors in the app log (`/var/log/shiny-server/`). To run the browser tests against the server from any machine with Chrome:

   ```r
   Sys.setenv(NOT_CRAN = "true", HOSTMAP_APP_URL = "https://<server>/host_map/")
   testthat::test_file("4_scripts/tests/test_app.R")
   ```

   (Most tests need the app started with `shiny.testmode = TRUE`, i.e. `options(shiny.testmode = TRUE)` in `app.R` while testing.)

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
| `4_scripts/` | `hostmap_functions.R` (shared logic), `theme.R` (VKM theme), `install_packages.R`, `00_`–`02_` validation scripts, `tests/` |
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

Set `HOSTMAP_OFFLINE=true` to run only the offline unit tests. The browser tests include a check that one user's long search does not block another user of the same app process.
