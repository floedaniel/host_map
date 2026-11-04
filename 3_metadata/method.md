### Pipeline

1. **Pest name → GBIF Backbone** (`rgbif::name_backbone`). Synonyms are resolved to the accepted taxon. If GBIF only matches at a higher rank (common for ambiguous synonyms such as *Picea excelsa*), the exact-match alternatives are inspected and the most frequent accepted taxon is chosen. The table's "Match note" column flags this.
2. **EPPO code**: the EPPO GD API v2 `tools/name2codes` is queried first with the name as entered, then with the GBIF accepted name. You can also type an EPPO code (e.g. `ERWIAM`) directly.
3. **Host plants**: EPPO GD API v2 `taxons/taxon/{code}/hosts`, with the EPPO host category (Major host, Host, Minor host, Experimental, Incidental, Artificial, Wild/Weed, Doubtful host, …).
4. **Each host → GBIF taxonKey → Norway summary**: a single `occ_search(country = "NO", limit = 0)` request with facets on `basisOfRecord`, `year` and `gadmLevel1Gid` (county). No records are downloaded. All calls are memoised to a disk cache.
5. **Classification of documented occurrence** (record count = records whose basisOfRecord is selected in Configuration):

| Class | Count rule | Revised rule (default) |
|---|---|---|
| Not documented in Norway | 0 | 0 |
| High uncertainty | 1–10 | 1–10 |
| Moderate uncertainty | 11–50 | 11–50, **or** > 50 but < 10 distinct years or < 2 counties |
| Low uncertainty | > 50 | > 50 **and** ≥ 10 distinct years **and** ≥ 2 counties |

See `5_outputs/REPORT.md` for the validation behind the revised rule.

### Two separate concepts

* **Documented occurrence** (the uncertainty class) is based on GBIF records only. A plant can be well documented because it is planted, cultivated or a casual escape.
* **Established in Norwegian nature** comes from Artsdatabanken: Norsk rødliste 2021 (native and pre-1800 established plants) and Fremmedartslista 2023 (alien plants, establishment classes A–C3). *Established* = reproducing, self-sustaining populations (native/pre-1800 established, or alien class C2/C3). *Present, not established* = alien class B1/B2/C0/C1, or a non-established Red List category. *Absent* = alien class A (door-knocker) or "no documented occurrence". An empty cell means the plant is not in either list.

### Map

Points are fetched per host (at most "max points per layer" per basisOfRecord). Records with coordinate uncertainty above the threshold are dropped, and duplicates are removed on the unique combination of latitude, longitude, year and recordedBy. A message is shown when more records exist than were fetched.
