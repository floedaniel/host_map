### Pipeline

1. **Pest → EPPO.** The name is resolved against the GBIF Backbone (`rgbif::name_backbone`) to handle synonyms. The EPPO code is then found with the EPPO GD API v2 `tools/name2codes`, using the name as entered and then the GBIF accepted name. An EPPO code (e.g. `ERWIAM`) can also be typed directly. The pest name shown is the **EPPO preferred name**.
2. **Host plants.** EPPO GD API v2 `taxons/taxon/{code}/hosts`, with the EPPO host category (Major host, Host, Minor host, Experimental, Incidental, Artificial, Wild/Weed, Doubtful host, …). Host names are EPPO's.
3. **Each host → GBIF taxon → Norway summary.** Synonyms are resolved to the accepted species. Ambiguous synonyms (e.g. *Picea excelsa*) are resolved via exact-match alternatives, and a match only at a higher rank is never counted. One `occurrence/search` request per host (`country = NO`, `limit = 0`) returns counts by basisOfRecord, year and county, so no records are downloaded. Requests run concurrently from a single R process and are cached.

### Uncertainty class (documented occurrence)

Records counted: every basisOfRecord except living specimens (botanic gardens) and fossils.

| Class | Records in Norway | Share of such hosts actually absent (validation) |
|---|---|---|
| Not documented in Norway | 0 | — |
| High uncertainty | 1–12 | about 30% |
| Moderate uncertainty | 13–37 | about 5% |
| Low uncertainty | ≥ 38 | < 2% |

The boundaries are where the probability that the host really occurs in Norway reaches 0.95 and 0.99. They were estimated from 273 hosts of 20 pests with known Norwegian status (logistic calibration on log record count). Distinct years, counties, record type and recency added no information, so they are not used. The settings are fixed; see `5_outputs/REPORT.md`.

### Two separate concepts

* **Documented occurrence** (the uncertainty class) is based on GBIF records only. A plant can be very well documented because it is a crop or a casual escape: oats, barley and potato have hundreds of Norwegian records but are not established.
* **Status in Norwegian nature** comes from Artsdatabanken: Norsk rødliste 2021 and Fremmedartslista 2023.
  * *Established*: native or pre-1800 established, or alien class C2/C3.
  * *Present, not established*: alien class B1/B2/C0/C1, or a non-established Red List category.
  * *Absent*: alien class A (door-knocker), or "no documented occurrence".
  * An empty cell means the plant is in neither list. Almost all such plants are not established in Norway.

### Map

Up to 1,000 points are fetched per host. Records with coordinate uncertainty above 1 km are dropped, and duplicates are removed on the unique combination of latitude, longitude, year and recordedBy. A message is shown when more records exist than were fetched. By default the 5 Major hosts with the most records are mapped; select rows in the table to map other hosts.
