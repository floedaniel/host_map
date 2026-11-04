# Build a reference "status in Norwegian nature" table for vascular plants from
# Artsdatabanken open OData API: Red List 2021 (native) + Alien Species List 2023.
library(httr2); library(jsonlite); library(dplyr)
odata_all <- function(entity, select, filter) {
  url <- request(paste0("https://lister.artsdatabanken.no/odata/v1/", entity)) |>
    req_url_query(`$select` = select, `$filter` = filter) |> req_timeout(120)
  out <- list(); i <- 0
  repeat {
    r <- req_perform(url) |> resp_body_string() |> fromJSON(flatten = TRUE)
    i <- i + 1; out[[i]] <- r$value
    nl <- r$`@odata.nextLink`
    if (is.null(nl)) break
    url <- request(nl) |> req_timeout(120)
  }
  bind_rows(out)
}
rl <- odata_all("SpeciesAssessment2021",
  "scientificName,category,assessmentInitialSubClassificationCode,speciesGroup,assessmentArea,taxonRank",
  "speciesGroup eq 'Karplanter' and assessmentArea eq 'N'")
cat("Red list plant rows:", nrow(rl), "\n"); print(table(rl$assessmentInitialSubClassificationCode)); print(table(rl$category))
fab <- odata_all("AlienSpeciesAssessment2023",
  "scientificName,speciesStatus,alienSpeciesCategory,category,expertGroup,evaluationContext",
  "expertGroup eq 'Karplanter' and evaluationContext eq 'N'")
cat("Alien plant rows:", nrow(fab), "\n"); print(table(fab$speciesStatus)); print(table(fab$alienSpeciesCategory))
names(fab)
saveRDS(list(redlist = rl, alien = fab), "1_raw_data/artsdatabanken_raw.rds")  # run from project root
