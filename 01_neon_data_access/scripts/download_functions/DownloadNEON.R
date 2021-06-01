# Function to extract and clean up the NEON data from NEON
download_neon <- function(site, product){
  
  # Download newest products
  neonstore::neon_download(product = product, site = site)
  neonstore::neon_store("SAAT_30min-basic")

  
  # Tidy up the met datagrowing_DD <- neonstore::neon_table(table = "SAAT_30min-basic", site = site) %>%
    select(endDateTime, tempSingleMean)%>%
    mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
    select(-endDateTime)%>%
    group_by(time) %>%
    summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
    arrange(time)
  
  
  readr::write_csv(growing_DD, "phenology-driver.csv.gz")
  
}