# Function to extract and clean up the NEON data from NEON
download_neon <- function(site, product){
  
  # Download newest products
  neonstore::neon_download(product = product, site = site)
  neonstore::neon_store("SAAT_30min-basic")

  
  # Tidy up the met 
  growing_DD <- neonstore::neon_table(table = "SAAT_30min-basic", site = site) %>%
    select(endDateTime, tempSingleMean)%>%
    mutate(time = lubridate::floor_date(endDateTime, unit = "day"))%>%
    select(-endDateTime)%>%
    group_by(time) %>%
    summarize(mean = mean(tempSingleMean, na.rm = T),
              min = min(tempSingleMean, na.rm = T),
              max = max(tempSingleMean, na.rm = T))%>%
    arrange(time)%>%
    mutate(GDD = ((max + min)/2) - 10)%>%
    mutate(GDD = ifelse(GDD > 0, GDD, 0))%>%
    select(time, GDD)%>%
    filter(time>="2014-10-20")
  
  
  readr::write_csv(growing_DD, "./01_neon_data_access/data/raw-phenology-driver.csv.gz")
  
  
  growing_DD <- as.data.frame(growing_DD)
  
  #AirTemp
  growing_DD.dd <- amelia(growing_DD, m = 200, polytime = 2, ts = "time", cs = NULL, lags = "GDD", leads = "GDD")
  DD_imputations <- bind_rows(unclass(growing_DD.dd$imputations), .id = "m") %>%
    select(time, GDD)%>%
    group_by(time)%>%
    summarise_all(funs(mean))
  
  plot(DD_imputations$time, DD_imputations$GDD, col = "red", pch = 19)
  points(growing_DD$time, growing_DD$GDD, col = "black", pch = 19)

  readr::write_csv(DD_imputations, "./01_neon_data_access/data/qaqc-phenology-driver.csv.gz")
  
}