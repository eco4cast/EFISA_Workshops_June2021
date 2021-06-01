met_qaqc <- function(realtime_file,
                     qaqc_file,
                     cleaned_met_file_dir){
  
  if(is.na(qaqc_file)){
    d1 <- readr::read_csv(realtime_file)
    d1$time <- lubridate::force_tz(d1$time, tzone = "UTC") #input_file_tz
    
    d2 <- readr::read_csv(qaqc_file)
    d2$time <- lubridate::force_tz(d2$time, tzone = "UTC") #input_file_tz
    
    d1 <- data.frame(time = d1$time, AirTemp = d1$air_temperature)
    d2 <- data.frame(time = d2$time, AirTemp = d2$air_temperature)
    
    d1 <- d1[which(d1$time > d2$time[nrow(d2)] | d1$time < d2$time[1]), ]
    
    d <- rbind(d2, d1)
    
  }else{
    
    d <- readr::read_csv(realtime_file)
  }
  
  d <- as.data.frame(d)
  
  
  # Fill in missing data (Used Amelia II instead of NLDAS)
  # This way all data are solely derived from the NEON data platforms
  
  #AirTemp
  amelia.at <- amelia(d, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
  at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
    select(time, AirTemp)%>%
    group_by(time)%>%
    summarise_all(funs(mean))
  
  plot(at_imputations$time, at_imputations$AirTemp)
  
  driver <- at_imputations %>%
    mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
    rename(surface_downwelling_shortwave_flux_in_air = ShortWave.x, 
           surface_downwelling_longwave_flux_in_air = LongWave.x, 
           air_temperature = AirTemp.x, 
           specific_humidity = RelHum.x, 
           wind_speed = WindSpeed.x, 
           precipitation_flux = Rain.x, 
           air_pressure = Pressure.x)
  
  
  d$time <- lubridate::force_tz(d$time, tzone = "UTC") #input_file_tz
  
}