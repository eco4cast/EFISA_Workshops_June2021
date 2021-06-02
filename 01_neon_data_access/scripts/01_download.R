
library(tidyverse)
library(duckdb)
library(Amelia)

source("./01_neon_data_access/scripts/download_functions/DownloadPhenocam.R")
source("./01_neon_data_access/scripts/download_functions/QuantifyUncertainty.R")
source("./01_neon_data_access/scripts/download_functions/DownloadNEON.R")

##Selected Sites for Challenge
siteIDs <- "NEON.D01.HARV.DP1.00033" # Harvard phenocam data
site <- "HARV"                       # Site names for download
product <- "DP1.00002.001"           # Air temperature NEON product


message(paste0("Downloading and generating phenology targets ", Sys.time()))

target <- data.frame(matrix(nrow = 0, ncol = 5))

for(i in 1:length(siteIDs)){
  siteName <- siteIDs[i]
  message(siteName)
  if(siteName != "NEON.D11.CLBJ.DP1.00033"){
    URL_gcc90 <- paste('https://phenocam.sr.unh.edu/data/archive/',
                       siteName,"/ROI/",siteName,"_DB_1000_1day.csv",sep="") ##URL for daily summary statistics
    URL_individual <- paste('https://phenocam.sr.unh.edu/data/archive/',
                            siteName,"/ROI/",siteName,"_DB_1000_roistats.csv",sep="") ##URL for individual image metrics
  }else{
    URL_gcc90 <- paste('https://phenocam.sr.unh.edu/data/archive/',
                       siteName,"/ROI/",siteName,"_DB_2000_1day.csv",sep="") ##URL for daily summary statistics
    URL_individual <- paste('https://phenocam.sr.unh.edu/data/archive/',
                            siteName,"/ROI/",siteName,"_DB_2000_roistats.csv",sep="") ##URL for individual image metrics
  }
  phenoData <- download.phenocam(URL = URL_gcc90)
  dates <- unique(phenoData$date)
  phenoData_individual <- download.phenocam(URL=URL_individual,skipNum = 17)
  gcc_sd <- calculate.phenocam.uncertainty(dat=phenoData_individual,dates=dates) ##Calculates standard deviations on daily gcc90 values
  
  subPhenoData <- phenoData %>% 
    mutate(siteID = stringr::str_sub(siteName, 10, 13), 
           time = date) %>% 
    select(time, siteID, gcc_90)
  subPhenoData <- cbind(subPhenoData,gcc_sd)
  
  target <- rbind(target,subPhenoData)
  
}

full_time <- seq(min(allData$time),max(allData$time), by = "1 day")
full_time <- tibble(time = rep(full_time, 1))
target <- left_join(full_time, target, by = c("time"))
readr::write_csv(target, "./01_neon_data_access/data/phenology-targets.csv.gz")

message(paste0("Completed downloading and generating phenology targets ", Sys.time()))




message(paste0("Downloading and generating driver data for phenology targets ", Sys.time()))

subDir <- "neonstore"
if (file.exists(file.path(getwd(),"01_neon_data_access","data",subDir))){
  Sys.setenv("NEONSTORE_DB" = "./01_neon_data_access/data/neonstore/")
  Sys.setenv("NEONSTORE_HOME" = "./01_neon_data_access/data/neonstore/")
  neonstore::neon_dir()
} else {
  dir.create(file.path(getwd(),"01_neon_data_access","data",subDir))
  Sys.setenv("NEONSTORE_HOME" = "./01_neon_data_access/data/neonstore/")
  Sys.setenv("NEONSTORE_DB" = "./01_neon_data_access/data/neonstore/")
  neonstore::neon_dir()
}

download_neon(site = site, product = product)

message(paste0("Completed downloading and generating driver data for phenology targets ", Sys.time()))

