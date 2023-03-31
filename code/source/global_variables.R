library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2) 
library(stringr)
library(ggplot2) 
library(data.table)
library(RColorBrewer)
library(sf)
library(foreach)

PATH_OUTFILES <- "~/shared/data_projects/med_datasets/2000_2019_data" #rename to PATH_ convention
STUDY_PERIOD_START <- as.Date("2000-01-01")
STUDY_PERIOD_END <- as.Date("2019-12-31")
STUDY_AREA <- extent(-10.25, 40.25, 29.75, 45.25) 



KG_CLASS_LEVEL3 <- data.frame(  
   c(1:30),
   c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb",
   "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd",
   "ET", "EF"),
   c(
    "Tropical, rainforest",
    "Tropical, monsoon",
    "Tropical, savannah",
    "Arid, desert, hot",
    "Arid, desert, cold",
    "Arid, steppe, hot",
    "Arid, steppe, cold",
    "Temperate, dry summer, hot summer",
    "Temperate, dry summer, warm summer",
    "Temperate, dry summer, cold summer",
    "Temperate, dry winter, hot summer",
    "Temperate, dry winter, warm summer",
    "Temperate, dry winter, cold summer",
    "Temperate, no dry season, hot summer",
    "Temperate, no dry season, warm summer",
    "Temperate, no dry season, cold summer",
    "Cold, dry summer, hot summer",
    "Cold, dry summer, warm summer",
    "Cold, dry summer, cold summer",
    "Cold, dry summer, very cold winter",
    "Cold, dry winter, hot summer",
    "Cold, dry winter, warm summer",
    "Cold, dry winter, cold summer ",
    "Cold, dry winter, very cold winter",
    "Cold, no dry season, hot summer",
    "Cold, no dry season, warm summer",
    "Cold, no dry season, cold summer",
    "Cold, no dry season, very cold winter",
    "Polar, tundra",
    "Polar, frost"
  ) 
)
names(KG_CLASS_LEVEL3) <- c("code", "abb", "description")


