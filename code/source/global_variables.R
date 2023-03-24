library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2) 
library(stringr)
library(ggplot2) 
library(data.table)


PATH_OUTFILES <- "../shared/data_projects/med_datasets/2000_2019_data" #rename to PATH_ convention
STUDY_PERIOD_START <- as.Date("2000-01-01")
STUDY_PERIOD_END <- as.Date("2019-12-31")
STUDY_AREA <- extent(-10.25, 40.25, 29.75, 45.25) 
