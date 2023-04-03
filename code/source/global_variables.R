# generic
library(data.table)
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)

# plotting
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(cowplot)
library(viridis)

# geospatial
library(raster)
library(rgdal) 
library(ncdf4)
library(sp)
library(sf)
library(stars)

# parallel
library(doParallel)
library(foreach)


PATH_OUTFILES <- "~/shared/data_projects/med_datasets/2000_2019_data" #rename to PATH_ convention
STUDY_PERIOD_START <- as.Date("2000-01-01")
STUDY_PERIOD_END <- as.Date("2019-12-31")
STUDY_AREA <- extent(-10.25, 40.25, 29.75, 45.25) 


PATH_OBS_PRECIP_MONTHLY <- "~/shared/data_projects/med_datasets/2000_2019_data/obs/precip/"
PATH_SIM_PRECIP_MONTHLY <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/precip/"
PATH_SIM_EVAP_MONTHLY <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/"





