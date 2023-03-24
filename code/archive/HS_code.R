rm(list = ls(all = TRUE))
Sys.setenv(TZ="UTC") # 
graphics.off()

library(zoo)
library(ncdf4)
library(xts)
#library(chron)
#library(easyNCDF)
maindir = getwd()
setwd(maindir)

## read discharge:

nc_data <- nc_open("merge_temp.nc")
tavg  <- ncvar_get(nc_data, "tavg")
tmin  <- ncvar_get(nc_data, "tmin")
tmax  <- ncvar_get(nc_data, "tmax")
lat <- ncvar_get(nc_data, "lat")
lon <- ncvar_get(nc_data, "lon")
time_int=ncvar_get(nc_data, "time")
#nc_close(nc_data)

## create date time series, obtain the origin from the units in time variable
dateNC = as.Date(time_int/24, origin="1900-01-01", tz="UTC")

#tavg=zoo(tavg,dateNC)

#####################
## Hargreve Samani PET calculation:
#####################

library(lubridate)

pet=tavg*NA
df= tavg*NA
latitude = lat*3.141592653589/180.0 
lambda_rho = 0.408
#length(dateNC)
start <- Sys.time()
for (i in 1:365){ 
   ##;-- using extraterr_rad (Zink, Kumar et al):     
  ## ;--inverse relative distance Earth-Sun - correction for eccentricity of Earths orbit around the sun
  dr     = 1.0 + 0.0330 * cos( 6.28318530717 * yday(dateNC[i]) / 365.25 )
  ##;-- declination of the sun above the celestial equator in radians
  delta  = 0.409 * sin( 6.28318530717 * yday(dateNC[i]) / 365.25 - 1.39)
  ##;-- arccos(x) is only defined between PI and 0 (for x between -1 and 1)
  ##;-- check limits:
  arg = - tan(latitude) * tan(delta)
  arg = ifelse( arg < -1.0, -1.0, arg ) 
  arg = ifelse( arg > 1.0, 1.0, arg ) 
  
  ##;-- sunrise hour angle in radians
  omega  = acos( arg )
  ##;-- Ra - converted from [MJ m-2 d-1]
  for (j in 1:length(lon)){
    extraterr_rad = (24.0 * 60.0) / 3.141592653589 * 0.0820  *   dr * (omega * sin(latitude) * sin(delta) + cos(latitude) * cos(delta) * sin(omega))
    df[j,,i] <-t(as.data.frame.array(extraterr_rad))
    
    pet[j,,i]=ifelse((tmax[j,,i]-tmin[j,,i])>=0,0.0023*df[j,,i]*sqrt(tmax[j,,i]-tmin[j,,i])*(tavg[j,,i]+17.8)/(2.5001-0.002361*tavg[j,,i]),0)
    
    #print(pet[j,,i])
  }
}
## Creating new NC file 
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- nc_data$dim[["time"]]
#timedim <- ncdim_def("time","RefTime =  1900-01-01 00:00:00  Units = hours  Calendar = gregorian",time_int)
dlname <- "PET hargreve samani method"

pet_def <- ncvar_def("pet","mm/day",list(londim,latdim,timedim),-9999,dlname,prec = "float")
#tmp_def <- ncvar_def("pet","mm/day",list(londim,latdim,timedim),pet)
ncout <- nc_create("pet_hs_1.nc",pet_def,force_v4=TRUE)
ncatt_put(ncout,"lon","axis","X")
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")
ncvar_put(ncout,pet_def,pet)
#pet_HS.nc <- nc_create("pet_HS.nc", list(tmp_def))


# Adding variable to exisiting files

#ncdata <- nc_open("CopyOfmerge_temp.nc")
#londim <- ncdata$dim[["lon"]]
#latdim <- ncdata$dim[["lat"]]
#timedim <- ncdata$dim[["time"]]
#dlname <- "PET hargreve samani method"
#tmp_def <- ncvar_def("pet","mm/day",list(londim,latdim,timedim),-9999,dlname,prec = "float")
#tmp_def <- ncvar_def("pet","mm/day",list(londim,latdim,timedim),pet)
#ncdata <-ncvar_add(ncdata,pet)
nc_close(ncout)
print(Sys.time()-start)