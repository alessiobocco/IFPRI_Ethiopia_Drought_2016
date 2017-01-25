
# Michael Mann
# This script uses RCurl and ModisDownload to access an ftp server and download desired modis tiles

# Run the following in bash before starting R
if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron

module load proj.4/4.8.0
module load gdal/gcc/1.11
module load R
module load gcc/4.9.0
R


rm(list=ls())
#source('R:\\Mann Research\\IFPRI_Ethiopia_Drought_2016\\IFPRI_Ethiopia_Drought_Code\\ModisDownload.R')
source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')



library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(maptools)
library(compiler)
library(ggplot2)
library(foreign)
library(plyr)
library(rgeos)
#cl <- makeCluster(32)
#registerDoParallel(cl)


# Compile Functions ---------------------------------------------------------------


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz$



# Set up parameters -------------------------------------------------------


  # Product Filters
  products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
  dates = c('2010-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02')


# Check if subsample of EAs matches EA shapefile ----------------------------------------------------

# read in subset of agss eas with crop cut data
agss = read.dta('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/AgSS/AgSS_2011_2016_EA.dta')
agss = agss[!is.na(agss$x_coord),]
coordinates(agss) =~ x_coord+y_coord
proj4string(agss) = "+init=epsg:32637"
agss = spTransform(agss,CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))

# read all eas
eas = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN',
        stringsAsFactors = F)
eas_backup = eas
eas$myid = 1:length(eas)


# Find EAS over subset
which_over = over(eas,agss, returnList=T) # returns values of eas that are over agss, empty if missing
not_empty = unlist(lapply(which_over,function(x) dim(x)[1]>0)) # if over agss dim >0
eas_sub = eas[not_empty,]

writeOGR(eas_sub, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/", 
	layer="EnumerationAreasSIN_sub_agss", driver="ESRI Shapefile") 




