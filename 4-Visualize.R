
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
#library(rts)
library(gdalUtils)
library(foreach)
library(doParallel)
library(compiler)
library(ggplot2)

#cl <- makeCluster(32)
#registerDoParallel(cl)


# Compile Functions ---------------------------------------------------------------


functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Profil$




# Set up parameters -------------------------------------------------------


# give path to Modis Reproduction Tool
MRT = 'H:/Projects/MRT/bin'

# get list of all available modis products
#GetProducts()

# Product Filters 
products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
dates = c('2011-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02') 
ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, /51/ for landcover
# allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
#  out_dir = 'R:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\VegetationIndex'
#  setwd(out_dir)




# Visualize examples of smoothed data -------------------------------------



setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')
load('./NDVI_200p_LUClasses.RData')
library(data.table)
#NDVI = rbindlist(NDVI)
NDVI_dates = strptime(gsub("^.*X([0-9]{7}).*$", "\\1",names(NDVI),perl = T),format='%Y%j')  # Strip dates

NDVI = as.data.frame.matrix(NDVI)
NDVI$Class = as.factor(points$class) # add classification


# smooth evi sample to avoid problems with NAs
NDVI_smooth = NDVI
for(i in 1:dim(NDVI)[1]){
  NDVI_smooth[i,] = SplineAndOutlierRemovalNA(x = NDVI[i,], dates=NDVI_dates, pred_dates=NDVI_dates,spline_spar = 0.4)
}
NDVI_smooth$Class = as.factor(points$class) # add classification


# get data ready for plotting
plotdata_NDVI = as.data.frame.matrix(reshape(NDVI,dir = 'long', times = format(NDVI_dates,'%Y%j'),varying = list(1:(dim(NDVI)[2]-1))))
names(plotdata_NDVI)[3] = 'NDVI'
head(plotdata_NDVI)
plotdata_NDVI_smooth = reshape(NDVI_smooth,dir = 'long', times = format(NDVI_dates,'%Y%j'),varying = list(1:(dim(NDVI)[2]-1))  )
names(plotdata_NDVI_smooth)[3] = 'NDVI'
head(plotdata_NDVI_smooth)


plotdata = data.frame(NDVI= plotdata_NDVI$NDVI, ID = plotdata_NDVI$id,Class = plotdata_NDVI$Class,
                      dates =as.Date(strptime(plotdata_NDVI$time,'%Y%j')),type = 'unsmoothed')

plotdata = rbind(plotdata, data.frame(NDVI= plotdata_NDVI_smooth$NDVI, ID = plotdata_NDVI_smooth$id,Class = plotdata_NDVI_smooth$Class,
                                      dates =as.Date(strptime(plotdata_NDVI$time,'%Y%j')),type = 'smoothed'))
head(plotdata)

plotdata =  plotdata[plotdata$Class %in% c('wetag'),]   # c('wetag','dryag','agforest')


# Get planting and harvest dates
plantharvest =   PlantHarvestDates(dates[1],dates[2],PlantingMonth=4,PlantingDay=1,HarvestMonth=1,HarvestDay=30)

# plot out time series with planting and harvest dates
rects = data.frame(xstart = as.Date(plantharvest$planting),
                   xend = as.Date(plantharvest$harvest))

ggplot()+geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), alpha = 0.4)+
  geom_point(data= plotdata[plotdata$type=='smoothed',], aes(x=dates,y=NDVI,group=ID) )
facet_wrap(~Class)

