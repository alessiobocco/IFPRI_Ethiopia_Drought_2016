# the file takes stack outputs from 2 - Stack Files.R and extracts data to EA polygons

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






# Extract polygon or points data from stacks -------------------------------------

library(data.table)
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean LC/') # don't load smoothed...
dir.create(file.path('../../Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # create dir for tifs
dir.create(file.path('/lustre/groups/manngroup/Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # folder on high speed


# load data stacks from both directories
rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

# get polygon data 
#ogrInfo('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
#Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
#Polys = spTransform(Polys, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
#writeOGR(Polys, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/", 
#     layer="EnumerationAreasSIN", driver="ESRI Shapefile") # this is in geographical projection
Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN',
                stringsAsFactors = F)
Polys$id = 1:dim(Polys@data)[1]

head(Polys)
unique(Polys$R_NAME)
Polys = Polys[!(Polys$R_NAME %in% c('SOMALI','Addis Ababa','SOMALIE')),]


# break into blocks of polygons
block_width = 250
nrows = dim(Polys)[1]
nblocks <- nrows%/%block_width
bs_rows <- seq(1,nblocks*block_width+1,block_width)
bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
print('Working on the following rows')
print(paste(bs_rows))

# use iterator package to move through rows 
inter_rows = lapply(1:length(bs_rows), function(x) seq(bs_rows[x],(bs_rows[x]+bs_nrows[x]-1)))  
inter_rows = iter(inter_rows)

product = c('NDVI','EVI')[1]

out = foreach(rows= seq(1,inter_rows$length)) %do% {
  # limit size of polys to avoid memory issues
  Polys_sub = Polys[nextElem(inter_rows),]
  # extract values croped to point or polygon
  Poly_Veg_Ext = extract_value_point_polygon(Polys_sub,
                                             list(get(paste(product,'_stack_h22v08_WO_Clouds_Clean_LC',sep='')),
                                                  get(paste(product,'_stack_h22v07_WO_Clouds_Clean_LC',sep='')),
                                                  get(paste(product,'_stack_h21v08_WO_Clouds_Clean_LC',sep='')),
                                                  get(paste(product,'_stack_h21v07_WO_Clouds_Clean_LC',sep=''))),10)
  print(paste("saving block",rows))
  save(Poly_Veg_Ext ,
       file = paste(
         '/lustre/groups/manngroup/Processed Panel/ExtractRaw/',
         rows,product,'_panel_','_ExtractRaw','.RData',sep='') )
  rm(Poly_Veg_Ext)
  return(0)
}

# Copy files back from lustre and delete lustre
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
flist = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
                   glob2rx(paste('*','.RData$',sep='')),full.names = T)
fname = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
                   glob2rx(paste('*','.RData$',sep='')),full.names = F)
file.copy(from=flist, to=paste(".",fname,sep='/'),
          overwrite = T, recursive = F, copy.mode = T)
file.remove(flist)
print(paste('Restacking',product,tile,sep=' '))




# Compile data from polygon extract  --------------------------------------


flist = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
                   glob2rx(paste('*','.RData$',sep='')),full.names = T)
fname = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
                   glob2rx(paste('*','.RData$',sep='')),full.names = F)

load(flist[1])




# Extract data from subset of eas that have agss data --------------------------
# this is a subset of data for which agss data contains relevant crop data 

setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean LC/')

# load data stacks from both directories
rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)


Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes',
                    stringsAsFactors = F)
Polys_sub$id = 1:dim(Polys_sub@data)[1]
product = c('NDVI','EVI')[1]

#Poly_Veg_Ext_sub = extract_value_point_polygon(Polys_sub,
#               list(get(paste(product,'_stack_h22v08_WO_Clouds_Clean_LC',sep='')),
#               get(paste(product,'_stack_h22v07_WO_Clouds_Clean_LC',sep='')),
#               get(paste(product,'_stack_h21v08_WO_Clouds_Clean_LC',sep='')),
#               get(paste(product,'_stack_h21v07_WO_Clouds_Clean_LC',sep=''))),15)

#save(Poly_Veg_Ext_sub,
#	file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
#	product,'_Poly_Ext_sub_agss.RData',sep=''))




# prepare other data ---------------------------------------

setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/DistanceTransport/')

# reproject transport variables
example = proj4string(raster('../LandUseClassifications/NDVI_stack_h21v07_smooth_lc_svm_mn.tif'))
#dist_rcap = raster('EucDist_Rcap.tif')
#dist_rcap = projectRaster(dist_rcap, crs= crs(example), filename = './EucDist_Rcap_sin.tif',overwrite=T)
#roadden = raster('RoadDen_5km_WLRC.tif')
#roadden = projectRaster(roadden,crs=crs( example), filename = './RoadDen_5km_WLRC_sin.tif',overwrite=T)
#dist_pp50k = raster('EucDist_pp50k.tif')
#dist_pp50k = projectRaster(dist_pp50k, crs=crs(example), filename = './EucDist_pp50k_sin.tif',overwrite=T)

# deal with PET
#flist = list.files("../PET/", glob2rx(paste('*','.tif$',sep='')),full.names = T)
#year = paste('20',gsub("^.*([0-9]{2})_([0-9]{2}).*$", "\\1",flist,perl = T),sep='')  # Strip dates
#month = gsub("^.*([0-9]{2})_([0-9]{2}).*$", "\\2",flist,perl = T)  # Strip dates
#flist_dates = format(strptime(paste(year,month,'01',sep='_'),'%Y_%m_%d'),'%Y%j')   
#flist = flist[order(as.numeric(paste(year,month,sep='')))]  # file list in order
#flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
#PET_stack = stack(flist)
#names(PET_stack)=flist_dates
#save(PET_stack,file = '../PET/PET_stack.RData')

# deal with ETa
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
#flist = list.files("./ETa Anomaly/", glob2rx(paste('*','.zip$',sep='')),full.names = T)
#foreach(i = 1:length(flist), .inorder=F) %dopar% {unzip(flist[i], exdir ='./ETa Anomaly/')}
#flist = list.files("./ETa Anomaly/", glob2rx(paste('*','ET.tif$',sep='')),full.names = T)
#flist_dates = paste(gsub("^.*ma([0-9]{4}).*$", "\\1",flist,perl = T),sep='')  # Strip dates
#flist = flist[order(flist_dates)]  # file list in order
#flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
#flist_dates = paste('20',substr(flist_dates,1,2),'-',substr(flist_dates,3,4),'-01',sep='')
#flist_dates = format(strptime(flist_dates, '%Y-%m-%d'),'%Y%j')

#example = raster(flist[1])
#example = projectRaster(example,crs= CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))
#for(layer in 1:length(flist)){ print(layer)	
#	print(raster(flist[layer]))
#	print(extent(raster(flist[layer]))==extent(example))
#}

#registerDoParallel(15)
#foreach(layer = 1:length(flist), .inorder=F, .errorhandling ='pass') %dopar% {
#	print(layer)
#	layer_out = raster(flist[layer])
#	if(extent(layer_out)!=extent(example)){
#		layer_out = projectRaster(layer_out, example)
#		writeRaster(layer_out,flist[layer],overwrite=T)
# 	}
#}

#ETA_stack = stack(flist)
#names(ETA_stack)=flist_dates
#save(ETA_stack,file = './ETa Anomaly/ETA_stack.RData')



# Summarize data to enumeration areas --------------------------------------------------

Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes',
                    stringsAsFactors = F)
Polys_sub$id = 1:dim(Polys_sub@data)[1]
product = 'NDVI'

load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
           product,'_Poly_Ext_sub_agss.RData',sep=''))

# Get planting and harvest dates
plantharvest =   PlantHarvestDates(start_date=dates[1],end_date=dates[2],PlantingMonth=4,
                                   PlantingDay=1,HarvestMonth=1,HarvestDay=30)

# Get summary statistics lists
extr_values=Poly_Veg_Ext_sub
PlantHarvestTable = plantharvest
Quant_percentile=0.90
num_workers = 10
spline_spar = 0
NDVI_summary =  Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, 
                                         return_df=T,num_workers)


# pull data to polygons
# load data
#setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
#dist_rcap = raster('./DistanceTransport/EucDist_Rcap_sin.tif')
#roadden = raster('./DistanceTransport/RoadDen_5km_WLRC_sin.tif')
#dist_pp50k = raster('./DistanceTransport/EucDist_pp50k_sin.tif')
#
# for(layer in c('dist_rcap','roadden','dist_pp50k')){
#       values = extract_value_point_polygon(Polys_sub,get(layer),16)
#       mean = do.call('rbind',lapply(values, function(x) if (!is.null(x)) colMeans(x, na.rm=TRUE) else NA ))
#       Polys_sub[[layer]] = as.numeric(mean)
#}

#writeOGR(obj=Polys_sub, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/",
#	 layer="EnumerationAreasSIN_sub_agss_codes_wdata", driver="ESRI Shapefile")


# pull ETA PET data to polygons 

setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
#load('./PET/PET_stack.RData')
#load('./ETa Anomaly/ETA_stack.RData')
#Poly_PET_Ext_sub = extract_value_point_polygon(Polys_sub,PET_stack,15)
#save(Poly_PET_Ext_sub,
#      file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
#      'Poly_PET_Ext_sub.RData',sep=''))
#Poly_ETA_Ext_sub = extract_value_point_polygon(Polys_sub,ETA_stack,15)
#save(Poly_ETA_Ext_sub,
#      file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
#      'Poly_ETA_Ext_sub.RData',sep=''))

load('./Processed Panel/ExtractRaw/Poly_PET_Ext_sub.RData')
load('./Processed Panel/ExtractRaw/Poly_ETA_Ext_sub.RData')

# Get summary statistics lists
extr_values = Poly_PET_Ext_sub
Veg_Annual_Summary = NDVI_summary
name_prefix = 'PET'
Quant_percentile=0.90
num_workers = 10
spline_spar = 0

PET_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable, Veg_Annual_Summary,name_prefix,
                                                  Quant_percentile=0.95,return_df=F,num_workers=5,spline_spar = 0,aggregate=T)
extr_values= Poly_ETA_Ext_sub
name_prefix = 'ETA'
ETA_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable, Veg_Annual_Summary,name_prefix,
                                                  Quant_percentile=0.95,return_df=F,num_workers=5,spline_spar = 0,aggregate=T)



# Convert data to panel format --------------------------------------------

Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes_wdata',
                    stringsAsFactors = F)
Polys_sub_data = Polys_sub@data

cbind(Polys_sub_data[1,],PET_summary[[1]])


cbind.fill <- function(...) {                                                      
  require(plyr) # requires plyr for rbind.fill()
  transpoted <- lapply(list(...),t)                
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                        
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                
}

a = cbind.fill(Polys_sub_data[1,],PET_summary[[1]]$ETA_plant_dates)

head(as.data.frame(
  rbindlist(PET_summary[[1]])