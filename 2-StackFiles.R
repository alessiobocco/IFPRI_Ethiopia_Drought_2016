# This script takes outputs from DownloadMODISFTP_Rcurl.R and stacks them




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


# Stack Raw data -----------------------------------------------------


registerDoParallel(5)
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/VegetationIndex/')  # folder where  EVI .tifs are
# create data stack for each variable and tile 

foreach(product =  c('composite_day_of_the_year',
                     'EVI','NDVI','pixel_reliability')) %dopar% {  
                       for( tile_2_process in  tiles){
                         # Set up data
                         print('stacking')
                         flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_',product,'.tif$',sep='')), 
                                            full.names = TRUE)
                         flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
                         flist = flist[order(flist_dates)]  # file list in order
                         flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
                         
                         # stack data and save
                         stacked = stack(flist)
                         names(stacked) = flist_dates
                         
                         # assign projection 
                         crs(stacked) ='+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'
                         # save
                         assign(paste(product,'stack',tile_2_process,sep='_'),stacked)
                         dir.create(file.path('../Data Stacks/Raw Stacks/'), showWarnings=F,recursive=T) # create stack directory if doesnt exist
                         save( list=paste(product,'stack',tile_2_process,sep='_') ,
                               file = paste('../Data Stacks/Raw Stacks/',product,'_stack_',tile_2_process,'.RData',sep='') )
                       }}




# Limit stacks to common dates -------------------------------------------


setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')

# load data stacks from both directories
stack_types_2_load =c('composite_day_of_the_year','EVI','NDVI','pixel_reliability')
dir1 = list.files('./Data Stacks/Raw Stacks/','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

# limit stacks to common elements
for(product in stack_types_2_load ){  
  for( tile in tiles){
    # find dates that exist in all datasets for current tile
    all_dates = lapply(paste(stack_types_2_load,'stack',tile,sep='_'),function(x){names(get(x))})
    # restrict to common dates 
    common_dates = Reduce(intersect, all_dates)
    # subset stacks for common dates  
    assign(paste(product,'_stack_',tile,sep=''),subset( get(paste(product,'_stack_',tile,sep='')), 
                                                        common_dates, drop=F) )
    print('raster depth all equal')
    print( all.equal(common_dates,names(get(paste(product,'_stack_',tile,sep=''))))   )
    print(dim(get(paste(product,'_stack_',tile,sep='')))[3])
  }}






# stack smoother -----------------------------------------------------
# this stack is used for land cover classification only (bc classifier can't have NA values)

rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Raw Stacks/') # don't load smoothed...

# load data stacks from both directories
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)


for( i in ls(pattern = "NDVI_stack*")){
  print('##############################################################')
  dir.create(file.path(getwd(), i), showWarnings = FALSE)
  print(paste('Starting processing of:',i))
  stack_in = get(i)
  stack_name = i
  dates =   as.numeric(gsub("^.*X([0-9]{7}).*$", "\\1",names(stack_in),perl = T))  # Strip dates
  pred_dates = dates
  spline_spar=0.4  # 0.4 for RF
  workers = 20
  out_dir = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/'
  stack_smoother(stack_in,dates,pred_dates,spline_spar,workers,stack_name,out_dir)
}


for( i in ls(pattern = "EVI_stack*")){
  print('##############################################################')
  dir.create(file.path(getwd(), i), showWarnings = FALSE)
  print(paste('Starting processing of:',i))
  stack_in = get(i)
  stack_name = i
  dates =   as.numeric(gsub("^.*X([0-9]{7}).*$", "\\1",names(stack_in),perl = T))  # Strip dates
  pred_dates = dates
  spline_spar=0.4  # 0.4 for RF
  workers = 20
  out_dir = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/'
  stack_smoother(stack_in,dates,pred_dates,spline_spar,workers,stack_name,out_dir)
}



# Restack Smoothed Files  ----------------------------------------------------


setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/Tifs/')  # folder where  EVI .tifs are
# create data stack for each variable and tile

# load data stacks from both directories
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

foreach(product = c('NDVI','EVI')) %do% {
  for( tile_2_process in  tiles){
    print(paste('processing',product,tile_2_process,sep=' '))
    # Set up data
    flist = list.files(".",glob2rx(paste(product,'_',tile_2_process,'*','.tif$',sep='')),
                       full.names = TRUE)
    flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
    flist = flist[order(flist_dates)]  # file list in order
    flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
    
    # stack data and save
    stacked = stack(flist)
    names(stacked) = flist_dates
    assign(paste(product,'stack',tile_2_process,'smooth',sep='_'),stacked)
    save( list=paste(product,'stack',tile_2_process,'smooth',sep='_') ,
          file = paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/',
                       product,'_stack_',tile_2_process,'_smooth','.RData',sep='') )
  }}




# Remove low quality,CLEAN, & assign projection FROM RAW, THEN STACK ------------------------------------------------


# load data in previous section and run common dates
rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Raw Stacks/') # don't load smoothed...
# load data stacks from both directories
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

# set up directories and names
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data//Data Stacks')
reliability_prefix = 'pixel_reliability'
dir.create(file.path('./WO Clouds/Tifs'), showWarnings=F,recursive=T)
dir.create(file.path('./WO Clouds Clean/tifs'), showWarnings=F,recursive=T)
dir.create(file.path('/lustre/groups/manngroup/WO Clouds Clean/Tifs'), showWarnings=F,recursive=T) # folder on high speed ssd drive

registerDoParallel(25)


# setup a dataframe with valid ranges and scale factors
valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,
                   scale=0.0001,stringsAsFactors=F)
valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
valid


for(product in  c('EVI','NDVI')){  #'EVI','NDVI'
  for( tile in tiles){
    print(paste('Working on',product,tile))
    # load quality flag
    reliability_stackvalues = get(paste(reliability_prefix,'_stack_',tile,sep=''))
    
    # remove clouds from produt
    data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
    valid_values = valid[grep(product,valid$stack),]
    
    ScaleClean = function(x,y){
      x[x==as.numeric(valid_values$fill)]=NA
      x[x < as.numeric(valid_values$validL)]=NA
      x[x > as.numeric(valid_values$validU)]=NA
      #x = x * as.numeric(valid_values$scale)
      x[ y<0 | y>1 ] = NA     # remove very low quality  
      x}
    # process and write to lustre
    foreach(i=(1:dim(data_stackvalues)[3]), .inorder=F) %dopar% { 
      print(i)
      data_stackvalues[[i]] = ScaleClean(data_stackvalues[[i]],reliability_stackvalues[[i]])
      writeRaster(data_stackvalues[[i]],paste('/lustre/groups/manngroup/WO Clouds Clean/Tifs/',product,'_',tile,
                                              '_',names(data_stackvalues[[i]]),'.tif',sep=''),overwrite=T)
    }
    
    # Copy files back from lustre and delete lustre
    flist = list.files("/lustre/groups/manngroup/WO Clouds Clean/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = T)
    fname = list.files("/lustre/groups/manngroup/WO Clouds Clean/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = F)
    file.copy(from=flist, to=paste("./WO Clouds Clean/tifs",fname,sep='/'), 
              overwrite = T, recursive = F, copy.mode = T)
    file.remove(flist)
    
    # Restack outputs 
    print(paste('Restacking',product,tile,sep=' '))
    # Set up data
    flist = list.files("./WO Clouds Clean/tifs/",glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = T)
    flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
    flist = flist[order(flist_dates)]  # file list in order
    flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
    
    # stack data and save 
    stacked = stack(flist)
    names(stacked) = flist_dates
    assign(paste(product,'stack',tile,'wo_clouds_clean',sep='_'),stacked)
    save( list=paste(product,'stack',tile,'wo_clouds_clean',sep='_') ,
          file = paste('./WO Clouds Clean/',product,'_stack_',
                       tile,'_wo_clouds_clean','.RData',sep='') )
  }} 





# Limit to crop signal ----------------------------------------------------
#  Class Codes:
#  1 agforest 2 arid 3 dryag 4 forest 5 semiarid 6 shrub 7 water 8 wetag 9 wetforest


# load data in previous section and run common dates
rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean/') # don't load smoothed...
dir.create(file.path('../WO Clouds Clean LC/tifs/'), showWarnings=F,recursive=T) # create dir for tifs
dir.create(file.path('/lustre/groups/manngroup/WO Clouds Clean LC/Tifs'), showWarnings=F,recursive=T) # folder on high speed


# load data stacks from both directories
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

# set up directories and names
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data//Data Stacks')
landcover_prefix = 'smooth_lc_svm_mn.tif'
landcover_path = '../LandUseClassifications/'

registerDoParallel(25)

for(product in  c('EVI','NDVI')){  #'EVI','NDVI'
  for( tile in tiles){
    print(paste('Working on',product,tile))
    # load quality flag
    lc_stackvalues = raster(paste(landcover_path,'NDVI','_stack_',tile,'_',landcover_prefix,sep=''))
    
    # remove clouds from produt
    data_stackvalues = get(paste(product,'_stack_',tile,'_wo_clouds_clean',sep=''))
    
    foreach(i=(1:dim(data_stackvalues)[3]), .inorder=F) %dopar% {
      print(i)
      data_stackvalues[[i]][lc_stackvalues[[i]]==2|lc_stackvalues[[i]]==7|
                              lc_stackvalues[[i]]==9]=NA
      writeRaster(data_stackvalues[[i]],paste('/lustre/groups/manngroup/WO Clouds Clean LC/Tifs/'
                                              ,product,'_',tile,'_',names(data_stackvalues[[i]]),
                                              '_Clean_LC','.tif',sep=''),overwrite=T)
    }
    
    # Copy files back from lustre and delete lustre
    flist = list.files("/lustre/groups/manngroup/WO Clouds Clean LC/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = T)
    fname = list.files("/lustre/groups/manngroup/WO Clouds Clean LC/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = F)
    file.copy(from=flist, to=paste("./WO Clouds Clean LC/tifs",fname,sep='/'),
              overwrite = T, recursive = F, copy.mode = T)
    file.remove(flist)
    print(paste('Restacking',product,tile,sep=' '))
    
    # Set up data
    flist = list.files("./WO Clouds Clean LC/tifs/",glob2rx(paste(product,'_',tile,'*','.tif$',sep='')),full.names = T)
    flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
    flist = flist[order(flist_dates)]  # file list in order
    flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
    
    # stack data and save
    stacked = stack(flist)
    names(stacked) = flist_dates
    assign(paste(product,'stack',tile,'WO_Clouds_Clean_LC',sep='_'),stacked)
    save( list=paste(product,'stack',tile,'WO_Clouds_Clean_LC',sep='_') ,
          file = paste('./WO_Clouds_Clean_LC/',product,'_stack_',
                       tile,'_WO_Clouds_Clean_LC','.RData',sep='') )
  }}

