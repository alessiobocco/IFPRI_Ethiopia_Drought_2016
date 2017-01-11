
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
#library(rts)
library(gdalUtils)
library(foreach)
library(doParallel)
library(compiler)
library(ggplot2)

cl <- makeCluster(32)
registerDoParallel(cl)


# Functions ---------------------------------------------------------------
  
  
  multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
    if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
    return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
  }
  
  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }

functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Profil$



# Set up parameters -------------------------------------------------------

  # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'
  
  # get list of all available modis products
  #GetProducts()
  
  # Product Filters 
  products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
  tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
  dates = c('2010-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02') 
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, /51/ for landcover
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
  strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
  out_dir = 'R:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\VegetationIndex'
  setwd(out_dir)
  
 
  
  # Download MODIS data -----------------------------------------------------
  
  # find all available dates for each product
  available_date_list = list()
  available_products_list = list()
  for(product in products){
    available_date_list = c(available_date_list,list(as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",
            GetDates(location[1], location[2],product)),'%Y%j'))   ))
    available_products_list = c(available_products_list,list(rep(product,length(GetDates(location[1], location[2],product)))))
  }
  
  avail_files_df = data.frame(products=unlist(available_products_list),date=unlist(available_date_list),stringsAsFactors = F)
  avail_files_df$year = strftime(avail_files_df$date, format="%Y")
  avail_files_df$doy = strftime(avail_files_df$date, format="%j")
  avail_files_df$yeardoy = strftime(avail_files_df$date, format="%Y%j")
  avail_files_df
  head(avail_files_df)
  dim(avail_files_df)
  
  # list all files we need to download
  needed_files_df = avail_files_df[ avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days'))  ,] # limit available files to needed date range
  not_needed_files_df = avail_files_df[ !(avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days')) ) ,] # limit available files to needed date range
  head(needed_files_df)
  dim(needed_files_df)
  
  # find all urls for download
  urls = paste(ftp, needed_files_df$products,'/',needed_files_df$year, "/", needed_files_df$doy, "/",sep='')
  junk= foreach(j = 1:length(urls),.packages = 'RCurl') %dopar% {
  #for(j in 1:length(urls)){
      url=urls[j]
      # get urls and limit to wanted tiles
      Sys.sleep(1)
      filenames_url = tryCatch({getURL(url, ftp.use.epsv = F, dirlistonly = T)}, error = function(err) {
                 # getURL fails if you make too many queries, slow down using system pause              
                  print(paste("Your server is pathetic, pausing for 60 seconds: ",err))
                  Sys.sleep(60)
                    tryCatch({getURL(url, ftp.use.epsv = F, dirlistonly = T)}, error = function(err) {
                      # getURL fails if you make too many queries, slow down using system pause              
                      print(paste("Your server is really pathetic, pausing for 60 seconds: ",err))
                      Sys.sleep(60)
                      getURL(url, ftp.use.epsv = F, dirlistonly = T)
                    })
                  })
      filenames_url = paste(url, strsplit(filenames_url, "\r*\n")[[1]], sep = "")
      filenames_url = filenames_url[multi_grep_character(tiles,filenames_url)] # find needed files based on tiles
       
      # get file names from available urls
      write_names=unlist(lapply(1:length(strsplit(filenames_url,'/')),function(x){strsplit(filenames_url,'/')[[x]][length(strsplit(filenames_url,'/')[[x]])]}))
      
      for(i in 1:length(filenames_url)){
          # download as binary and save
          print(write_names[i])
          if(file.exists(paste(out_dir,'/',write_names[i],sep=''))==F){
              print('writing hdf file')
              bin = getBinaryURL(filenames_url[i])
              writeBin(bin, paste(out_dir,'/',write_names[i],sep='')) 
          }else{
              print('Skipping file already exists')
              next
          }
      }
  }
  
  
  
# Find any missing files and download -------------------------------------
  
  # list all files
  files = data.frame(files=list.files(out_dir,pattern=".hdf", all.files=T, full.names=T),stringsAsFactors = F)
  files$short_name =  list.files(out_dir,pattern=".hdf", all.files=T, full.names=F)
  
  # list dates of files downloaded 
  files$dates    = as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",files$files),'%Y%j'))  # Strip dates
  files$products = gsub(paste("^.*(",paste(products,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  files$tiles    = gsub(paste("^.*(",paste(tiles,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  files$yeardoy  = strftime(files$dates, format="%Y%j")
  files$reproj_files = paste(gsub("[.]006.*.hdf$", "\\1",files$short_name,perl=T) )
  files
  
  # find files not listed 
  missing_dates =  outersect(paste(files$products,files$dates,files$tiles,sep=' '), 
	apply(MARGIN=1,X=expand.grid(paste(needed_files_df$products,needed_files_df$date,sep=' '),tiles), 
	FUN=function(x){paste(x,collapse=' ')} )  )
  missing_dates
  # check dates in year,doy
  format(strptime('2010-05-25','%Y-%m-%d'),'%Y%j')
  

  
# Get Names of all Layers in HDF ------------------------------------------


  get_subdatasets('./MYD13Q1.A2015361.h22v08.006.2016012202549.hdf')
  
    
# Reproject ---------------------------------------------------------------

  
  band_subset = "0 0 0 0 0 0 0 0 0 0 1 0"  # EVI 1 1 0 0 0 0 0 0 0 0 1 1# Example: first seven and last layer'1 1 1 1 1 1 1 0 0 0 0 1" landcover= "1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0"
  output_pattern = '250m_16_days_EVI.tif' # '250m_16_days_EVI.tif' looks for existing EVI tif files to avoid repeating  Land_Cover_Type_1.tif
   
  for (i in (1:length(files$reproj_files))){
      print(i)
      print(paste(i,'out of',length(files$reproj_files)))
      print(paste("Writing out tiffs ", list.files('.',pattern =  files$reproj_files[i] ),' for date ',files$yeardoy[i]))
      tifs = list.files(getwd(),pattern =  output_pattern)
      
      if(length(tifs[grep(tifs,pattern=paste(files$products[i],files$yeardoy[i],files$tiles[i],sep='_'))])>=1){ print('File exists')
          next
      }else{
          print(paste('Input:',files$reproj_files[i],' Output:',paste(files$products[i],'_',files$yeardoy[i],'.tif',sep='')))
          reprojectHDF(hdfName = files$short_name[i],
                     filename=paste(files$products[i],'_',files$yeardoy[i],'_',files$tiles[i],'.tif',sep=''),  
                     MRTpath=MRT, proj_type='SIN', 
                     proj_params='6371007.181 0 0 0', 
                     datum='NODATUM', pixel_size=250,
                     bands_subset=band_subset)
        }
  }
  

  
#   MODIS SINUSOIDAL PROJECITON DETAILS:
#  This is what I use in gdal:
#     uly_map = 10007554.677
#   ulx_map = -20015109.354
#   pix = 463.312716525
#   proj4 = '+proj=sinu +a=6371007.181 +b=6371007.181 +units=m' 

  

# Stack relevant data -----------------------------------------------------

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/VegetationIndex/')  # folder where  EVI .tifs are
  # create data stack for each variable and tile 
 
  foreach(product =  c('composite_day_of_the_year',
 	 'EVI','NDVI','pixel_reliability')) %dopar% {  
    for( tile_2_process in  tiles){
  	 # Set up data
  	 flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_',product,'.tif$',sep='')), 
 	 	 full.names = TRUE)
  	 flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  	 flist = flist[order(flist_dates)]  # file list in order
         flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order

  	 # stack data and save
  	 stacked = stack(flist)
  	 names(stacked) = flist_dates
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

  foreach(product = c('EVI','NDVI')) %do% {
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
         assign(paste(product,'stack',tile_2_process,'wo_clouds_clean_smooth',sep='_'),stacked)
         save( list=paste(product,'stack',tile_2_process,'wo_clouds_clean_smooth',sep='_') ,
              file = paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/',
              product,'_stack_',tile_2_process,'_wo_clouds_clean_smooth','.RData',sep='') )
  }}

  
  
# Remove low quality cells & assign projection ------------------------------------------------
  # load data in previous section and run common dates
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data//Data Stacks')

  reliability_prefix = 'pixel_reliability'
  products2removeclouds = c('composite_day_of_the_year','EVI','NDVI')
  for(product in products2removeclouds){
  for( tile in tiles){
	print(paste('Working on',product,tile))
	# load quality flag
        reliability_stackvalues = get(paste(reliability_prefix,'_stack_',tile,sep=''))

	# remove clouds from produt
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        crs(data_stackvalues) ='+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'

	foreach(i=1:dim(data_stackvalues)[3]) %dopar% { 
		data_stackvalues[[i]][reliability_stackvalues[[i]]!=0]=NA}
        assign(paste(product,'_stack_',tile,sep=''),data_stackvalues)
        dir.create(file.path('../Data Stacks/WO Clouds'), showWarnings=F,recursive=T) # create stack directory i$
	save(list=paste(product,'_stack_',tile,sep=''),
		file = paste('WO Clouds/',product,'_stack_',tile,'_wo_clouds.RData',sep=''))
  }} 
  


# Rescale and set valid ranges of data  ---------------------------------------------
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data//Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)
  dir.create(file.path('../Data Stacks/WO Clouds Clean/tifs/'), showWarnings=F,recursive=T) # create dir for tifs


  # setup a dataframe with valid ranges and scale factors
  valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,
		scale=0.0001,stringsAsFactors=F)
  valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
  valid 

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

  # Loop through valid ranges  
  products2clean = unique(valid$stack)
  for(product in products2clean){
  for( tile in tiles){
        print(paste('Working on',product,tile))	
        # load product data
        lapply(dir1[grep(product,dir1)],load,.GlobalEnv) 
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        valid_values = valid[grep(product,valid$stack),]

	ScaleClean = function(x){
        	x[x==as.numeric(valid_values$fill)]=NA
        	x[x < as.numeric(valid_values$validL)]=NA
        	x[x > as.numeric(valid_values$validU)]=NA
        	#x = x * as.numeric(valid_values$scale)
        	x}
        junk = foreach(i=1:dim(data_stackvalues)[3]) %dopar% {
                clean = ScaleClean(data_stackvalues[[i]])
		writeRaster(clean, filename=paste('WO Clouds Clean/tifs/',product,'_stack_',tile,'_wo_clouds_clean_',
                 names(data_stackvalues[[i]]),'.tif',sep=''), overwrite=T)
 	        return(0)
		} 

  }}


# Restack clean data -----------------------------------------------------

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean/tifs/')  # folder where  EVI .tifs are
  # create data stack for each variable and tile

  foreach(product =  c('EVI','NDVI')) %do% {
    for( tile_2_process in  tiles){
	 print(paste('processing',product,tile_2_process,sep=' '))
         # Set up data
         flist = list.files(".",glob2rx(paste(product,'_stack_',tile_2_process,'*','.tif$',sep='')),
                 full.names = TRUE)
         flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
         flist = flist[order(flist_dates)]  # file list in order
         flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order

         # stack data and save
         stacked = stack(flist)
         names(stacked) = flist_dates
         assign(paste(product,'stack',tile_2_process,'wo_clouds_clean',sep='_'),stacked)
         save( list=paste(product,'stack',tile_2_process,'wo_clouds_clean',sep='_') ,
                 file = paste('../../WO Clouds Clean/',product,'_stack_',
		 tile_2_process,'_wo_clouds_clean','.RData',sep='') )
  }}







# Limit to crop signal ----------------------------------------------------

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean/') # don't load smoothed... 

  # load data stacks from both directories
  dir1 = list.files('.','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')
  
  #### get original training dataset from previous study #### 
  ET = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/AdminBoundaries/','ETH_adm0')
  ET = spTransform(ET, CRS(projection(EVI_stack_h21v07_wo_clouds_clean)))

  classes=list(dryag=c(2,15,82,123,129,153,162,198),
  	wetag=c(7,11,16,18,27,41,47,53,54,77,85,90,91,100,102,105,106,117,119,122,125,133,150,151,152,166,173,178,182,195),
  	agforest=c(6,8,9,21,32,61,95,111,128,131,137,143,145,168,169,186,191,193),
  	arid = c(13,29,56,69,71,78,96,149,156),
  	semiarid=c(5,14,19,23,24,26,30,35,36,37,40,43,44,45,50,57,59,60,63, 64,73,75,76,80,92,103,107,108,114,135,136,141,
	146,158,167,170,179,197,200),
  	shrub=c(1,12,17,22,25,39,46,48,49,52,55,62,66,67,72,87,88,89,93,97,104,109,110,113,116,118,132,138,142,144,147,148,154,
	159,160,164,175,185,187,188,189,190,192),
  	forest=c(3,4,10,28,31,38,42,51,58,68,74,79,81,84,86,94,98,99,101,115,120,121,124,126,127,130,139,140,157,161,163,165,
	171,174,176,180,181,183,184,194,196),
  	wetforest=c(20,33,34,65,70,83,112,134,155,199),
  	water=c(172,177))

  points = unlist(getKMLcoordinates('./LUTrainingPoints_1stStudy.kml')) # get coordinates from original training data
  dim(points) = c(3,200)
  points = t(points)
  points = data.frame(points)
  names(points)=c('lon','lat','z')
  points$class = NA
  for (i in 1:length(classes)){
    points$class[classes[[i]]]=names(classes)[i]
  }
  coordinates(points) = ~ lon + lat
  proj4string(points) = "+proj=longlat +ellps=WGS84 +datum=WGS84"
  points = spTransform(points, CRS(projection(NDVI_stack_h21v07_wo_clouds_clean_smooth)))

  #  writeOGR(points, dsn=".", layer="LUTrainingPoints", driver="ESRI Shapefile") 

  plot(NDVI_stack_h21v07_wo_clouds_clean_smooth[[1]])
  plot(points,add=T)


  #### Train classifier ####
  library(randomForest)
  library(e1071)
  library(data.table)

  # extract time series (MUST BE DONE ON SHORT OR LARGER MEMORY NODE, DOESN"T WORK ON DEFQ OR DEBUG)
  
  NDVI = extract_value_point_polygon(points,list(NDVI_stack_h21v07_wo_clouds_clean,
	NDVI_stack_h21v08_wo_clouds_clean,NDVI_stack_h22v07_wo_clouds_clean,
	NDVI_stack_h22v08_wo_clouds_clean),25)
  save(NDVI, file = paste('./NDVI_200p_LUClasses.RData',sep='') )
  load('./NDVI_200p_LUClasses.RData')

  NDVI = rbindlist(NDVI, fill=T) # convert list to table
  NDVI_dates = gsub("^.*X([0-9]{7}).*$", "\\1",names(NDVI),perl = T)  # Strip dates
  NDVI = as.data.frame.matrix(NDVI)

  NDVI_smooth = NDVI
  # smooth evi sample to avoid problems with NAs
  for(i in 1:dim(NDVI)[1]){
    NDVI_smooth[i,] = SplineAndOutlierRemovalNA(x = NDVI[i,], dates=NDVI_dates, pred_dates=NDVI_dates,spline_spar = 0.4)
  }

  # add class data
  NDVI_smooth$Class = as.factor(points$class) # add classification

  formula1 = Class ~ .

  set.seed(10)
  rf <- randomForest(formula1, data=na.omit(NDVI_smooth), ntree=2000, proximity=T)
  table(predict(rf), NDVI_smooth$Class)
  print(rf)
  plot(rf)
  varImpPlot(rf)

  source( '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/mctune.R')
  rf_ranges = list(ntree=c(seq(1,1000,100),seq(1000,8000,500)),mtry=seq(5,15,2))
  set.seed(10)
  tuned.rf = mctune(method = randomForest, train.x = formula1, data = na.omit(NDVI_smooth),
         tunecontrol = tune.control(sampling = "cross",cross = 5), ranges=rf_ranges,
         mc.control=list(mc.cores=20, mc.preschedule=T),confusionmatrizes=T )
  save(tuned.rf, file = paste('./NDVI_tuned_rf.RData',sep='') )
  load('./NDVI_tuned_rf.RData')

  tuned.rf$best.model
  plot(tuned.rf)

  # WORKS WELL LEARN HOW TO TUNE
  svm_model1 <- svm(formula1,na.omit(NDVI_smooth))
  table(predict(svm_model1), NDVI_smooth$Class)


  # Predict land class --------------------------------------------------

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed') # don't load smoothed...

  # load data stacks from both directories
  dir1 = list.files('.','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')


  cl = makeCluster(15)
  beginCluster(cl)
  lc = clusterR(NDVI_stack_h21v07_wo_clouds_clean_smooth, predict, args=list(model = svm_model1))
  endCluster()


  lc =  predict(NDVI_stack_h21v07_wo_clouds_clean,tuned.rf$best.model)
  plot(lc)

  writeRaster(lc,'/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/h21v07_lc.tif')



  
# Extract polygon or points data from stacks ------------------------

  # get polygon data 
  #ogrInfo('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
  #Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
  #Polys = spTransform(Polys, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
  #writeOGR(Polys, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/", 
  #     layer="EnumerationAreasSIN", driver="ESRI Shapefile") # this is in geographical projection
  Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN')
  Polys$id = 1:dim(Polys@data)[1]



  # extract values croped to point or polygon

  out2 = extract_value_point_polygon(Polys[1:200,],list(NDVI_stack_h22v08_wo_clouds_clean,NDVI_stack_h22v07_wo_clouds_clean,
		NDVI_stack_h21v08_wo_clouds_clean,NDVI_stack_h21v07_wo_clouds_clean),16)














# Visualize examples of smoothed data -------------------------------------


  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')
  load('./NDVI_200p_LUClasses.RData')
  library(data.table)
  NDVI = rbindlist(NDVI)
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







    
##############################
# attempt to make stack mc apply function

  
  
  

 stack_out_raster__mc_function <- function(stack_in,fun_in,workers){
    require(foreach,doParallel,raster,rgdal)

    #Determine optimal block size for loading in MODIS stack data
    block_width = 100
    nrows = dim(stack_in)[1]
    nblocks <- nrows%/%block_width
    bs_rows <- seq(1,nblocks*block_width+1,block_width)
    bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
    #print('Working on the following rows')
    #print(paste(bs_rows))

    #Register the parallel backend
    registerDoParallel(workers)

    result <- foreach(i = 1:length(bs_rows), .combine = rbind,.inorder=T) %dopar% {
      print(paste("Working on block",i))
      stack_values = getValues(stack_in, bs_rows[i], bs_nrows[i])
      return( as.data.frame(unlist(lapply(stack_values,FUN=fun_in ))))
      out=      with(stack_values,fun_in)

     }

   out_raster = stack_in[[1]]
   out_raster = setValues(out_raster,result[,1])
   print('stacking output')
   nrows = dim(stack_in)[1]
   ncols = dim(stack_in)[2]
   dim(result) = c(nrows,ncols)
   a_raster = stack_in[[1]]
   a_raster[] = result
   stopImplicitCluster()
   return(stack_in)
  }
a = NDVI_stack_h21v07_wo_clouds_clean[[1:20]]
i = 1
stack_in = a
workers = 20
fun_in= function(x) mean(x,na.rm=F)
b = stack_mcsummary_function(a,fun_in= function(x) mean(x,na.rm=F),20)


