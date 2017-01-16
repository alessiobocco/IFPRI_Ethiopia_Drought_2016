
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

#cl <- makeCluster(32)
#registerDoParallel(cl)


# Compile Functions ---------------------------------------------------------------
  

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
#  out_dir = 'R:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\VegetationIndex'
#  setwd(out_dir)
  
 

  
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
		x[ y!=0 & y!=1 ] = NA     # remove very low quality  
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


