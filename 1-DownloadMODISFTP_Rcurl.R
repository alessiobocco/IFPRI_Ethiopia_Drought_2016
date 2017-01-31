
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
#source('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_Code/ModisDownload.R')
source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')
#source('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_Code/SummaryFunctions.R')



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
  products =  c('MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
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

  
  band_subset = "1 1 0 0 0 0 0 0 0 0 1 1"  # EVI 1 1 0 0 0 0 0 0 0 0 1 1# Example: first seven and last layer'1 1 1 1 1 1 1 0 0 0 0 1" landcover= "1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0"
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

  








# 
#     
# ##############################
# # attempt to make stack mc apply function   NOT WORKING 
#  stack_out_raster__mc_function <- function(stack_in,fun_in,workers){
#     require(foreach,doParallel,raster,rgdal)
# 
#     #Determine optimal block size for loading in MODIS stack data
#     block_width = 100
#     nrows = dim(stack_in)[1]
#     nblocks <- nrows%/%block_width
#     bs_rows <- seq(1,nblocks*block_width+1,block_width)
#     bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
#     #print('Working on the following rows')
#     #print(paste(bs_rows))
# 
#     #Register the parallel backend
#     registerDoParallel(workers)
# 
#     result <- foreach(i = 1:length(bs_rows), .combine = rbind,.inorder=T) %dopar% {
#       print(paste("Working on block",i))
#       stack_values = getValues(stack_in, bs_rows[i], bs_nrows[i])
#       return( as.data.frame(unlist(lapply(stack_values,FUN=fun_in ))))
#       out=      with(stack_values,fun_in)
# 
#      }
# 
#    out_raster = stack_in[[1]]
#    out_raster = setValues(out_raster,result[,1])
#    print('stacking output')
#    nrows = dim(stack_in)[1]
#    ncols = dim(stack_in)[2]
#    dim(result) = c(nrows,ncols)
#    a_raster = stack_in[[1]]
#    a_raster[] = result
#    stopImplicitCluster()
#    return(stack_in)
#   }
# a = NDVI_stack_h21v07_wo_clouds_clean[[1:20]]
# i = 1
# stack_in = a
# workers = 20
# fun_in= function(x) mean(x,na.rm=F)
# b = stack_mcsummary_function(a,fun_in= function(x) mean(x,na.rm=F),20)


