
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
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.ha$


# Set up parameters -------------------------------------------------------


  # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'

  # get list of all available modis products
  #GetProducts()

  # Product Filters
  products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-5$
  tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
  dates = c('2010-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02')
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, /51/ for landcover
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
  strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available d$
  out_dir = 'R:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\VegetationIndex'
  setwd(out_dir)




# Limit to crop signal ----------------------------------------------------

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/') # don't load smoothed...

  # load data stacks from both directories
  dir1 = list.files('.','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')


  #### get original training dataset from previous study ####

  ET = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/AdminBoundaries/','ETH_adm0')
  ET = spTransform(ET, CRS(projection(NDVI_stack_h21v07_smooth)))

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
  points = spTransform(points, CRS(projection(NDVI_stack_h21v07_smooth)))
  #  writeOGR(points, dsn=".", layer="LUTrainingPoints", driver="ESRI Shapefile")

  plot(NDVI_stack_h21v07_smooth[[1]])
  plot(points,add=T)


  ot_training = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/',
	'NewTrainingData', stringsAsFactors=F)
  ot_training = spTransform(ot_training, CRS(projection(NDVI_stack_h21v07_smooth)))
  table(ot_training$Class)

  ot_training$Class[ot_training$Class=='dryagri'] = 'dryag'
  ot_training$Class[ot_training$Class=='agriforest'] = 'agforest'
  ot_training$Class[ot_training$Class=='wetagri'] = 'wetag'
  names(ot_training) = c('z','class')
  points =  rbind(points,ot_training)


  #### Train classifier ####
  library(randomForest)
  library(e1071)
  library(data.table)

  # add other important features  (run seperatately in parallel)
  #beginCluster(10,type="SOCK")
  #f2 <- function(x) calc(x, mean)
  #setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/')
  #NDVI_stack_h21v07_smooth_mean=clusterR(NDVI_stack_h21v07_smooth, f2)
  #writeRaster(NDVI_stack_h21v07_mean,'NDVI_stack_h21v07_smooth_mean.tif')
  #NDVI_stack_h21v08_smooth_mean=clusterR(NDVI_stack_h21v08_smooth,f2)
  #writeRaster(NDVI_stack_h21v08_smooth_mean,'NDVI_stack_h21v08_smooth_mean.tif')
  #NDVI_stack_h22v07_smooth_mean=clusterR(NDVI_stack_h22v07_smooth,f2)
  #writeRaster(NDVI_stack_h22v07_smooth_mean,'NDVI_stack_h22v07_smooth_mean.tif')
  #NDVI_stack_h22v08_smooth_mean=clusterR(NDVI_stack_h22v08_smooth,f2)
  #writeRaster(NDVI_stack_h22v08_smooth_mean,'NDVI_stack_h22v08_smooth_mean.tif')
  #endCluster()

  beginCluster(10,type="SOCK")
  f2 <- function(x) calc(x, sd)
  #setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/')
  #NDVI_stack_h21v07_smooth_sd=clusterR(NDVI_stack_h21v07_smooth, f2)
  #writeRaster(NDVI_stack_h21v07_smooth_sd,'NDVI_stack_h21v07_smooth_sd.tif')
  #NDVI_stack_h21v08_smooth_sd=clusterR(NDVI_stack_h21v08_smooth,f2)
  #writeRaster(NDVI_stack_h21v08_smooth_sd,'NDVI_stack_h21v08_smooth_sd.tif')
  #NDVI_stack_h22v07_smooth_sd=clusterR(NDVI_stack_h22v07_smooth,f2)
  #writeRaster(NDVI_stack_h22v07_smooth_sd,'NDVI_stack_h22v07_smooth_sd.tif')
  #NDVI_stack_h22v08_smooth_sd=clusterR(NDVI_stack_h22v08_smooth,f2)
  #writeRaster(NDVI_stack_h22v08_smooth_sd,'NDVI_stack_h22v08_smooth_sd.tif')
  #endCluster()


  #beginCluster(5,type="SOCK")
  #f2 <- function(x) calc(x, max)
  #setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/')
  #NDVI_stack_h21v07_smooth_mx=clusterR(NDVI_stack_h21v07_smooth, f2)
  #writeRaster(NDVI_stack_h21v07_smooth_mx,'NDVI_stack_h21v07_smooth_mx.tif')
  #NDVI_stack_h21v08_smooth_mx=clusterR(NDVI_stack_h21v08_smooth,f2)
  #writeRaster(NDVI_stack_h21v08_smooth_mx,'NDVI_stack_h21v08_smooth_mx.tif')
  #NDVI_stack_h22v07_smooth_mx=clusterR(NDVI_stack_h22v07_smooth,f2)
  #writeRaster(NDVI_stack_h22v07_smooth_mx,'NDVI_stack_h22v07_smooth_mx.tif')
  #NDVI_stack_h22v08_smooth_mx=clusterR(NDVI_stack_h22v08_smooth,f2)
  #writeRaster(NDVI_stack_h22v08_smooth_mx,'NDVI_stack_h22v08_smooth_mx.tif')
  #endCluster()



  # add mean & sd to stacks
  layers_2_add = c('mean','sd','mx','tile') # 'mean','sd','tile' :  Tile adds tile number for training

  foreach(product = c('NDVI')) %do% {
     for( tile in  tiles){
          stack_name = paste(product,'_stack_',tile,'_smooth',sep='') 
	  stack_data = get(stack_name)

	  zs = dim(stack_data)[3]  # use as index
	  for(layer in layers_2_add){
		# assign computed raster data to end of stack
		if(layer!='tile'){stack_data[[zs+1]]=raster(
			paste('../Data Stacks/Smoothed/',product,'_stack_',tile,'_smooth_',layer,'.tif',sep=''))}
		if(layer=='tile'){  # assign stack number for training (minimize edge)
			hnumb =   gsub("^.*h([0-9]{2}).*$", "\\1",stack_name,perl = T)
			vnumb =   gsub("^.*v([0-9]{2}).*$", "\\1",stack_name,perl = T)
		 	stack_data[[zs+1]]=as.numeric(paste(hnumb,vnumb,sep=''))}
	        # name layer
		names(stack_data[[zs+1]])=layer
		print(names(stack_data[[zs+1]]))
         	assign(paste(product,'_stack_',tile,'_smooth',sep=''),stack_data)
		zs=zs+1  # update index
	  }
     }
  }


  # extract time series (MUST BE DONE ON SHORT OR LARGER MEMORY NODE, DOESN"T WORK ON DEFQ OR DEBUG)
  NDVI = extract_value_point_polygon(points,list(NDVI_stack_h21v07_smooth,
       NDVI_stack_h21v08_smooth,NDVI_stack_h22v07_smooth,
       NDVI_stack_h22v08_smooth),20)

  #save(NDVI, file = paste('./NDVI_200p_LUClasses_mnsdmx_newtrain.RData',sep='') )
  load('./NDVI_200p_LUClasses_mnsdmx_newtrain.RData')

  # create a dataframe from list
  NDVI = rbindlist(NDVI, fill=T) # convert list to table
  NDVI_dates = gsub("^.*X([0-9]{7}).*$", "\\1",names(NDVI),perl = T)  # Strip dates
  NDVI = as.data.frame.matrix(NDVI)
  NDVI_smooth = as.matrix(NDVI)

  # fill NAs (water) with row means
  k <- which(is.na(NDVI_smooth), arr.ind=TRUE)
  NDVI_smooth[k] <- rowMeans(NDVI_smooth, na.rm=TRUE)[k[,1]]
  NDVI_smooth = as.data.frame(NDVI_smooth)


  # add class data
  NDVI_smooth$Class = as.factor(points$class) # add classification

  formula1 = Class ~ .

  set.seed(10)
  rf <- randomForest(formula1, data=na.omit(NDVI_smooth), ntree=2000, proximity=T)
  table(predict(rf), NDVI_smooth$Class)
  print(rf)
  plot(rf)
  varImpPlot(rf)

  #source( '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/mctune.R')
  #rf_ranges = list(ntree=c(seq(1,1000,100),seq(1000,8000,500)),mtry=seq(5,15,2))
  #set.seed(10)
  #tuned.rf = mctune(method = randomForest, train.x = formula1, data = na.omit(NDVI_smooth),
  #       tunecontrol = tune.control(sampling = "cross",cross = 5), ranges=rf_ranges,
  #       mc.control=list(mc.cores=20, mc.preschedule=T),confusionmatrizes=T )
  #save(tuned.rf, file = paste('./NDVI_tuned_rf.RData',sep='') )
  #load('./NDVI_tuned_rf.RData')

  #tuned.rf$best.model
  #plot(tuned.rf)

 # WORKS WELL LEARN HOW TO TUNE
  svm_model1 <- svm(formula1,na.omit(NDVI_smooth))
  table(predict(svm_model1), NDVI_smooth$Class)
  #save(svm_model1,
  #  file = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/svm_model1.RData')

  set.seed(10)
  obj <- tune.svm(formula1, data = na.omit(NDVI_smooth), gamma =
   seq(.5, .9, by = .1), cost = seq(100,1000, by = 100))

  set.seed(10)
  svm_tuned<-mctune(confusionmatrizes=T,
          mc.control=list(mc.cores=15, mc.preschedule=F), method=svm,
          ranges=list(type='C',kernel='radial',gamma=3^(-10:-1),cost=3^(-8:8)),
          train.x=formula1,data = na.omit(NDVI_smooth),validation.x=NULL,  validation.y=NULL,
          tunecontrol=tune.control(sampling='cross',cross=3,performances=T,nrepeat=5,best.model=T))

  save(svm_tuned,
    file = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/svm_model_tuned_mnsdmx.RData')

  svm_tuned



  # Predict land class --------------------------------------------------
  #  Class Codes:
  #  1 agforest 2 arid 3 dryag 4 forest 5 semiarid 6 shrub 7 water 8 wetag 9 wetforest


  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/')
  load('./svm_model_tuned_mnsdmx.RData')

  for(stacks in c('NDVI_stack_h21v07_smooth','NDVI_stack_h21v08_smooth','NDVI_stack_h22v07_smooth',
                'NDVI_stack_h22v08_smooth')){
        beginCluster(10)
        print(paste('predicting stack',stacks))
        lc = clusterR(get(stacks), predict, args=list(model = svm_tuned$best.model))
        endCluster()
        writeRaster(lc,paste('./',stacks,'_lc_svm_mn.tif',sep=''),
                overwrite=T)
  }


