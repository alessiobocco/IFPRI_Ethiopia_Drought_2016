# Functions        --------------------------------------------------------

 PlantHarvestDates = function(start_date,end_date,PlantingMonth,PlantingDay,HarvestMonth,HarvestDay){
    # this function takes in date range and returns planting and harvest date for time series
    # set planting
    start_end_years = c(strptime(start_date,'%Y-%m-%d'),strptime(end_date,'%Y-%m-%d'))
    names(unclass(start_end_years[1]))
    start_end_years[1]$mon=PlantingMonth-1
    start_end_years[1]$mday=PlantingDay
    planting = as.Date(seq(start_end_years[1],
      length=strptime(dates[2],'%Y-%m-%d')$year-strptime(dates[1],'%Y-%m-%d')$year,
      by='year'))
    # set harvest
    start_end_years[2]$year=start_end_years[1]$year+1    # set year equal to start year +1
    start_end_years[2]$mon=HarvestMonth-1
    start_end_years[2]$mday=HarvestDay
    harvest = as.Date(seq(start_end_years[2],
      length=strptime(end_date,'%Y-%m-%d')$year-strptime(start_date,'%Y-%m-%d')$year,
      by='year'))
    return(data.frame(planting=planting,harvest=harvest))
  }

# Calculate EVI statistics ------------------------------------------------

  localMaxima <- function(x) {
    if(sum(is.na(x))>0){return()}
    # Use -Inf instead if x is numeric (non-integer)
    y <- diff(c(-.Machine$integer.max, x)) > 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
      y <- y[-1]
    }
    y  }

  AnnualMaxima = function(x,dates_in){
    # returns location of maximum value by year
    datesY = format(dates_in,'%Y')
    a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
    dates_in[x %in% a ]
  }

  AnnualMaximaValue = function(x,dates_in){
    # returns location of maximum value by year
    datesY = format(dates_in,'%Y')
    a=do.call(c,lapply(split(x,datesY),function(x)x[which.max(x)]))
    a}


  AnnualAggregator = function(x,dates_in,FUN){
    # returns an annual summary statistic of any function
    # x = vegetation index data, dates_in = dates of observation POSIX,
    # E.g. AnnualAggregator(x=  plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates, FUN = function(y){mean(y,na.rm=T)})
    datesY = format(dates_in,'%Y')
    do.call(c,lapply(split(x,datesY),FUN))}


  correct_dates = function(dates_in, dates_str, dates_end){
    # handle plant or harvest dates with a different lengths
    # returns corrected start dates in list [[1]] and corrected end dates in [[2]]
    if(class(dates_in)[1]== "POSIXct" )dates_in = as.Date(dates_in)

    length_diff = length(dates_str)-length(dates_end)
    if(length_diff!=1){stop('difference in date lengths can only be =1')}else{
                if(length_diff==1){dates_end =  as.Date(c(as.character(dates_end),
			as.character(dates_in[length(dates_in)])))  }
                if(length_diff==-1){dates_str = c(dates_str,dates_in[1])}
        list(dates_str,dates_end)}
    }


  PeriodAggregator = function(x,dates_in,date_range_st, date_range_end,by_in='days',FUN){
    	# returns a summary statistic of x for any function FUN, over the period defined by date_range_st, date_range_end
      # x = vegetation index data, dates_in = dates of observation POSIX, dates_in,date_range_st = start end dates of period, FUN = function
      # E.g. PeriodAggregator(x=plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,date_range_st=plotdatasmoothed$dates[1],date_range_end=plotdatasmoothed$dates[20], FUN = function(y){mean(y,na.rm=T)})
    	if(class(dates_in)[1]== "POSIXct"|class(dates_in)[1]== "POSIXlt" )dates_in = as.Date(dates_in)
    	if(class(date_range_st)[1]== "POSIXct" ){date_range_st = as.Date(date_range_st)
                                             date_range_end = as.Date(date_range_end)}
    	#Avoid problems with missing plant or harvest dates
    	if(length(date_range_st)!=length(date_range_end)){print('number of elements in start end dates dont match');	break}
    	dataout=lapply(1:length(date_range_st),function(z){
      		DateRange = seq(date_range_st[z],date_range_end[z],by=by_in)
      		x=x[dates_in %in% DateRange]
      		dates_in=dates_in[dates_in %in% DateRange]
      		FUN(x)})
    	dataout = do.call(c,dataout)
      names(dataout)=format(date_range_st,'%Y')
  	  dataout
    }

  PeriodAggregatorDates = function(x,dates_in,date_range_st, date_range_end,by_in='days',FUN){
        # returns a date of summary statistic like max date of x for the period defined by date_range_st, date_range_end
        if(class(dates_in)[1]== "POSIXct"|class(dates_in)[1]== "POSIXlt" )dates_in = as.Date(dates_in)
        if(class(date_range_st)[1]== "POSIXct" ){date_range_st = as.Date(date_range_st)
                                             date_range_end = as.Date(date_range_end)}
        #Avoid problems with missing plant or harvest dates
        if(length(date_range_st)!=length(date_range_end)){print('number of elements in start end dates dont match');
                    break}

        dataout=lapply(1:length(date_range_st),function(z){
            DateRange2 = seq(date_range_st[z],date_range_end[z],by=by_in)
            x2 = x[dates_in %in% DateRange2]
            dates_in2 = dates_in[dates_in %in% DateRange2]
            which_max = which(FUN(x2) ==  x2)
            if(length(which_max)>1){
    		if(length(which_max)>1){
    		   which_max = c(which_max[1],which_max[length(which_max)]) # limit to only 2 
    			if((which_max[2]-which_max[1])==1){
    				which_max=which_max[1]  # favor the first instance of maximum
    			} else if((which_max[2]-which_max[1])==2){
    				which_max=which_max[1]+1 # is seperated by 2 choose middle left
                    	} else if((which_max[2]-which_max[1])==3){
    				which_max=which_max[1]+2} # is seperated by 3 choose middle
    		}
    	    }
            max_dates = dates_in2[which_max]

                })
        dataout = do.call(c,dataout)
        names(dataout)=format(date_range_st,'%Y')
        dataout
    }



  GlobalPeriodAggregator = function(x,dates_in,date_range_st, date_range_end,by_in='days',FUN){
        # returns a *single* summary statistic of x for all periods defined by date_range_st, date_range_end
        if(class(dates_in)[1]== "POSIXct"|class(dates_in)[1]== "POSIXlt" )dates_in = as.Date(dates_in)
        if(class(date_range_st)[1]== "POSIXct" ){date_range_st = as.Date(date_range_st)
                                             date_range_end = as.Date(date_range_end)}
        #Avoid problems with missing plant or harvest dates
        if(length(date_range_st)!=length(date_range_end)){print('number of elements in start end dates dont match');
                break}
        # get data for ranges and cbind then run function
        dataout=lapply(1:length(date_range_st),function(z){
                DateRange = seq(date_range_st[z],date_range_end[z],by=by_in)
                x[dates_in %in% DateRange]})
                dataout = do.call(c,dataout)
                FUN(dataout)
    }


  AnnualMinumumNearDOY = function(x,dates_in,DOY_in){

       # I THINK THIS FUNCTION IS NO LONGER USED... I THINK
    	#x = EVI values, dates=dates of observation POSIX, DOY_in = DOY of rain onset as.Date
    	tempDOY = as.POSIXlt(DOY_in)
    	# avoid problems with time class
    	if(is.na(tempDOY[1])){print('ERROR: convert date format to as.Date()');break}
    	if(class(dates_in)[1]!= 'POSIXlt' ){dates_in=as.POSIXlt(dates_in)}
    	# find all local minima, and match with DOY
    	tempMINdate = dates_in[localMaxima(x*-1)]
    	grid = expand.grid(tempDOY, tempMINdate)
    	# find best minimal per DOY
    	tempout=do.call(rbind,lapply(split(as.numeric(abs(grid[,1]-grid[,2])),
    	    	format(grid[,1],'%Y%j')),function(x)x[which.min(x)]))
    	whichwasmin =  which(as.numeric(abs(grid[,1]-grid[,2])) %in% tempout)
    	grid[whichwasmin,2]
  }


  AnnualAverageDOYvalues = function(x,dates_in){
    	# calculates the average value for DOY for the whole series
    	datesj = format(dates_in,'%j')
    	do.call(c,lapply(split(x,datesj),function(y){mean(y,na.rm=T)}))}



  AnnualAUC = function(x,dates_in){
         # calculate area under the curve by year
         FUN = function(q,w){auc(q,w,type='spline')}
         datesY = format(dates_in,'%Y')
         data.split = split(x,datesY)
         date.split = split(as.numeric(dates_in),datesY)
         dataout = do.call(c,lapply(1:length(data.split),function(z){
                FUN(q=date.split[[z]],w=data.split[[z]])} ))
         names(dataout)=unique(datesY)
         dataout
	}


    AnnualMinumumBeforeDOY = function(x,dates_in,DOY_in,days_shift,dir){
      # calculates the globala minimum for days before,after,both of expected planting date
      # best to set DOY as the last expected date of planting
      
      # x = vegetation index, dates_in = dates of observation POSIX, DOY_in = expected planting or harvest date
      # days_shift = # days to search around DOY_in,  dir='before' 'after' 'beforeafter'
    	
 
    	if(days_shift<=8){print('Using less than 8 days is dangerous, 15-30 stable')}

    	# avoid problems with time class
    	if(is.na(DOY_in[1])){print('ERROR: convert date format to %Y%j');break}
    	if(class(dates_in)[1]!= 'POSIXlt' ){dates_in=as.POSIXlt(dates_in)}

    	# limit to fixed # of days before/after DOY
        DOY_in = as.POSIXlt(DOY_in)
	DOY_before = DOY_in

    	#names(unclass(DOY_before[1]))
    	if(dir=='before') DOY_before$mday=DOY_before$mday-days_shift      # set days before to doy - days_before
    	if(dir=='after') DOY_before$mday=DOY_before$mday+days_shift      # set days before to doy - days_before
        if(dir=='beforeafter'){ DOY_before$mday=DOY_before$mday-days_shift 
		DOY_in$mday=DOY_in$mday+days_shift}
    	DOY_table = data.frame(DOY_before=DOY_before,DOY_in=DOY_in)   #join start end search dates
  
  	# list all days 'days_before' DOY_in
 	if(dir=='before'|dir=='beforeafter'){ DOY_interest = as.POSIXlt(unlist(lapply(1:dim(DOY_table)[1],
			function(h){format(seq(DOY_table[h,1],
	                DOY_table[h,2],by='day'),'%Y-%m-%d')})),tz='UTC')}
	if(dir=='after'){DOY_interest = as.POSIXlt(unlist(lapply(1:dim(DOY_table)[1],
			function(h){format(seq(DOY_table[h,2],
	                DOY_table[h,1],by='day'),'%Y-%m-%d')})),tz='UTC')}

	# find all local minima, and match with DOY
	x_DOY_interest = x[dates_in %in% DOY_interest]
	dates_DOY_interest = dates_in[dates_in %in% DOY_interest]
	# get min value for this period for each year
	sort(AnnualMaxima(x_DOY_interest*-1,as.Date(dates_DOY_interest)))
    }


   PeriodAUC = function(x_in,dates_in,DOY_start_in,DOY_end_in){
         # calculate area under the curve by period of the year using spline estimation
         # x = data, dates_in=asDate(dates),DOY_start_in=asDate(list of start periods),DOY_end_in=asDate(list of end per
         # x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates , DOY_start_in= plant_dates ,DOY_end_in=harvest_dates)
        if(class(dates_in)[1]== "POSIXct"|class(dates_in)[1]== "POSIXlt" )dates_in = as.Date(dates_in)
         dates_group = rep(0,length(dates_in))    # create storage for factors of periods
         # get sequences of periods of inerest
         seq_interest = lapply(1:length(DOY_start_in),function(z){seq(DOY_start_in[z],DOY_end_in[z],by='days')})
         # switch dates-group to period group
         years_avail = sort(as.numeric(unique(unlist(
                lapply(seq_interest,function(z) format(z,'%Y'))))))
         for(z in 1:length(seq_interest)){        #assigns year for beginging of planting season
		            dates_group[dates_in %in% seq_interest[[z]]]=years_avail[z]
                assign('dates_group',dates_group,envir = .GlobalEnv) }  # assign doesn't work in lapply using for loop instead
	      # calculate AUC for periods of interest
         FUN = function(q,w){auc(q,w,type='spline')}
         datesY = format(dates_in,'%Y')
         data.split = split(x_in,dates_group)
         d = do.call(c,lapply(2:length(data.split),function(z){   # start at 2 to avoid group=0
	          	FUN(q=1:length(data.split[[z]]),w=data.split[[z]]) }))
         names(d) = names(data.split)[2:length(data.split)]
         d
	}


   PeriodAUC_method2 = function(x_in,dates_in,DOY_start_in,DOY_end_in){
	        #NOTE SPLINE METHOD 1 SEEMS to WORK BETTER
         # calculate area under the curve by period of the year
         # x = data, dates_in=asDate(dates),DOY_start=asDate(list of start periods),DOY_end=asDate(list of end per$
         # x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates , DOY_start=annualMinumumBeforeDOY(x = plotd$
         if(class(dates_in)[1]== "POSIXct"|class(dates_in)[1]== "POSIXlt" )dates_in = as.Date(dates_in)

         dates_group = rep(0,length(dates_in))    # create storage for factors of periods
         # get sequences of periods of inerest
         seq_interest = lapply(1:length(DOY_start_in),function(z){seq(DOY_start_in[z],DOY_end_in[z],by='days')})
         # switch dates-group to period group
         years_avail = sort(as.numeric(unique(unlist(
                lapply(seq_interest,function(z) format(z,'%Y'))))))
         for(z in 1:length(seq_interest)){        #assigns year for beginging of planting season
                dates_group[dates_in %in% seq_interest[[z]]]=years_avail[z]
                assign('dates_group',dates_group,envir = .GlobalEnv) }  # assign doesn't work in lapply using for loop instead
 
        # calculate AUC for periods of interest
         FUN = function(q,w){  sum(diff(q)*rollmean(w,2))}
         datesY = format(dates_in,'%Y')
         data.split = split(x_in,dates_group)
         d = do.call(c,lapply(2:length(data.split),function(z){   # start at 2 to avoid group=0
                FUN(q=1:length(data.split[[z]]),w=data.split[[z]]) }))
         names(d) = names(data.split)[2:length(data.split)]
         #print(cbind(names(data.split)[2:length(data.split)], d))
         d
        }



 extract_value_point_polygon = function(point_or_polygon, raster_stack, num_workers=1){
  # Returns list containing values from locations of spatial points or polygons from a raster stack object
 	# if polygons are too small reverts to centroid 
      require(doParallel)
      require(parallel)
      if(num_workers==1){registerDoParallel(detectCores() - 1)}else{registerDoParallel(num_workers)}
      if(class(raster_stack)!='list'){raster_stack=list(raster_stack)}
      lapply(c('raster','foreach','doParallel'), require, character.only = T)
      registerDoParallel(num_workers)
      ptm <- proc.time()
      # iterate between points or polygons
      ply_result = foreach(j = 1:length(point_or_polygon),.inorder=T) %dopar%{
            print(paste('Working on feature: ',j,' out of ',length(point_or_polygon)))
            get_class= class(point_or_polygon)[1]
            # switch rasterstack according to which point or polygon is %over%
            for(z in 1:length(raster_stack)){
                    # set raster to use
                    raster_stack_use = raster_stack[[z]]
                    # get cell numbers of point of polygon, repeat if missing
                    if(get_class=='SpatialPolygons'|get_class=='SpatialPolygonsDataFrame'){
                        cell = as.numeric(cellFromPolygon(raster_stack_use, point_or_polygon[j,], weights=F)[[1]])
                        # if polygon is too small to find cells, convert to centroid and get cellfromXY
                       if(length(cell)==0){                                       #coord(poly) returns centroid
                            cell = as.numeric(na.omit(cellFromXY(raster_stack_use, coordinates(point_or_polygon[j,]) )))}}
                    if(get_class=='SpatialPointsDataFrame'|get_class=='SpatialPoints'){
                        cell = as.numeric(na.omit(cellFromXY(raster_stack_use, point_or_polygon[j,])))}
                    # if cells found keep raster_stack_use = raster_stack[[z]]
                    if(length(cell)!=0){break}
                    # if cells not found repeat for different stack or return NA
                    if(length(cell)==0 & z!=length(raster_stack)){next}else{return(NA)}
            }
            # create raster mask from cell numbers
            r = rasterFromCells(raster_stack_use, cell,values=F)
            result = foreach(i = 1:dim(raster_stack_use)[3],.packages='raster',.inorder=T) %dopar% {
               crop(raster_stack_use[[i]],r)
            }
            result=as.data.frame(getValues(stack(result)))
            return(result)
      }
      print( proc.time() - ptm)
      endCluster()
      return(ply_result)
 }



 Neighborhood_quantile=function(extr_values, PlantHarvestTable,Quant_percentile=0.95,num_workers=1,spline_spar = 0){
     # take in values from extract_value_polygon and returns quantile for all raster values wihtin poly
     # if spline_spar = 0, doesn't smooth data, as spline_spar increases smoothing decreases
     # iterate between spatial objects
     require(doParallel)
     require(parallel)
     if(num_workers==1){registerDoParallel(detectCores() - 1)}else{registerDoParallel(num_workers)}
     result_summary=foreach(i = 1:length(extr_values),.packages='raster',.inorder=T) %dopar%{
        if(is.na(extr_values[[i]])){ print('Empty Object');return(NA)} # avoid empties

        # Get dates from stack names
        dats = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(extr_values[[i]])),format='%Y%j')
        # Calculate smoothed values
        if(spline_spar!=0){
        smooth = lapply(1:dim(extr_values[[i]])[1],function(z){SplineAndOutlierRemoval(
            x = as.numeric(extr_values[[i]][z,]),
            dates=as.Date(dats),
            pred_dates=as.Date(dats),spline_spar)})}else{
            smooth = lapply(1:dim(extr_values[[i]])[1],function(z) as.numeric(extr_values[[i]][z,]))    }
	      # calculate quantile for all values over polygon 
        N_Qnt = quantile(x = unlist(smooth),p=Quant_percentile,type=8,na.rm=T)
        N_Qnt     
      }
    result_summary
 }


 ######### Quant_percentile SHOULD BE 0.95 
 
 
 Annual_Summary_Functions=function(extr_values, PlantHarvestTable,Quant_percentile=0.05,aggregate=F,return_df=F,num_workers=5,
	spline_spar = 0){
     # take in values from extract_value_point_polygon and create annual and global summary statistics
     # returns a list where elements are composed of annual and growing season statistics
     # PlantHarvestTable default is table from PlantHarvestDates() for wheat, can do list(wheatPHdates, ricePHdates)
     # if aggregate=T, pixels comprising a polygon are smoothed and then the average signal is obtained, statistics are run from that
     # if return_df==T, returns data frame of summary stats for long form panel
     # if spline_spar = 0, doesn't smooth data, as spline_spar increases smoothing decreases
     # iterate between spatial objects

     if(is.list(PlantHarvestTable)){RicePlantHarvest=PlantHarvestTable[[2]];PlantHarvestTable=PlantHarvestTable[[1]]}

     registerDoParallel(num_workers)
     result_summary=foreach(i = 1:length(extr_values),.packages=c('raster','zoo'),.inorder=T) %dopar%{
        if(is.na(extr_values[[i]])){ print('Empty Object');return(NA)} # avoid empties

        # if aggregate = T, summarize multiple pixels per polygon into one smooth time series

        # create a mean value for input data
      	if(aggregate == T){
              	row_names = names(extr_values[[i]])
              	extr_values[[i]] = t(as.data.frame(colMeans( extr_values[[i]],na.rm=T )))
      		names(extr_values[[i]]) = row_names
      		row.names( extr_values[[i]] ) = NULL}
      
              # Get dates from stack names
              dats = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(extr_values[[i]])),format='%Y%j')
              # Calculate smoothed values
      	if(spline_spar!=0){
              smooth = lapply(1:dim(extr_values[[i]])[1],function(z){SplineAndOutlierRemoval(
                  x = as.numeric(extr_values[[i]][z,]), dates=as.Date(dats),
                  pred_dates=as.Date(dats),spline_spar)})}else{
      	    	  smooth = lapply(1:dim(extr_values[[i]])[1],
		  function(z) as.numeric(extr_values[[i]][z,]))	}
      
              # estimate planting and harvest dates
      	# is spline_spar ==0, dates need to be set by slightly smoothed data
      	if(spline_spar!=0){
              plant_dates = lapply(1:length(smooth),function(z){ AnnualMinumumBeforeDOY(x = smooth[[z]],
                  dates_in = dats, DOY_in=PlantHarvestTable$planting,days_shift=30,dir='beforeafter')})
              harvest_dates = lapply(1:length(smooth),function(z){ AnnualMinumumBeforeDOY(x = smooth[[z]],
                  dates_in = dats, DOY_in=PlantHarvestTable$harvest,days_shift=30,dir='beforeafter')})}

      	# if no smoothing, still need to smooth for harvest and plant dates 
      	if(spline_spar==0){
              smooth_4_dates = lapply(1:dim(extr_values[[i]])[1],function(z){SplineAndOutlierRemoval(
                  x = as.numeric(extr_values[[i]][z,]),
                  dates=as.Date(dats),
                  pred_dates=as.Date(dats),spline_spar=0.2)})
      	      plant_dates = lapply(1:length(smooth_4_dates),function(z){ AnnualMinumumBeforeDOY(x = smooth_4_dates[[z]],
                  dates_in = dats, DOY_in=PlantHarvestTable$planting,days_shift=30,dir='beforeafter')})
              harvest_dates = lapply(1:length(smooth_4_dates),function(z){ AnnualMinumumBeforeDOY(x = smooth_4_dates[[z]],
                  dates_in = dats, DOY_in=PlantHarvestTable$harvest,days_shift=30,dir='beforeafter')})
      	rm(smooth_4_dates)}

        # correct the number of elements in each date vector (assigns last day if no final harvest date available)
        plant_dates = lapply(1:length(plant_dates),function(z){ correct_dates(dates_in= dats, dates_str=plant_dates[[z]],
                dates_end=harvest_dates[[z]])[[1]] })
        harvest_dates = lapply(1:length(plant_dates),function(z){ correct_dates(dates_in= dats, dates_str=plant_dates[[z]],
                dates_end=harvest_dates[[z]])[[2]] })

        # Annual statistics
        A_mn = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)mean(x,na.rm=T))})
        A_min = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)min(x,na.rm=T))})
        A_max = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)max(x,na.rm=T))})
        A_max_Qnt = lapply(1:length(A_max),function(z){rep(quantile(x = A_max[[z]],p=Quant_percentile,type=8,na.rm=T),
                length(A_max[[z]])) }) #quantile of annual max values
        for(z in 1:length(A_max_Qnt)){names(A_max_Qnt[[z]])=names(A_max[[z]])}  # change names

        A_sd = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)sd(x,na.rm=T))})
        A_AUC = lapply(1:length(smooth),function(z){ AnnualAUC(x = smooth[[z]],dates_in = dats) })
        A_AUC_Qnt = lapply(1:length(A_AUC),function(z){rep(quantile(x = A_AUC[[z]],p=Quant_percentile,type=8,na.rm=T),
		        length(A_AUC[[z]])) }) #quantile of auc values
        for(z in 1:length(A_AUC_Qnt)){names(A_AUC_Qnt[[z]])=names(A_AUC[[z]])}  # change names
 	      A_Qnt = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)quantile(x,p=Quant_percentile,type=8,na.rm=T))})
        for(z in 1:length(A_Qnt)){names(A_Qnt[[z]])=names(A_AUC[[z]])}  # change names


        # Growing season statistics
        G_mx_dates = lapply(1:length(smooth),function(z){ PeriodAggregatorDates(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T))})
        G_mn = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) mean(x,na.rm=T)) })
        G_min = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) min(x,na.rm=T)) })
        G_mx =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T)) })
 	G_mx_Qnt = lapply(1:length(G_mx),function(z){rep(quantile(x = G_mx[[z]],p=Quant_percentile,type=8,na.rm=T),
                length(G_mx[[z]])) }) #quantile of annual max values
	    for(z in 1:length(G_mx_Qnt)){names(G_mx_Qnt[[z]])=names(G_mx[[z]])}  # change names

        G_sd =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) sd(x,na.rm=T)) })
        G_AUC = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=plant_dates[[z]],DOY_end_in=harvest_dates[[z]]) })
 	G_AUC_Qnt = lapply(1:length(G_AUC),function(z){rep(quantile(x = G_AUC[[z]],p=Quant_percentile,type=8,na.rm=T),
                length(G_AUC[[z]])) }) #quantile of annual max values
        for(z in 1:length(G_AUC_Qnt)){names(G_AUC_Qnt[[z]])=names(G_AUC[[z]])}  # change names

      	G_AUC2 = lapply(1:length(smooth),function(z){ PeriodAUC_method2(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=plant_dates[[z]],DOY_end_in=harvest_dates[[z]]) })
        G_AUC_leading  = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=plant_dates[[z]],DOY_end_in=G_mx_dates[[z]]) })
        G_AUC_trailing = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=G_mx_dates[[z]],DOY_end_in=harvest_dates[[z]]) })
	      G_Qnt =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) quantile(x,p=Quant_percentile,type=8,na.rm=T))})


    	# G_AUC_trailing lag by one year if growing season is over new year
    	names(G_AUC_trailing[[1]]) = names(G_AUC_leading[[1]])
    	# compare AUC annual to mean AUC and 90th percentile AUC
    	G_AUC_diff_mn = lapply(1:length(smooth),function(z){ G_AUC[[z]] - mean(G_AUC[[z]],na.rm=T) })
    	G_AUC_diff_90th = lapply(1:length(smooth),function(z){ G_AUC[[z]] - quantile(G_AUC[[z]],p=0.9,type=8,na.rm=T) })

    	# global statistics (whole period) 
    	T_G_Qnt = lapply(1:length(smooth),function(z){ rep(GlobalPeriodAggregator(x = smooth[[z]],
            dates_in = dats, date_range_st=plant_dates[[z]],
            date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x)
            quantile(x,p=Quant_percentile,type=8,na.rm=T)),length(G_AUC[[z]])) })
    	for(z in 1:length(T_G_Qnt)){names(T_G_Qnt[[z]])=names(G_AUC[[z]])}  # change names


	####################################################################################
        # Rice Growing season statistics
    	if(exists('RicePlantHarvest')){

  	    # if no smoothing, still need to smooth for harvest and plant dates
        	if(spline_spar==0){
        	    smooth_4_dates = lapply(1:dim(extr_values[[i]])[1],function(z){SplineAndOutlierRemoval(
        	          x = as.numeric(extr_values[[i]][z,]),
        	          dates=as.Date(dats),
        	          pred_dates=as.Date(dats),spline_spar=0.2)})
        	    rice_plant_dates = lapply(1:length(smooth_4_dates),function(z){ AnnualMinumumBeforeDOY(x = smooth_4_dates[[z]],
        	        dates_in = dats, DOY_in=RicePlantHarvest$planting,days_shift=30,dir='beforeafter')})
           	    rice_harvest_dates = lapply(1:length(smooth_4_dates),function(z){ AnnualMinumumBeforeDOY(x = smooth_4_dates[[z]],
        	        dates_in = dats, DOY_in=RicePlantHarvest$harvest,days_shift=30,dir='beforeafter')})
        	    rm(smooth_4_dates)
        	}

 	   # correct the number of elements in each date vector (assigns last day if no final harvest date available)
        	R_mx_dates = lapply(1:length(smooth),function(z){ PeriodAggregatorDates(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T))})
        	R_mn = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',FUN=function(x) mean(x,na.rm=T)) })
        	R_min = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',FUN=function(x) min(x,na.rm=T)) })
        	R_mx =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T)) })
        	R_mx_Qnt = lapply(1:length(R_mx),function(z){rep(quantile(x = R_mx[[z]],p=Quant_percentile,type=8,na.rm=T),
        	        length(R_mx[[z]])) }) #quantile of annual max values
        	    for(z in 1:length(R_mx_Qnt)){names(R_mx_Qnt[[z]])=names(R_mx[[z]])}  # change names

        	R_sd =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',FUN=function(x) sd(x,na.rm=T)) })
        	R_AUC = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
        	        DOY_start_in=rice_plant_dates[[z]],DOY_end_in=rice_harvest_dates[[z]]) })
        	R_AUC_Qnt = lapply(1:length(R_AUC),function(z){rep(quantile(x = R_AUC[[z]],p=Quant_percentile,type=8,na.rm=T),
        	        length(R_AUC[[z]])) }) #quantile of annual max values
        		for(z in 1:length(R_AUC_Qnt)){names(R_AUC_Qnt[[z]])=names(R_AUC[[z]])}  # change names
        	R_AUC2 = lapply(1:length(smooth),function(z){ PeriodAUC_method2(x_in = smooth[[z]],dates_in = dats,
        	        DOY_start_in=rice_plant_dates[[z]],DOY_end_in=rice_harvest_dates[[z]]) })
        	R_AUC_leading  = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
        	        DOY_start_in=rice_plant_dates[[z]],DOY_end_in=R_mx_dates[[z]]) })
        	R_AUC_trailing = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
        	        DOY_start_in=R_mx_dates[[z]],DOY_end_in=rice_harvest_dates[[z]]) })
        	R_Qnt =  lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
        	        dates_in = dats, date_range_st=rice_plant_dates[[z]],
        	        date_range_end=rice_harvest_dates[[z]], by_in='days',
			FUN=function(x) quantile(x,p=Quant_percentile,type=8,na.rm=T))})
      	}


	#####################################################################

    	# collect all data products
    	out = list(smooth_stat = smooth,plant_dates=plant_dates,harvest_dates=harvest_dates,A_mn=A_mn,
		A_min=A_min,A_max=A_max,A_AUC=A_AUC,A_Qnt=A_Qnt,A_sd=A_sd,A_max_Qnt=A_max_Qnt,A_AUC_Qnt=A_AUC_Qnt,
		G_mx_dates=G_mx_dates,G_mn=G_mn,G_min=G_min,G_mx=G_mx,G_AUC=G_AUC,G_Qnt=G_Qnt,G_mx_Qnt=G_mx_Qnt,G_AUC_Qnt=G_AUC_Qnt,G_AUC2=G_AUC2,
		G_AUC_leading=G_AUC_leading,G_AUC_trailing=G_AUC_trailing,G_AUC_diff_mn=G_AUC_diff_mn,
		G_AUC_diff_90th=G_AUC_diff_90th,T_G_Qnt=T_G_Qnt,G_sd=G_sd,
		rice_plant_dates=rice_plant_dates,rice_harvest_dates=rice_harvest_dates,
		R_mx_dates=R_mx_dates,R_mn=R_mn,R_min=R_min,R_mx=R_mx,R_AUC=R_AUC,R_Qnt=R_Qnt,R_mx_Qnt=R_mx_Qnt,R_AUC_Qnt=R_AUC_Qnt,R_AUC2=R_AUC2,
                R_AUC_leading=R_AUC_leading,R_AUC_trailing=R_AUC_trailing
		)
  	out = lapply(out,unlist) # unlist elements
	
  	# convert dates back
  	out$plant_dates = as.Date(out$plant_dates,origin=as.Date('1970-01-01'))
        out$harvest_dates = as.Date(out$harvest_dates,origin=as.Date('1970-01-01'))
        out$G_mx_dates = as.Date(out$G_mx_dates,origin=as.Date('1970-01-01'))

        names(out$plant_dates)=format( out$plant_dates,'%Y') # add year names
        names(out$harvest_dates) = names(out$plant_dates)
	if(exists('RicePlantHarvest')){
		out$rice_plant_dates = as.Date(out$rice_plant_dates,origin=as.Date('1970-01-01'))
	        out$rice_harvest_dates = as.Date(out$rice_harvest_dates,origin=as.Date('1970-01-01'))
		out$R_mx_dates = as.Date(out$R_mx_dates,origin=as.Date('1970-01-01'))
		names(out$rice_plant_dates)=format( out$rice_plant_dates,'%Y') # add year names
  		names(out$rice_harvest_dates) = names(out$rice_plant_dates)
	}
  	# check if data frame or list should be returned
  	if(return_df ==F)return(out)
  	if(return_df ==T){
    		test = lapply(1:length(out),function(x) as.data.frame(out[x]))
    		for(j in 1:length(test)){test[[j]]$row = row.names(test[[j]])} # add rowname for join
    		mymerge = function(x,y){merge(x,y,by='row',all=T)}
    		test = Reduce(mymerge,test[names(out) %in% c("plant_dates","harvest_dates","A_mn","A_min",
    			"A_max","A_AUC",'A_max_Qnt','A_AUC_Qnt','A_Qnt','A_sd',"G_mx_dates","G_mn","G_min",
    			"G_mx","G_AUC",'G_Qnt','G_mx_Qnt','G_AUC_Qnt','G_AUC2',"G_AUC_leading",
    			"G_AUC_trailing","G_AUC_diff_mn",'G_AUC_diff_90th','G_sd','T_G_Qnt',
			'rice_plant_dates','rice_harvest_dates','R_mx_dates','R_mn','R_min','R_mx','R_AUC','R_Qnt','R_mx_Qnt',
			'R_AUC_Qnt','R_AUC2','R_AUC_leading','R_AUC_trailing','R_Qnt') ])
    		test = cbind(i,test)
    		return(test)
  	  } 
    } 
  }



spar_find = function(){
  performance_list =list() 
  for(spar in seq(0,3,by=0.1)){  #smoothing parameter, typically (but not necessarily) in (0,1]. FROM HELP FILE
    evi_summary = Annual_Summary_Functions(extr_values=evi_district,PlantHarvestTable=PlantHarvest,Quant_percentile=0.05,
          aggregate=T, return_df=T,num_workers=13,spline_spar=spar)


  # Merge EVI data with yields
    districts$i = 1:length(districts)
    districts$district = districts$NAME_2

    for(i in 1:length(evi_summary)){
          evi_summary[[i]]=join(evi_summary[[i]], districts@data[,c('i','district','NAME_0','NAME_1','NAME_2')])
          evi_summary[[i]]$year = paste(format(evi_summary[[i]]$plant_dates,'%Y'),format(evi_summary[[i]]$harvest_dates,'%y'),sep='-')
          evi_summary[[i]]=join(evi_summary[[i]], yield[yield$crop=='WHEAT'& yield$season=="RABI",],type='left') #Rabi Kharif Rice Wheat
    }

    yield_evi = na.omit(do.call(rbind,evi_summary))
    yield_evi$season_length = as.numeric(yield_evi$harvest_dates -yield_evi$plant_dates)
    yield_evi$plant_dates = as.numeric(format(yield_evi$plant_dates,'%j'))
    yield_evi$harvest_dates = as.numeric(format(yield_evi$harvest_dates,'%j'))
    yield_evi$G_mx_dates = as.numeric(format(yield_evi$G_mx_dates,'%j'))
    yield_evi$year_trend = as.numeric(  yield_evi$row)
    write.csv(yield_evi,'/groups/manngroup/India_Index/Data/Intermediates/yield_evi.csv')

    lm1=  lm((yield_tn_ha) ~factor(i)+A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
          +G_AUC_trailing+G_AUC_diff_mn +season_length+year_trend+A_sd+G_sd,data=yield_evi)
    performance_list = c(performance_list,spar,summary(lm1)$adj.r.squared,
       mean(sqrt((yield_evi$yield_tn_ha - predict(lm1, yield_evi))^2))/mean(yield_evi$yield_tn_ha))
  }
   matrix(unlist(performance_list),ncol=3,byrow=T)
}




PolygonFromExtent <-function(ext, asSpatial=T, crs=CRS(NA), id=1)
{
  if(class(ext)== "RasterLayer")
  {
    # if raster supplied determine extent and crs then proceed
    crs <- ext@crs
    ext <- extent(ext)
  }
  if(class(ext)== "Extent")
        x1 <- ext@xmin
        x2 <- ext@xmax
        y1 <- ext@ymin
        y2<-ext@ymax

        coords <- matrix(c(x1, y1,
                                           x1, y2,
                                           x2, y2,
                                           x2, y1,
                                           x1, y1), ncol=2, byrow=T)

        poly <- Polygon(coords)
        if(asSpatial)
        {
                spPoly <- SpatialPolygons(list(Polygons(list(poly), ID=id)), proj4string=crs)
                return(spPoly)

        }
        return(poly)

}







mean_neighbors <- function(values,sweights){
  # function calculates the mean value of neighbors values by using sweights
  out2=list()
  for(row in 1:length(values)){
    out2=c(out2,sum(values*sweights[row,]))
  }
  return(as.numeric(out2))
}




#---------------------------------------------------------------------
# This function takes a time series w/ dates (x, dates) and returns a spline smoothed time series with outliers removed.
# Outliers are identified as points with absolute value more than out_sigma * sd, where sd is the residual
# standard deviation between the input data and the initial spline fit, and out_sigma is a variable
# coefficient. The spline smoothing parameter spline_spar controls the smoothness of the fit (see spline.smooth help)
# and out_iterations controls the number of times that outliers are checked and removed w/ subsequent spline refit
# pred_dates is a vector of dates where spline smoothed predictions of x are desired. If NA, then a daily series spanning
# min(dates)-max(dates) is returned

SplineAndOutlierRemovalNA <- function(x, dates, out_sigma=3, spline_spar=0.3, out_iterations=1,pred_dates){
  dates <- as.numeric(dates) # spline doesn't work with dates

  #return NAs if all values are NA
  if(sum(is.na(x))==length(x)){return(rep(NA,length(x)))}

  pred_dates = as.numeric(pred_dates)
  # if prediction dates aren't provided, we assume we want daily ones
  if(is.na(pred_dates[1])){
    pred_dates <- min(dates, na.rm=T):max(dates, na.rm=T)}
  # eliminate outliers and respline
  for(i in 1:out_iterations){
    # fit a smoothing spline to non-missing data
    spl <- try(smooth.spline(dates[!is.na(x)], x[!is.na(x)], spar=spline_spar), silent=T)
    if(inherits(spl, 'try-error')){
      print("Failed to fit smoothing spline")
      return(return(rep(NA,length(x))))
    }
    smooth_x <- try(predict(spl, dates)$y, silent=T) # calculate spline smoothed values
    if(inherits(smooth_x, 'try-error')){
      print("Failed to predict with spline")
      return(return(rep(NA,length(x))))
    }
    smooth_x_resid <- x - smooth_x # calculate residuals from spline
    smooth_x_resid_sd <- try(sd(smooth_x_resid, na.rm=T), silent=T) # standard dev of absolute value of residuals
    if(inherits(smooth_x_resid_sd, 'try-error')){
      print("Failed to get sd of residuals")
      return(return(rep(NA,length(x))))
    }
    outliers <- abs(smooth_x_resid) > out_sigma * smooth_x_resid_sd
    outliers[is.na(outliers)] <- F
    if(sum(outliers) > 0){
      # if we found outliers, eliminate them in x and refit up to iterations
      x[outliers] <- NA
    }else{
      # if we didn't find any outliers, we abandon the iteration and return the smoothed values
      smooth_x_return <- try(predict(spl, pred_dates)$y, silent=T)
      if(inherits(smooth_x_return, 'try-error')){
        print("No outliers, but failed to predict with final spline")
        return(return(rep(NA,length(x))))
      }else{
        return(smooth_x_return)
      }
    }
  }
  # fit the spline to the outlier screened data, then return the predicted series
  spl <- try(smooth.spline(dates[!is.na(x)], x[!is.na(x)], spar=spline_spar), silent=T)
  if(inherits(spl, 'try-error')){
    print("Failed to predict with final spline")
    return(return(rep(NA,length(x))))
  }else{
    smooth_x_return <- try(predict(spl, pred_dates)$y, silent=T)
    if(inherits(smooth_x_return, 'try-error')){
      return(return(rep(NA,length(x))))
    }else{
      return(smooth_x_return)
    }
  }
}


