
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







# JUNK BELOW #############






# CLEAN UP 
clean = read.table('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/AgSS/CleaningFiles/AgSSCodeClean2.txt',
	header=F, sep='',strip.white=T)

conn <- file('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/AgSS/CleaningFiles/AgSSCodeClean2.txt',open="r")
linn <-readLines(conn)
strsplit(linn, 'EA_NAME')





clean = data.frame(OLD_EA_NAME =

matrix(c("023-05",'0123-0',"Weserbi Guto",
"032-03","032--0","Huluko Birbisa",
"014-04","0014-0","Balaref",
"025-05","0025-0","Layiyeduge",
"020-01","020-o1","Derajaresso",
"010-01","01001","Girari Reh",
"001-01","00101","Pekedu",
"007-01","00701" ,"Yerer"
"007-01","00701" & RK_NAME=="Chefe Konichi"

),ncol=3,byrow=T)


replace EA_NAME="007-01" if EA_NAME=="00701" & RK_NAME=="Chefe Konichi"
replace EA_NAME="007-03" if EA_NAME=="00703" & RK_NAME=="Chefe Konichi"
replace EA_NAME="004-01" if EA_NAME=="00401" & RK_NAME=="Debir agonat"
replace EA_NAME="004-02" if EA_NAME=="00402" & RK_NAME=="Leada"
replace EA_NAME="005-02" if EA_NAME=="00502" & RK_NAME=="Lefesa Germegi"
replace EA_NAME="018-02" if EA_NAME=="01802" & RK_NAME=="Darona"
replace EA_NAME="074-03" if EA_NAME=="07403" & RK_NAME=="Hantezo"
replace EA_NAME="029-03" if EA_NAME=="02903" & RK_NAME=="Mendide"
replace EA_NAME="028-03" if EA_NAME=="02803" & RK_NAME=="Cheqesasisha"
replace EA_NAME="027-03" if EA_NAME=="02703" & RK_NAME=="Loma"
replace EA_NAME="006-03" if EA_NAME=="006-3" & RK_NAME=="Hedishadi"
replace EA_NAME="013-03" if EA_NAME=="013-3" & RK_NAME=="Dibdibo"
replace EA_NAME="017-04" if EA_NAME=="01704" & RK_NAME=="Jara hinesa"
replace EA_NAME="009-04" if EA_NAME=="009-4" & RK_NAME=="Tilmo"
replace EA_NAME="024-04" if EA_NAME=="02404" & RK_NAME=="Chocha"
replace EA_NAME="037-08" if EA_NAME=="03708" & RK_NAME=="Debirzuria Adisgetsion"
replace EA_NAME="037-04" if EA_NAME=="03704" & RK_NAME=="Debirzuria Adisgetsion"
replace EA_NAME="007-04" if EA_NAME=="007-4" & RK_NAME=="May Weyine"
replace EA_NAME="006-04" if EA_NAME=="00604" & RK_NAME=="Medhani Alem"
replace EA_NAME="001-05" if EA_NAME=="00105" & RK_NAME=="Yegagina"
replace EA_NAME="031-05" if EA_NAME=="03105" & RK_NAME=="Sada Korkora"
replace EA_NAME="035-06" if EA_NAME=="03506" & RK_NAME=="Gidena Aborat"
replace EA_NAME="019-06" if EA_NAME=="01906" & RK_NAME=="Tulu Kore"
replace EA_NAME="017-07" if EA_NAME=="01707" & RK_NAME=="Wutie"
replace EA_NAME="003-07" if EA_NAME=="00307" & RK_NAME=="Atsefit yekendat"
replace EA_NAME="031-07" if EA_NAME=="03107" & RK_NAME=="Qura Jege"
replace EA_NAME="004-08" if EA_NAME=="00408" & RK_NAME=="Gola Kure"
replace EA_NAME="031-09" if EA_NAME=="031-9" & RK_NAME=="Kupor"
replace EA_NAME="014-09" if EA_NAME=="013-9" & RK_NAME=="Abenet"
replace EA_NAME="030-04" if EA_NAME=="030-34" & RK_NAME=="Leamuguna"
replace EA_NAME="012-01" if EA_NAME=="012-0" & RK_NAME=="Gobi MIkael"
replace EA_NAME="006-01" if EA_NAME=="006-0" & RK_NAME=="Bocho"
replace EA_NAME="004-01" if EA_NAME=="004- 1" & RK_NAME=="Abdaarba"
replace EA_NAME="019-01" if EA_NAME=="19-01" & RK_NAME=="Debre kebire"
replace EA_NAME="019-02" if EA_NAME=="19-02" & RK_NAME=="Debre kebire"
replace EA_NAME="019-03" if EA_NAME=="19-03" & RK_NAME=="Debre kebire"
replace EA_NAME="019-04" if EA_NAME=="19-04" & RK_NAME=="Debre kebire"
replace EA_NAME="019-05" if EA_NAME=="19-05" & RK_NAME=="Debre kebire"
replace EA_NAME="008-01" if EA_NAME=="08-01" & RK_NAME=="Ketena 2 Menider 30"
replace EA_NAME="004-02" if EA_NAME=="" & RK_NAME=="Lafito Yemenigist Den"
replace EA_NAME="004-01" if EA_NAME=="" & RK_NAME=="Jeyi Yemenigist Den"
replace EA_NAME="004-04" if EA_NAME=="" & RK_NAME=="Yemenigist Den"
replace EA_NAME="040-01" if EA_NAME=="00" & RK_NAME=="May Cadera Geter"
replace EA_NAME="020-01" if EA_NAME=="00" & RK_NAME=="Adebaye Geter"
replace EA_NAME="030-01" if EA_NAME=="00" & RK_NAME=="Biher Geter"

replace EA_NAME="002-13" if EA_NAME=="401" & RK_NAME=="bedeno serte"
replace EA_NAME="006-08" if EA_NAME=="401" & RK_NAME=="Shekila"

replace EA_NAME="020-01" if EA_NAME=="" & RK_NAME=="Sefafi Erisha"
replace EA_NAME="021-01" if EA_NAME=="" & RK_NAME=="Alatish Park"
replace EA_NAME="022-01" if EA_NAME=="" & RK_NAME=="Not covered by field work"

replace EA_NAME="015-01" if EA_NAME=="" & RK_NAME=="Agensera"
replace EA_NAME="016-01" if EA_NAME=="" & RK_NAME=="Akule"
replace EA_NAME="014-01" if EA_NAME=="" & RK_NAME=="Dibanina wuhin"
*Lafito Yemenigist Den
replace EA_NAME="020-02" if EA_NAME=="021-02" & RK_NAME=="Dalia"
replace EA_NAME="017-01" if EA_NAME=="013-01" & RK_NAME=="Hoticha" 
replace EA_NAME="008-03" if EA_NAME=="008-02" & RK_NAME=="Moleogodo" & x_coord > 280000
replace EA_NAME="022-05" if EA_NAME=="002-05" & RK_NAME=="Sito" 

*THIS IS A PROBLEM, three EA's all labeled the same so I just gave them independent values but may not match up with other data
replace EA_NAME="075-02" if EA_NAME=="075-01" & RK_NAME=="Langaw Mekala" & x_coord > 409000
replace EA_NAME="075-03" if EA_NAME=="075-01" & RK_NAME=="Langaw Mekala" & x_coord > 407000
replace EA_NAME="001-02" if EA_NAME=="001-01" & RK_NAME=="Keltewo" & x_coord > 340000
replace EA_NAME="001-03" if EA_NAME=="001-01" & RK_NAME=="Keltewo" & x_coord > 338000 & x_coord < 338500
*Another renamed but not sure
replace EA_NAME="009-03" if EA_NAME=="009-05" & RK_NAME=="Anigiyo Kola" & y_coord > 803047 & y_coord < 803500
replace EA_NAME="013-02" if EA_NAME=="013-01" & RK_NAME=="Aera" & x_coord > 140000 
replace EA_NAME="014-06" if EA_NAME=="014-05" & RK_NAME=="Zazi" & x_coord > 145618 & x_coord < 145620
replace EA_NAME="017-03" if EA_NAME=="017-02" & RK_NAME=="Ankozuza" & x_coord > 254700
replace EA_NAME="022-06" if EA_NAME=="022-05" & RK_NAME=="Shifode Debar" & x_coord > 429900 & x_coord < 430000
replace EA_NAME="024-02" if EA_NAME=="024-01" & RK_NAME=="Addis boder" & x_coord > 309000
replace EA_NAME="027-01" if EA_NAME=="026-01" & RK_NAME=="Arga becho" 
replace EA_NAME="027-02" if EA_NAME=="026-02" & RK_NAME=="Arga becho" 
*These next three seem particularly dubious
replace EA_NAME="035-01" if EA_NAME=="028-01" & RK_NAME=="Malditi mashuncha" 
replace EA_NAME="035-02" if EA_NAME=="028-02" & RK_NAME=="Malditi mashuncha" 
replace EA_NAME="035-03" if EA_NAME=="028-03" & RK_NAME=="Malditi mashuncha" 
*less dubious
replace EA_NAME="017-06" if EA_NAME=="017-01" & RK_NAME=="Layinyaw Argoba" & y_coord > 631000 
replace EA_NAME="028-02" if EA_NAME=="028-01" & RK_NAME=="Waka Araba" & x_coord < 188300 
replace EA_NAME="018-02" if EA_NAME=="018-01" & RK_NAME=="Bura" & x_coord > 265000 & y_coord < 565000
*Another dubious allocation of kebele code
replace EA_NAME="011-01" if EA_NAME=="005-01" & RK_NAME=="Wamura Berikos'he" 
replace EA_NAME="011-02" if EA_NAME=="005-02" & RK_NAME=="Wamura Berikos'he" 
replace EA_NAME="011-03" if EA_NAME=="005-03" & RK_NAME=="Wamura Berikos'he" 
replace EA_NAME="011-04" if EA_NAME=="005-04" & RK_NAME=="Wamura Berikos'he" 
*Less dubious again
replace EA_NAME="009-03" if EA_NAME=="009-08" & RK_NAME=="Borena" & x_coord > 455100 & x_coord < 455120
replace EA_NAME="005-06" if EA_NAME=="005-05" & RK_NAME=="Andida" & x_coord > 427340 & x_coord < 427350
*This one is a mess and should be checked with mapping to see it's location
replace EA_NAME="028-01" if EA_NAME=="025-05" & RK_NAME=="Hure" & x_coord > 466000 & x_coord < 466010
*Better
replace EA_NAME="005-01" if EA_NAME=="003-01" & RK_NAME=="Gureta" 
replace EA_NAME="005-02" if EA_NAME=="003-02" & RK_NAME=="Gureta" 
replace EA_NAME="005-03" if EA_NAME=="003-03" & RK_NAME=="Gureta" 
replace EA_NAME="005-04" if EA_NAME=="003-04" & RK_NAME=="Gureta" 
replace EA_NAME="011-01" if EA_NAME=="001-01" & RK_NAME=="Beshiro Gute" 
replace EA_NAME="023-02" if EA_NAME=="023-01" & RK_NAME=="Mutana Mursheno" & x_coord > 456600 
replace EA_NAME="014-06" if EA_NAME=="014-04" & RK_NAME=="Terche" & x_coord < 416000 
replace EA_NAME="002-02" if EA_NAME=="022-02" & RK_NAME=="Andenya Fero"
replace EA_NAME="005-06" if EA_NAME=="005-02" & RK_NAME=="Ololicho" & x_coord > 363240 & x_coord < 363260
replace EA_NAME="016-04" if EA_NAME=="016-03" & RK_NAME=="Chuche" & x_coord < 470100
replace EA_NAME="012-04" if EA_NAME=="012-03" & RK_NAME=="Langute Chafe" & x_coord > 389000
replace EA_NAME="024-01" if EA_NAME=="021-01" & RK_NAME=="Bonadi Bero"
replace EA_NAME="001-05" if EA_NAME=="001-01" & RK_NAME=="Kuno Kertefa" & x_coord < 440100
replace EA_NAME="401-02" if EA_NAME=="401-01" & RK_NAME=="Brale Irsha Lmat" & x_coord < 276000
replace EA_NAME="022-03" if EA_NAME=="022-02" & RK_NAME=="ZONIGAWACHA" & x_coord > 345000
replace EA_NAME="030-02" if EA_NAME=="010-02" & RK_NAME=="Semenwa Gebeta" 
replace EA_NAME="035-05" if EA_NAME=="033-05" & RK_NAME=="1st Akema" 
replace EA_NAME="002-01" if EA_NAME=="" & RK_NAME=="UnCovered by Field Work" & W_NAME=="Dima" 
replace EA_NAME="021-02" if EA_NAME=="021-01" & RK_NAME=="Pakuwes" & y_coord > 910000
replace EA_NAME="006-02" if EA_NAME=="006-01" & RK_NAME=="Wanke" & y_coord > 946000
replace EA_NAME="001-01" if EA_NAME=="" & RK_NAME=="Sekele" 
*Back to Dubious, Gergetesa name seems to be in two woredas maybe mislabeled woreda name?
replace EA_NAME="032-01" if EA_NAME=="009-02" & RK_NAME=="GERGETESA" 
*Replace town code
replace T_CODE=3 if W_NAME=="Aleiltu" & RK_NAME=="Beke Town"
replace EA_NAME="004-05" if EA_NAME=="005-05" & RK_NAME=="Kisane M /Alem" 
replace EA_NAME="008-09" if EA_NAME=="008-07" & RK_NAME=="Hana Jiru Gedam" & x_coord < 453000
replace EA_NAME="015-11" if EA_NAME=="015-01" & RK_NAME=="Melisa Hakwa" & x_coord < 755800
replace EA_NAME="015-04" if EA_NAME=="015-06" & RK_NAME=="Dere Kebisa" & x_coord > 824000
replace EA_NAME="024-01" if EA_NAME=="021-03" & RK_NAME=="Pocket Land"
replace EA_NAME="022-01" if EA_NAME=="002-01" & RK_NAME=="Amuma Bubisa"
replace EA_NAME="002-08" if EA_NAME=="002-07" & RK_NAME=="Usemani P .Land"
replace EA_NAME="026-09" if EA_NAME=="026-06"& RK_NAME=="Kelecha Muriti" & x_coord < 430000
replace EA_NAME="022-01" if EA_NAME=="004-01" & RK_NAME=="Jeyi Yemenigist Den"
replace EA_NAME="023-01" if EA_NAME=="004-04" & RK_NAME=="Yemenigist Den"
replace EA_NAME="016-01" if EA_NAME=="016-02"& RK_NAME=="Keku Kalu" & x_coord < 320000
replace EA_NAME="004-02" if EA_NAME=="004-01"& RK_NAME=="Sendabo Dengoro" & x_coord < 310000
replace EA_NAME="013-01" if EA_NAME=="013-03"& RK_NAME=="Nensebo chebi" 
replace EA_NAME="006-02" if EA_NAME=="006-01"& RK_NAME=="Nonodino" & x_coord > 98000
replace EA_NAME="020-01" if EA_NAME=="020-04"& RK_NAME=="Jajo Akakil" & x_coord < 76000
replace EA_NAME="004-01" if EA_NAME=="004-02"& RK_NAME=="Charu"
replace EA_NAME="039-01" if EA_NAME==""& RK_NAME=="Edo Laki Island" & x_coord < 475000
replace EA_NAME="039-02" if EA_NAME==""& RK_NAME=="Edo Laki Island"
replace EA_NAME="010-01" if EA_NAME=="001-01"& RK_NAME=="Ilasa Hagala"
replace EA_NAME="017-01" if EA_NAME=="002-01"& RK_NAME=="UnCovered by Field Work"
replace EA_NAME="001-02" if EA_NAME=="002-02"& RK_NAME=="Chulule Habera"
replace T_CODE=3 if RK_NAME=="401 Yemenigist Den"
replace EA_NAME="401-03" if EA_NAME=="401-01"& RK_NAME=="Geredeala Ersha Limat"
replace EA_NAME="003-03" if EA_NAME=="002-03"& RK_NAME=="Bejiro"
replace EA_NAME="022-01" if EA_NAME=="013-01"& RK_NAME=="Geja Gaje"
replace EA_NAME="004-02" if EA_NAME=="004-01" & RK_NAME=="Charu" & x_coord > 263000
replace EA_NAME="004-01" if EA_NAME=="004-02" & RK_NAME=="Sendabo Dengoro" & x_coord < 307000
replace EA_NAME="006-03" if EA_NAME=="006-02" & RK_NAME=="Nonodino" & x_coord < 98000
replace EA_NAME="020-05" if EA_NAME=="020-01" & RK_NAME=="Jajo Akakil" & x_coord < 75200

replace EA_NAME="022-01" if EA_NAME=="013-01"& RK_NAME=="Geja Gaje"
replace EA_NAME="401-01" if EA_NAME==""& RK_NAME=="Sama Yearbtoader Akababi"
replace EA_NAME="401-02" if EA_NAME==""& RK_NAME=="Yemengist Den" & W_NAME=="Anchar"
replace EA_NAME="012-03" if EA_NAME=="012-01" & RK_NAME=="SIBO" & x_coord < 166000
replace EA_NAME="014-01" if EA_NAME=="014-02" & RK_NAME=="Hamuma" & x_coord > 151000
replace EA_NAME="019-02" if EA_NAME=="019-01" & RK_NAME=="Wenige Berikege" & x_coord > 327000
replace EA_NAME="035-08" if EA_NAME=="035-07" & RK_NAME=="Weynma Werqima" & x_coord > 299800

replace EA_NAME="011-02" if EA_NAME=="006-02" & RK_NAME=="Santime Yeshoh" 
replace EA_NAME="017-02" if EA_NAME=="018-02" & RK_NAME=="Damot Tseyion" 
replace EA_NAME="017-03" if EA_NAME=="018-03" & RK_NAME=="Damot Tseyion" 
replace EA_NAME="011-03" if EA_NAME=="010-03" & RK_NAME=="Wededer"
replace EA_NAME="005-04" if EA_NAME=="004-04" & RK_NAME=="Buda Kera"
replace EA_NAME="404" if EA_NAME=="405" & RK_NAME=="Yemenigist Den" & x_coord < 212000
replace EA_NAME="027-01" if EA_NAME=="025-01" & RK_NAME=="ABIDELA" 
replace EA_NAME="027-01" if EA_NAME=="026-01" & RK_NAME=="Chemoso" 
replace EA_NAME="002-02" if EA_NAME=="001-02" & RK_NAME=="Nego Agu" 
replace EA_NAME="017-04" if EA_NAME=="014-04" & RK_NAME=="Zelma Shembekuma" 
replace EA_NAME="002-02" if EA_NAME=="001-02" & RK_NAME=="Shebele" 
replace EA_NAME="014-10" if EA_NAME=="014-01" & RK_NAME=="Robit" & x_coord < 329000
replace EA_NAME="021-02" if EA_NAME=="021-01" & RK_NAME=="Wenidata" & x_coord < 322000
replace EA_NAME="007-04" if EA_NAME=="007001" & RK_NAME=="Addisina Gulit"
replace T_CODE=2 if W_NAME=="Enarj Enawuga" & RK_NAME=="Felege Birihan Town"
replace T_CODE=1 if W_NAME=="Enarj Enawuga" & RK_NAME=="Lumame Town"
replace EA_NAME="016-01" if EA_NAME=="016-03" & RK_NAME=="014 Zihon Wiha" & x_coord > 435000
replace EA_NAME="021-02" if EA_NAME=="021-03" & RK_NAME=="Enese Qole" & x_coord > 407500
replace T_CODE=2 if W_NAME=="Awabel" & RK_NAME=="Wejel Town"
replace T_CODE=1 if W_NAME=="Awabel" & RK_NAME=="Lumame Town"
replace EA_NAME="033-02" if EA_NAME=="029-02" & RK_NAME=="Gurigur Minata" 
replace EA_NAME="020-04" if EA_NAME=="019-04" & RK_NAME=="Abergina" 
replace EA_NAME="007-01" if EA_NAME=="006-01" & RK_NAME=="Tirahina" 
replace EA_NAME="022-01" if EA_NAME=="021-01" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-02" if EA_NAME=="021-02" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-03" if EA_NAME=="021-03" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-04" if EA_NAME=="021-04" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-05" if EA_NAME=="021-05" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-06" if EA_NAME=="021-06" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-07" if EA_NAME=="021-07" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="022-08" if EA_NAME=="021-08" & RK_NAME=="Abedebay Tseyon"
replace EA_NAME="035-01" if EA_NAME=="033-01" & RK_NAME=="Debresalam"
replace EA_NAME="035-02" if EA_NAME=="033-02" & RK_NAME=="Debresalam"
replace EA_NAME="035-03" if EA_NAME=="033-03" & RK_NAME=="Debresalam"
replace EA_NAME="035-04" if EA_NAME=="033-04" & RK_NAME=="Debresalam"
replace EA_NAME="002-10" if EA_NAME=="002-01" & RK_NAME=="Garasge {35}" & x_coord > 250000
replace EA_NAME="007-04" if EA_NAME=="007-02" & RK_NAME=="Bete Semaniya {29}" & x_coord > 260300
replace EA_NAME="012-11" if EA_NAME=="012-01" & RK_NAME=="Kezen Shahura {02}" & x_coord > 270000
replace EA_NAME="013-05" if EA_NAME=="013-03" & RK_NAME=="Atuga {07}" & x_coord > 274300
replace EA_NAME="012-06" if EA_NAME=="010-06" & RK_NAME=="Chinifera"
replace EA_NAME="010-03" if EA_NAME=="012-03" & RK_NAME=="Adamo Mihiret Abina"
replace EA_NAME="014-01" if EA_NAME=="012-01" & RK_NAME=="Adet"
replace EA_NAME="014-01" if EA_NAME=="015-01" & RK_NAME=="Hadish Adi"
replace EA_NAME="014-03" if EA_NAME=="015-03" & RK_NAME=="Hadish Adi"
replace EA_NAME="029-02" if EA_NAME=="029-01" & RK_NAME=="Huletu Simina" & x_coord > 393000
replace EA_NAME="023-03" if EA_NAME=="023-02" & RK_NAME=="Wanga giten" & y_coord < 1035000
replace EA_NAME="013-02" if EA_NAME=="013-03" & RK_NAME=="MAY TUOM" & x_coord < 490000
replace EA_NAME="011-03" if EA_NAME=="011-05" & RK_NAME=="Dereka" & x_coord > 473000
replace EA_NAME="008-01" if EA_NAME=="008-04" & RK_NAME=="Maychikanta" & x_coord< 511000
replace EA_NAME="013-10" if EA_NAME=="013-01" & RK_NAME=="Adihuta" & x_coord < 460000
replace EA_NAME="006-01" if EA_NAME=="006-04" & RK_NAME=="Shewit lemlem" & x_coord < 540000
replace EA_NAME="023-11" if EA_NAME=="023-01" & RK_NAME=="Adi Alem" & x_coord < 350000
replace EA_NAME="002-02" if EA_NAME=="002-03" & RK_NAME=="Ginida Temem" & x_coord < 379000
replace EA_NAME="037-01" if EA_NAME=="" & RK_NAME=="Sisa Yekerikeha Den" 
replace EA_NAME="020-02" if EA_NAME=="02-02" & RK_NAME=="Aleamer"
replace T_CODE=1 if W_NAME=="Mandura" & RK_NAME=="Genete Mariam Town"
replace EA_NAME="015-01"  if W_NAME=="Mandura" & RK_NAME=="Genete Mariam Town"
replace EA_NAME="002-05" if EA_NAME=="002-02" & RK_NAME=="Alaga Dore" & x_coord ==.
*In Afar there are a few kebeles with just woreda codes
replace EA_NAME="012-01" if EA_NAME=="" & RK_NAME=="Asayiten"
replace EA_NAME="013-01" if EA_NAME=="" & RK_NAME=="Hayiten"
*These seem to be zurias around the towns. I gave them unique but random numbers
replace EA_NAME="015-01" if EA_NAME=="" & RK_NAME=="Melka Sedi"
replace EA_NAME="016-01" if EA_NAME=="" & RK_NAME=="Andido"
replace EA_NAME="016-01" if EA_NAME=="" & RK_NAME=="Mankush Geter"
replace EA_NAME="011-01" if EA_NAME=="001-01" & RK_NAME=="Sekele"
replace EA_NAME="002-04" if EA_NAME=="Soge" & RK_NAME=="Belo Jiganifodo"
replace EA_NAME="025-01" if EA_NAME=="" & RK_NAME=="Sefi Yegileseb Yeirisha Meret"
replace EA_NAME="024-01" if EA_NAME=="Manibu" & RK_NAME=="Manibuk"
replace EA_NAME="023-03" if EA_NAME=="023-02" & RK_NAME=="Wanga giten" & y_coord < 1037000
*This is a blank value in Hudene Woreda, I assigned it a random value but mapping would check where it is
replace EA_NAME="018-01" if EA_NAME=="" & W_NAME=="Hundene" & x_coord < 838000
*Back to simple corrections
replace T_CODE=3 if W_NAME=="Semen Mekele_Mekele" & RK_NAME=="Mekele Town" & x_coord == .
replace EA_NAME="011-05" if EA_NAME=="001-05" & RK_NAME=="Adi Geshei"
replace EA_NAME="016-11" if EA_NAME=="16-401" & RK_NAME=="Lemele"
replace EA_NAME="017-12" if EA_NAME=="17-401" & RK_NAME=="Mesarete"
replace EA_NAME="003-07" if EA_NAME=="003-03" & RK_NAME=="Gogakemese" & x_coord == .
replace EA_NAME="014-01" if EA_NAME=="013401" & RK_NAME=="Agid Kiregna"
replace EA_NAME="002-03" if EA_NAME=="001-03" & RK_NAME=="Korhumer 01"
replace EA_NAME="022-04" if EA_NAME=="023-04" & RK_NAME=="Btezaze"
replace EA_NAME="020-01" if EA_NAME=="" & RK_NAME=="Farm Land" & W_NAME=="Metema"
replace EA_NAME="021-01" if EA_NAME=="" & RK_NAME=="Uncovered by Field Work" & W_NAME=="Metema"
replace EA_NAME="015-03" if EA_NAME=="013-03" & RK_NAME=="May Negad"
replace EA_NAME="024-01" if EA_NAME=="004-02" & RK_NAME=="Lafito Yemenigist Den"
replace EA_NAME="021-09" if EA_NAME=="020-07" & RK_NAME=="Miniji Yibiza"
replace T_CODE=1 if W_NAME=="Bibugn" & RK_NAME=="Weyin wiha Town" 
replace T_CODE=2 if W_NAME=="Bibugn" & RK_NAME=="Digo Tseyon Town" 
replace EA_NAME="033-05" if EA_NAME=="033-01" & RK_NAME=="Werro Awelo" & x_coord < 474000
replace EA_NAME="019-06" if EA_NAME=="017-06" & RK_NAME=="Agemana Embibelo"
replace W_CODE=30616 if W_CODE==30609 & W_NAME=="Senan"
replace W_CODE=30310 if W_CODE==30410 & RK_NAME=="Lalibela Town"
replace EA_NAME="017-02" if EA_NAME=="007-02" & RK_NAME=="Moyona Garaboru" 





# create common unique id
# RK_Code same as kebele_code_merge
# merge RK_Code and EA_Name

eas$kebele_code_merge = paste(eas$W_CODE,sub ,substrRight( eas$EA_NAME,2),sep='')


# Join files 
# limit polys to common ids
length(eas)
eas = eas[eas@data$kebele_code_merge %in% agss$kebele_code_merge,]
agss_eas = join(agss,eas@data, type='left',by = 'kebele_code_merge')

