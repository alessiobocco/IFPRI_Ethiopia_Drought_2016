
# This file uses RStata to clean up the polygon shapefile holding EA 
# this allows us to create a unique EA code
# NOT RUN ON SERVER SINCE THEY DONT HAVE STATA

library(RStata)
library(readstata13)



*Dec. 8, 2017, 75,590 obs.
*NOTE: Yemenigist Den is an RK_NAME in three different woredas in two zones
*Trying to clear up the EA codes, one value for every EA
use "D:\Users\jwarner.ILRI\Desktop\EA\Ethio_EA_2016data_13.dta", clear
format RK_CODE %12.0g
*drop some areas that do not have kebeles, somali region and addis
*10,159 obs. dropped--remaining 65,431
drop if R_CODE==14 | R_CODE==5
*Drop this duplicate
*In Burji Special woreda, Tinisha Keyate kebele one obs. with no coordinates
drop if RK_NAME=="Tinisha Keyate" & x_coord==.
*There are 7 observations without x and y coordinates
*Some corrections that became apperent as I worked on this
replace EA_NAME="023-05" if EA_NAME=="0123-0" & RK_NAME=="Weserbi Guto"
replace EA_NAME="032-03" if EA_NAME=="032--0" & RK_NAME=="Huluko Birbisa"
replace EA_NAME="014-04" if EA_NAME=="0014-0" & RK_NAME=="Balaref"
replace EA_NAME="025-05" if EA_NAME=="0025-0" & RK_NAME=="Layiyeduge"
replace EA_NAME="020-01" if EA_NAME=="020-o1" & RK_NAME=="Derajaresso"
replace EA_NAME="010-01" if EA_NAME=="01001" & RK_NAME=="Girari Reh"
replace EA_NAME="001-01" if EA_NAME=="00101" & RK_NAME=="Pekedu"
replace EA_NAME="007-01" if EA_NAME=="00701" & RK_NAME=="Yerer"
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



replace EA_NAME="401-02" if EA_NAME=="401" & RK_NAME=="401 Yemenigist Den"

*These are "odd" ones named 401 in a group of consistent labeled
replace EA_NAME="001-14" if EA_NAME=="401" & RK_NAME=="Sitir Dirma"
*replace EA_NAME="T_CODE" if RK_NAME=="Alimu Town"
*replace EA_NAME="T_CODE" if RK_NAME=="Adis Alem Town"
*replace EA_NAME="T_CODE" if RK_NAME=="Felege Selam Town"

*There are 22 observations of EA code that end with 91.  I don't know why.
*The most EA's seem to be 22 with Dubaba in Quara Woreda, North Gondar


gen RK_CODE1=RK_CODE
tostring RK_CODE1, force replace
*If 7 digits then town, if 8 digits then rural kebele
gen RK_CODE_length=length(RK_CODE1)
*Trying to create accurate EA codes using EA_NAME
gen kebele_code_new=substr(EA_NAME,1,3)
*The Town values are a mess--Value as 98 for town 
replace kebele_code_new="98" if RK_CODE_length==7 
*Replacing some with zero EA_NAME, 855 obs. for cities 
replace kebele_code_new="98" if RK_NAME=="Meti Town" | RK_NAME=="Assosa Town"  | RK_NAME=="Alimu Town" | RK_NAME=="Adis Alem Town" | RK_NAME=="Felege Selam Town" 
replace T_CODE=1 if RK_NAME=="Keranio Town" 
replace T_CODE=2 if RK_NAME=="Mota Town" 
replace T_CODE=3 if RK_NAME=="Sedie Town" 

destring kebele_code_new, force replace
*(101 missing values generated)

*"Odd" EAs (110 obs.) (example Fincha Suger Factory) are given 97 code
replace kebele_code_new=97 if  kebele_code_new==. | kebele_code_new==0

*generate EA code
gen EA_code_new=substr(EA_NAME,5,6)
*replace town codes
replace EA_code_new="98" if kebele_code_new==98
*replace "odd" codes
replace EA_code_new="97" if kebele_code_new==97 
*replace "91" codes of CSA (count as odd (Haramaya Univ. etc.), 9 changes made
replace EA_code_new="91" if kebele_code_new==91



destring EA_code_new, replace
*37 missing values, These are AFDEM and MULO Special EAs
replace kebele_code_new=135 if kebele_code_new==35 & EA_CODE=="05010400000135" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=134 if kebele_code_new==34 & EA_CODE=="05010400000134" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=136 if kebele_code_new==36 & EA_CODE=="05010400000136" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=153 if kebele_code_new==53 & EA_CODE=="05010400000153" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=154 if kebele_code_new==54 & EA_CODE=="05010400000154" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=166 if kebele_code_new==66 & EA_CODE=="05010400000166" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=176 if kebele_code_new==76 & EA_CODE=="05010400000176" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=175 if kebele_code_new==75 & EA_CODE=="05010400000175" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=183 if kebele_code_new==83 & EA_CODE=="05010400000183" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=199 if kebele_code_new==99 & EA_CODE=="05010400000199" & W_NAME=="Afdem Special EA" 
replace kebele_code_new=181 if kebele_code_new==81 & EA_CODE=="05010400000181" & W_NAME=="Mulo Special EA" 
replace kebele_code_new=185 if kebele_code_new==85 & EA_CODE=="05010400000185" & W_NAME=="Mulo Special EA" 
*For the EA codes of these special 
replace EA_code_new=1 if EA_code_new==.  & R_NAME=="Special EA"
*(37 real changes made)
*The 401's of EA_CODE replace with 1 instead of missing 
replace EA_code_new=1 if EA_code_new==.
*Fixing towns add the T_CODE for EA_CODE
replace EA_code_new=T_CODE if RK_CODE_length==7 &  T_CODE > 0 | RK_NAME=="Alimu Town" | RK_NAME=="Adis Alem Town" | RK_NAME=="Felege Selam Town" | RK_NAME=="Keranio Town" | RK_NAME=="Mota Town" | RK_NAME=="Sedie Town" 
*(824 changes)


gen double kebele_code_merge= (W_CODE*1000)+ kebele_code_new

format kebele_code_merge %14.0g

distinct kebele_code_merge

*                   |        Observations
*                   |      total   distinct
*-------------------+----------------------
* kebele_code_merge |      65431      15370

gen double EA_code_merge= (kebele_code_merge *100)+ EA_code_new
format EA_code_merge %16.0g
*because one was a duplicate in Afdem Special EA, changed to 90, I didn't get this too well maybe I am tired
replace EA_code_merge=17010409190 if EA_NAME=="91" & W_NAME=="Afdem Special EA" & x_coord > 678000

distinct EA_code_merge


*               |        Observations
*               |      total   distinct
*---------------+----------------------
* EA_code_merge |      65270      64845


duplicates tag EA_code_merge, generate(ea_dup)
tab ea_dup

*        ea_dup |      Freq.     Percent        Cum.
*------------+-----------------------------------
*          0 |     65,099       99.49       99.49
*          1 |        298        0.46       99.95
*          2 |         21        0.03       99.98
*          3 |          8        0.01       99.99
*          4 |          5        0.01      100.00
*------------+-----------------------------------
*      Total |     65,431      100.00
drop if ea_dup==1
rename id _ID
keep if R_CODE==7

*mergepoly using "D:\Users\jwarner.ILRI\Desktop\EA\Ethio_EA_2016coor_13.dta", by(EA_code_merge) coor ("D:\Users\jwarner.ILRI\Desktop\EA\Ethio_EA_SNNPR.dta")
*Mapping a woreda Wensho
*spmap using  "D:\Users\jwarner.ILRI\Desktop\EA\Ethio_EA_2016coor_13.dta" if W_CODE ==70403, id(_ID) label(xcoord(x_coord) ycoord(y_coord) label( EA_NAME) color(red) size(*.6) select(keep if W_CODE==70403) ) polygon(data("D:\Users\jwarner.ILRI\Desktop\EA\Ethio_EA_Wensho_kebele.dta") osize(medthick) ocolor(blue) ) title("Wensho Woreda SNNP")
save "D:\Users\jwarner.ILRI\Desktop\Drought_Study\Sample_Kebeles_2011-2016\EA_Data_Cleaned.dta"



