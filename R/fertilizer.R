

## the R packages neede to run these functions are "plyr" and "tidyr". THe other packages will be automatically
## installed when you download R.

## The Wrapper function is called "FR_recommendation".
## This function requires lat, long, IntendedPlantingDate, country="Nigeria", rootUP, investment as input.
## It calls the other functions at different stages as required.

#load libraries
#library(plyr)
#library(dplyr)
#library(reshape2)
#library(tidyr)

require(plyr)
require(tidyr)


actual_uptake_tool <- function(ds_supply){
  with(ds_supply,
       {
         UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
         UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
         UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
         UN <- min(UNP, UNK, UNW)


         UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
         UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
         UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
         UP <- min(UPN, UPK, UPW)


         UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
         UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
         UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
         UK <- min(UKN, UKP, UKW)


         return(data.frame(UN=UN, UP=UP, UK=UK))
       })
}


water_dependent_nutrient_uptake <- function(S1=NA, WLY=NA, d1=NA, a1=NA, r1=NA) {
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1
  } else if (S1 > r1 + 2*WLY/a1 - WLY/d1) {
    uptakeX_givenWater = WLY / a1
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY/d1)^2 / (WLY / a1 - WLY / d1)
  }

  return(uptakeX_givenWater)
}


#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA) {
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
    uptakeX_givenY = S1
  } else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
    uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
  } else {
    uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  # Nutrient uptake given availability of other nutrient
  return(uptakeX_givenY)
}

max_min_yields_tools <- function(dss){

  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK


  return(data.frame(YNA=YNA, YND=YND, YPA=YPA, YPD=YPD, YKA=YKA, YKD=YKD))

}


final_yield_tools <- function(Uptake_Yield){
  with(Uptake_Yield,
       {
         YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
         YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
         YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
         YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
         YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
         YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)

         # Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
         YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
         YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
         YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
         YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
         YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
         YKPc <- min(c(YKP, YND, YPD, YKD, WLY))

         #Final estimate
         YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))

         return(YEc)
       })
}

#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
yield_nutrients_combined <- function(U1=NA, d1=NA, a1=NA, Y2A=NA, Y2D=NA, Y3D=NA, r1=NA){
  # Determine which nutrient limited yield is lowest.
  YxD = min(Y2D, Y3D)
  # If the uptake of one of the nutrients, and therefore the yield associated with that
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else{
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}




quefts_tools <- function(supply_wly){
  # Actual uptake of nutrients.
  tmp <- actual_uptake_tool(supply_wly)
  supply_wly$UN <- tmp[[1]]
  supply_wly$UP <- tmp[[2]]
  supply_wly$UK <- tmp[[3]]

  # Maximum and minimum yields, depending on maximum accumulation and dilution.
  yields <- max_min_yields_tools(supply_wly)
  supply_wly$YNA <- yields$YNA
  supply_wly$YND <- yields$YND
  supply_wly$YPA <- yields$YPA
  supply_wly$YPD <- yields$YPD
  supply_wly$YKA <- yields$YKA
  supply_wly$YKD <- yields$YKD

  # Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
  supply_wly$FinalYield <- final_yield_tools(supply_wly)

  return(supply_wly)
}



#' using the output of function "NPK_TargetYield_forinput" and a dat frame per long and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param NutrUse_soilNPK Update Descriptiosn here
#' @param NPKdata: needs to be provided
#' @return
#'
#' @author Meklit
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate){
  NutrUse_soilNPK$N_rate <- N_rate
  NutrUse_soilNPK$P_rate <- P_rate
  NutrUse_soilNPK$K_rate <- K_rate

  ## Supply of nutrients to the crop
  NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
  NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
  NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK

  #coord <- paste(lat, long)


  ## Actual Uptake of nutrients: crop param + nutrient supply
  tmp <-   plyr::ddply(NutrUse_soilNPK,plyr::.(lat, long), actual_uptake_tool)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by=c("lat", "long"))

  ## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
  maxminY <-   plyr::ddply(NutrUse_soilNPK,plyr::.(lat, long), max_min_yields_tools)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by=c("lat", "long"))

  ## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations
  Target_Yield <- plyr::ddply(NutrUse_soilNPK,plyr::.(lat, long), quefts_tools)
  TY <- data.frame(lat=Target_Yield$lat, long=Target_Yield$long, TargetYield=Target_Yield$FinalYield)

  return(TY)
}




#' computes target yield in tonnes/ha from a given NPK rate
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @returnType
#' @return target yield in ton/ha dry matter
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec){
  QID$WLY <- QID$water_limited_yield

  crop_param <- cbind(NUE(HI=0.52), data.frame(rN=0, rP=0, rK=0, max_yield=QID$WLY, tolerance=0.01))	## nutrient use efficiency of the crop


  Queft_Input_Data_Var1 <- cbind(QID, crop_param)
  indata <- Queft_Input_Data_Var1[,c("lat","long" ,"WLY","aN", "dN", "aP", "dP","aK","dK", "rN", "rP", "rK", "soilN", "soilP", "soilK","max_yield", "tolerance")]

  N_rate <- rec[1]
  P_rate <- rec[2]
  K_rate <- rec[3]

  TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK=indata, N_rate, P_rate, K_rate)

  return(TargetYield_from_NPK$TargetYield)
}



#'  Optimize the UREA, TSP and MOP needed to maximize the NR. x1, x2, x3 = Urea, MOP and Nafaka kg/ha.
optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest){
  f_price <-fertilizer$price
  TC <- sum(fertRate * f_price)

  x1 <- fertRate[1] ## Kg of Urea
  x2 <- fertRate[2] ## Kg of TSP
  x3 <- fertRate[3]	## Kg of MOP

  N <- x1 * fertilizer$N_cont[1]
  P <- x2 * fertilizer$P_cont[2]
  K <- x3 * fertilizer$K_cont[3]
  rec <- c(N,P,K)

  TotalYield <- QUEFTS1_Pedotransfer(QID, rec)*3/1000
  AdditionalYield <- TotalYield - CY
  PriceYield <- AdditionalYield * rootUP
  NetRev <- PriceYield - TC

  if (!is.na(invest) & TC > invest) {NetRev <- NetRev - (invest-TC)^2} #penalize NR if costs exceed investment cap

  return(NetRev)
}



run_Optim_NG <- function(rootUP, QID, fertilizer, invest, plDate, fert_onePixel, lat, long){

  QID$water_limited_yield <- fert_onePixel$water_limited_yield

  urea <- (fert_onePixel$fert_N)/fertilizer$N_cont[1]
  if(urea < 0) urea <- 0

  TSP <- fert_onePixel$fert_P/fertilizer$P_cont[2]
  if(TSP < 0) TSP <- 0

  Mop <- (fert_onePixel$fert_K)/fertilizer$K_cont[3]
  if(Mop <0) Mop <- 0

  initial <- c(urea, TSP, Mop)
  CY <- (fert_onePixel$CurrentYield)/1000 ## is fresh wt in t/ha
  FR <- optim(par=initial, optim_NR, lower=c(0,0,0), method = "L-BFGS-B", control=list(fnscale=-1), rootUP=rootUP,
              QID=QID, CY=CY, fertilizer=fertilizer, invest=invest)$par
  TC <- round(sum(FR * fertilizer$price), digits=0)							# total cost


  x1 <- round(FR[1], digits=0)
  x2 <- round(FR[2], digits=0)
  x3 <- round(FR[3] , digits=0)

  N <- x1 * fertilizer$N_cont[1]
  P <- x2 * fertilizer$P_cont[2]
  K <- x3 * fertilizer$K_cont[3]
  rec <- c(N, P, K)

  TY <- round((QUEFTS1_Pedotransfer(QID, rec)/1000), digits=3)	# Yield possible at recommended NPK in t/ha fresh wt.
  GR <- (TY - CY) * rootUP                                # Gross revenue given root up for fresh wt is 270
  NR <- round(GR - TC, digits=0)    											# Net Revenue
  return(data.frame(lat=lat, long=long, plDate, urea=x1, TSP=x2, MOP=x3, NR=NR, N=N, P=P, K=K,TC, TY, CurrYield = CY,
                    WLY=QID$water_limited_yield/1000))
}

NUE <- function(HI, CmaxNroots=6.6, CminNroots=2.5, CmaxNtops=17.9, CminNtops=7.9, CmaxProots=1.5, CminProots=0.8, CmaxPtops=2.8, CminPtops=0.9,
                CmaxKroots=11, CminKroots=2.8, CmaxKtops=18.8, CminKtops=3.4 ){
  aN = round(1000 * HI/(HI * CmaxNroots + (1 - HI) * CmaxNtops), digits=0)
  dN = round(1000 * HI/(HI * CminNroots + (1 - HI) * CminNtops), digits=0)

  aP = round(1000 * HI/(HI * CmaxProots + (1 - HI) * CmaxPtops), digits=0)
  dP = round(1000 * HI/(HI * CminProots + (1 - HI) * CminPtops), digits=0)

  aK = round(1000 * HI/(HI * CmaxKroots + (1 - HI) * CmaxKtops), digits=0)
  dK = round(1000 * HI/(HI * CminKroots + (1 - HI) * CminKtops), digits=0)

  return(data.frame(aN=aN, dN=dN,aP=aP,dP=dP,aK=aK,dK=dK))

}


#' after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price
#' @param zone zone
#' @param wdd wdd
#' @param rdd rdd
#' @param fertilizer fertilizer type
#' @author Meklit
#' @export
Rerun_25kgKa_try <- function(rootUP, wdd, rdd, fertilizer, Quefts_Input_Data){

  fertilizers <- fertilizer
  recalc_data <- rdd
  QID <- Quefts_Input_Data
  fert_onePixel <- wdd
  QID$water_limited_yield <- fert_onePixel$water_limited_yield


  CY <- (fert_onePixel$CurrentYield)/1000							#fresh wt yield in ton
  rec_FertAmount <- c(recalc_data$urea, recalc_data$TSP, recalc_data$MOP)
  TC <- sum(rec_FertAmount * fertilizers$price)


  x1 <- rec_FertAmount[1]
  x2 <- rec_FertAmount[2]
  x3 <- rec_FertAmount[3]

  N <- as.numeric(x1 * fertilizers$N_cont[1])
  P <- as.numeric(x2 * fertilizers$P_cont[2])
  K <- as.numeric(x3 * fertilizers$K_cont[3])

  rec <- c(N, P, K)
  TY  <- (QUEFTS1_Pedotransfer(QID,rec))/1000						#fresh wt yield in ton
  if(TY <= CY){
    GR <- 0
    NR <- 0
  }else{
    GR  <- (TY - CY) * rootUP                       						#gross revenue
    NR  <- GR - TC
  }

  return(data.frame(lat=recalc_data$lat, long=recalc_data$long, rateUrea=x1,  rateNafaka=x3, rateMOP=x2,currentY= CY,
                    targetY=TY, netRev=NR,totalCost = TC, N=N, P=P, K=K, plDate=recalc_data$plDate))
}



### see if profit is > 0.18 * otoal cost
NRabove18Cost <- function(ds){
  ds$return <- ds$totalCost * 0.18
  ds$SC <- ifelse(ds$netRev > ds$return, "Keep", "Change")

  fertRecom_profitable <- droplevels(ds[ds$SC=="Keep", ])
  fertRecom_NOTprofitable <- droplevels(ds[ds$SC=="Change", ])

  if(nrow(fertRecom_NOTprofitable)>0){
    fertRecom_NOTprofitable$rateUrea <- 0
    fertRecom_NOTprofitable$rateNafaka  <- 0
    fertRecom_NOTprofitable$rateMOP <- 0
    fertRecom_NOTprofitable$ netRev <- 0
    fertRecom_NOTprofitable$totalCost <- 0
    fertRecom_NOTprofitable$N <- 0
    fertRecom_NOTprofitable$P <- 0
    fertRecom_NOTprofitable$K <- 0
    fertRecom_NOTprofitable$targetY <- fertRecom_NOTprofitable$currentY
  }

  fertRecom <- (rbind(fertRecom_NOTprofitable, fertRecom_profitable))

  fertRecom$currentY <- round(fertRecom$currentY, digits=2)
  fertRecom$targetY <- round(fertRecom$targetY, digits=2)
  fertRecom$netRev <- round(fertRecom$netRev, digits=2)
  fertRecom <- subset(fertRecom, select=-c(return, SC))

  return(fertRecom)
}



#' This function computes the fertilizer estimate
#' @param lat Lattitude
#' @param long Longitude
#' @param countryObj Country code 1 is Nigeria 2 is Tanzania this is because the strings are not taken properly here and conversion os hectic
#' @param IntendedPlantingDate controls not to ask recommendation for dates that are not considerd as planing dates in the zone
#' @param rootUP root rpice for fresh wt. ton/ha it is 270 in TZ and  141 for NG
#' @param investment: how much the farmer is willing to ivest  in USD (currently 200 USD)
#' @param harvestDate: how much the farmer is willing to ivest  in USD (currently 200 USD)
#' @param farmSize: how much the farmer is willing to ivest  in USD (currently 200 USD)
#' @param ureaUse: boolean value False or True
#' @param DAPUse: boolean value False or True
#' @param NPK171717Use: boolean value False or True
#' @param NPK201010Use: boolean value False or True
#' @param NPK251010Use: boolean value False or True
#' @param TSPuse: boolean value False or True
#' @param SSPUse: boolean value False or True
#' @param CANuse: boolean value False or True
#' @param NafakaUse: boolean value False or True
#' @param MOPUse: boolean value False or True
#' @param ureaPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param DAPPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param NPK171717price: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param NPK151515Price: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param NPK201010Price: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param NPK251010Price: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param TSPPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param SSPPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param CANPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param NafakaPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @param MOPPrice: Price of specified fertilizer default is 20 USD but it will have to be converted base on the country
#' @return The estimate array
#' @export
recommendation <-function (lat, long, IntendedPlantingDate, countryObj, rootUP, investment, harvestDate = 300, farmSize=2, ureaUse=TRUE,
                           DAPUse = TRUE, NPK171717Use=FALSE, NPK151515Use=FALSE,NPK201010Use=FALSE, NPK251010Use =FALSE, TSPuse=TRUE,
                           SSPUse=FALSE, CANuse=FALSE, NafakaUse=FALSE, MOPUse=FALSE, ureaPrice=20, DAPPrice =20, NPK171717price=20,
                           NPK151515Price=20,NPK201010Price=20, NPK251010Price=20, TSPPrice=20, SSPPrice=20, CANPrice=20, NafakaPrice=20, MOPPrice=20)
{
  if (countryObj == 1) {
    country <- "Nigeria"
  }
  else if (countryObj == 2) {
    country <- "Tanzania"
  }
  if (country == "Nigeria") {
    fertilizer <- c("urea", "TSP", "MOP")
    N_cont <- c(0.46, 0, 0)
    P_cont <- c(0, 0.2, 0)
    K_cont <- c(0, 0, 0.5)
    Zones <- rep("Nigeria", 3)
    fertilizer <- data.frame(fertilizer, N_cont, P_cont,
                             K_cont)
    fertilizer$price <- c(0.39, 1, 0.5)
  }
  else if (country == "Tanzania") {
    fertilizer <- rep(c("urea", "TSP", "MOP", "DAP", "NPK171717",
                        "MPR_nafaka", "MPR_mazao"), 3)
    N_cont <- rep(c(0.46, 0, 0, 0.18, 0.17, 0.09, 0.1), 3)
    P_cont <- rep(c(0, 0.2, 0, 0.2, 0.07, 0.07, 0.09), 3)
    K_cont <- rep(c(0, 0, 0.43, 0, 0.12, 0.06, 0), 3)
    Zones <- c(rep("Lake_zone", 7), rep("Southern_zone",
                                        7), rep("Eastern_zone", 7))
    fertilizer <- data.frame(fertilizer, N_cont, P_cont,
                             K_cont, Zones)
    fertilizer <- fertilizer[fertilizer$fertilizer %in% c("urea",
                                                          "MPR_nafaka", "MOP"), ]
    fertilizer$price <- c(c(c(70000, 120000, 57000) * 0.00045/50),
                          c(c(50000, 65000, 57000) * 0.00045/50), c(c(40000,
                                                                      90000, 57000) * 0.00045/50))
    fertilizer <- fertilizer[fertilizer$Zones == zone, ]
  }
  WLY_FertRecom <- data.frame(lat = lat, long = long, fert_N = sample(c(seq(60,
                                                                            150, 12)), 1), fert_P = sample(c(seq(30, 80, 7)), 1),
                              fert_K = sample(c(seq(35, 146, 7)), 1), location = paste(lat,
                                                                                       long, sep = "_"), pl_Date = IntendedPlantingDate,
                              zone = country)
  water_limited_yield <- sample(c(seq(12000, 17000, 1000)),
                                1)
  CurrentYield <- sample(c(seq(7000, 12000, 800)), 1)
  WLY_FertRecom$water_limited_yield <- ifelse(water_limited_yield >
                                                CurrentYield, water_limited_yield, CurrentYield)
  WLY_FertRecom$CurrentYield <- ifelse(water_limited_yield >
                                         CurrentYield, CurrentYield, water_limited_yield)
  soilGPS <- data.frame(lat = lat, long = long, soilN = sample(c(seq(28,
                                                                     200, 25)), 1), soilP = sample(c(seq(12, 80, 15)), 1),
                        soilK = sample(c(seq(12, 80, 15)), 1), rec_N = 0.5, rec_P = 0.15,
                        rec_K = 0.5, rel_N = 1)
  soilGPS$rel_P <- soilGPS$soilP/soilGPS$soilN
  soilGPS$rel_K <- soilGPS$soilK/soilGPS$soilN
  soilGPS$latlong <- paste(lat, long, sep = "_")
  fert_optim <- run_Optim_NG(rootUP = rootUP, QID = soilGPS,
                             fertilizer = fertilizer, invest = investment, plDate = IntendedPlantingDate,
                             fert_onePixel = WLY_FertRecom, lat = lat, long = long)
  fert_optim$urea <- ifelse(fert_optim$urea < 25, 0, fert_optim$urea)
  fert_optim$MOP <- ifelse(fert_optim$MOP < 25, 0, fert_optim$MOP)
  fert_optim$TSP <- ifelse(fert_optim$TSP < 25, 0, fert_optim$TSP)
  Reset_fert_South <- Rerun_25kgKa_try(rootUP = rootUP, wdd = WLY_FertRecom,
                                       rdd = fert_optim, fertilizer = fertilizer, Quefts_Input_Data = soilGPS)
  GPS_fertRecom <- NRabove18Cost(ds = Reset_fert_South)
  return(GPS_fertRecom)
}
