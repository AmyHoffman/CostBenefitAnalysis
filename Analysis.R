rm(list = ls())

library(tidyverse)
library(ggplot2)
library(cowplot)
source("EC389Functions.R")

years <- 13
months <- years*12
days <- 260
inf <- 0.04
def <- 0.02
avgTHprice <- 23000
avgTHsqft <- 150

DPmtg <- 0.05
DPll <- 0.2
DPrv <- 0.15

Rmtg <-  0.0415
Rll <- 0.055
Rrv <- 0.05

avgcommute <- 6.4 * 260 # for 206 work days in a year
percentincrease <- 0.13
avgprice <- 2.415
avgmpg <- 23.4

gasbtu <- 120476
gaslbperbtu <- 157.2
electricbtu <- 3412
electriclbperbtu <- 215
ngasbtu <- 1037
ngaslbperbtu <- 117
USsqft <- 924.59

movingcost <- 5630
chance <- 0.15*0.75 # 0.1125 

WTP <- 38 *360 # convert per day to year

HPI <-  0.067

#######################################################
# Rental Prices for Apartments and Land
# RentApt.total, Rentland.total, Purchaseland, state
#######################################################
{
  Rent1bed <- read.csv("State_MedianRentalPrice_1Bedroom.csv")
  state <- Rent1bed[-(50),1]
  Rent1bed <- by.region.name(Rent1bed,1)
  Rent1bed <- groupAndAvg.mult(Rent1bed)
  Rent1bed.total <- cbind(Rent1bed[,1], mySummation(Rent1bed[,2]*12, years, inf, def))
  
  Rentstudio <- read.csv("State_MedianRentalPrice_Studio.csv")
  Rentstudio <- by.region.name(Rentstudio, 1)
  Rentstudio <- groupAndAvg.mult(Rentstudio)
  Rentstudio.total <- cbind(Rentstudio[,1], mySummation(Rentstudio[,2]*12, years, inf, def))
  
  RentApt.total <-  cbind(Rent1bed.total, Rentstudio.total[,2])
  RentApt.total <- groupAndAvg.mult(RentApt.total)

  RPratio <-  0.0418
  Purchaseland <- read.csv("LandValue_byState.csv")
  colnames(Purchaseland) <- c("RegionName", "Year", "Variable")
  Purchaseland[,2] <- sapply(strsplit(as.character(Purchaseland[,2]), split='Q'), function(x) (x[1]))
  Purchaseland <- Purchaseland %>% 
    filter(Purchaseland[,2] == 2016) %>% 
    select(-Year) 
  Purchaseland <- by.region.abv(Purchaseland,1) 
  Purchaseland <- groupAndAvg.sing(Purchaseland)
  
  Rentland.total <- cbind(Purchaseland[,1], mySummation.withvalue(Purchaseland[,2]*12, RPratio, years, inf, def))
  # these values seem rather high to me, maybe check to make sure?
}

GSAregions <- as.data.frame(as.factor(Rentland.total[,1]))
colnames(GSAregions) <- c("Regions")
GSAregions.short <- as.data.frame(GSAregions[-(3),])
colnames(GSAregions.short) <- c("Regions")

#######################################################
# Purchase Prices Prices for Apartments
#######################################################
{
  PurchaseApt <- read.csv("Purchase_1bed.csv")
  PurchaseApt <- by.region.name(PurchaseApt,1)
  PurchaseApt <- groupAndAvg.mult(PurchaseApt)
  PurchaseApt.total <- cbind(PurchaseApt[,1], mySummation(PurchaseApt[,2]*12, years, inf, def))
}

#######################################################
# Down Payment
# DPApt, DPland, DPthloan, avgTHprice
#######################################################
{
  # DPmtg <- 0.05
  # DPll <- 0.2
  # DPrv <- 0.15
  # don't forget that down payment of TH if no loan is avgTHprice
  
  DPApt <- cbind(PurchaseApt[,1], DPmtg * PurchaseApt[,2])
  DPland <- cbind(Purchaseland[,1], (DPll * Purchaseland[,2]))
  DPthloan <- DPrv * avgTHprice
  
}

#######################################################
# Loan Payments
# MtgApt, Mtgland, MtgTH
#######################################################
{
  # Rmtg <-  0.0415
  # Rll <- 0.055
  # Rrv <- 0.05
  
  MtgApt <- cbind(PurchaseApt[,1], mySummation(mortgage(PurchaseApt[,2], Rmtg, 30)*12, years, inf, def))
  Mtgland <- cbind(Purchaseland[,1], mySummation(mortgage(Purchaseland[,2], Rll, 30)*12, years, inf, def))
  MtgTH <- mySummation.onevalue(mortgage(avgTHprice, Rrv, 20)*12, years, inf, def)
}

#######################################################
# Property Taxes
# proptaxapt, proptaxland, regtaxTH
#######################################################
{
  proptax <- read.csv("prop_tax.csv")
  proptax <- groupAndAvg.sing(by.region.name(proptax,1))
  
  tempapt <- PurchaseApt[-(3),]
  proptaxapt <- cbind(proptax[,1], mySummation.withvector(tempapt[,2],proptax[,2], years, inf, def))
  templand <- Purchaseland[-(3),]
  proptaxland <- cbind(proptax[,2], mySummation.withvector(templand[,2], proptax[,2], years, inf, def))
  regtaxTH <- mySummation.onevalue(120, years, inf, def)
}

#######################################################
# Total Utilities
# totalUtilapt, totalUtilTH
######################################################
# check unit conversions, add sqft reqs
# may have to adjust inf and def values
{
  electric <- read.csv("electricity_state.csv")
  electric <- groupAndAvg.sing(by.region.name(electric,1))
  
  electricprice <- read.csv("electric_price.csv")
  electricprice <- groupAndAvg.sing(by.region.name(electricprice,1))
  electricprice$avg <- electricprice$avg /100
  
  ngas <- read.csv("gas_consumption.csv")
  ngas[,1] <- sapply(strsplit(as.character(ngas[,1]), split=' N'), function(x) (x[1]))
  ngas <- groupAndAvg.sing(by.region.name(ngas, 1))
  
  ngasprice <-  read.csv("gas_price.csv")
  ngasprice[,1] <- sapply(strsplit(as.character(ngasprice[,1]), split=' P'), function(x) (x[1]))
  ngasprice <- groupAndAvg.sing(by.region.name(ngasprice, 1))
  ngasprice$avg <- ngasprice$avg /1000
  
  totalUtilapt <- cbind(electric[,1], mySummation.util(electric[,2]*12, electricprice[,2], ngas[,2]*12, ngasprice[,2], years, inf, def))
  totalUtilTH <- cbind(electric[,1], mySummation.util(electric[,2]*12, electricprice[,2], ngas[,2]*12, ngasprice[,2], years, inf, def))
}

#######################################################
# Commute
# commuteapt, commuteTH
# https://www.sciencedirect.com/science/article/pii/S0264275115300226#fn0050
#######################################################
{
  # avgcommute <- 6.4 * 260 # for 206 work days in a year
  # percentincrease <- 0.13
  # avgprice <- 2.415
  # avgmpg <- 23.4
  
  commuteapt <- (avgcommute/avgmpg)*mySummation.onevalue(avgprice, years, inf, def)
  commuteTH <- ((avgcommute+(percentincrease*avgcommute))/avgmpg)*mySummation.onevalue(avgprice, years, inf, def)
}

#######################################################
# Avoided Environmental
# CO2avoidedcost, Sqft
# https://www.eia.gov/consumption/residential/data/2015/#squarefootage
#######################################################
{
  # gasbtu <- 120476
  # gaslbperbtu <- 157.2
  # electricbtu <- 3412
  # electriclbperbtu <- 215
  # ngasbtu <- 1037
  # ngaslbperbtu <- 117
  # USsqft <- 924.59
  #reference avgmpg if necessary
  
  # Sqft <- read.csv("aptsqft.csv")
  # Sqft <- divtoGSA(Sqft, as.data.frame(state))
  # colnames(Sqft) <- c("RegionName", "Variable")
  # Sqft <- by.region.name(Sqft,1)
  # Sqft$Variable <- as.numeric(as.character(Sqft$Variable))
  # Sqft <- groupAndAvg.sing(Sqft)
  # 
  # CO2emitapt <- years*((avgcommute/avgmpg) * gaslbperbtu * gasbtu) + ((Sqft[,2]/USsqft) * ((electric[-(3),2] * electriclbperbtu * electricbtu)+ (ngas[-(3),2] * ngaslbperbtu * ngasbtu)))
  # CO2emitTH <- years*((avgcommute/avgmpg) * gaslbperbtu * gasbtu) + ((avgTHsqft/USsqft) * ((electric[-(3),2] * electriclbperbtu * electricbtu)+ (ngas[-(3),2] * ngaslbperbtu * ngasbtu)))
  # 
  # redDeath <- 0.0012
  # SVL <- 7400000 # in 2006 dollars
  # SVL <- inflate.deflate.value(SVL, inf, def, 2017-2006)
  # 
  # CO2avoidedcost <-  (CO2emitapt/CO2emitTH) * redDeath * mySummation.onevalue(SVL, years, inf, def) 
  CO2avoidedcost <- as.data.frame(cbind(0, 0 ))
}

#######################################################
# Avoided Moving Costs
# 
# https://www.fmcsa.dot.gov/sites/fmcsa.dot.gov/files/pictures/FMCSA%20PYM%20Infographic%202016.pdf
# https://usa.ipums.org/usa/resources/voliii/pubdocs/2000/censr-12.pdf
#   75% move between 1995-2000, so approx 15% per yr
#######################################################
# do this; requires calculations
{
  # movingcost <- 5630
  # chance <- 0.15*0.75 # 0.1125 
  
  avoidedmoving <- chance * mySummation.onevalue(movingcost, years, inf, def)
}

#######################################################
# Avoided Health Costs
# health
#######################################################
{
  # WTP <- 38 *360 # convert per day to year
  health <-  mySummation.onevalue(WTP, years, inf, def)
}

#######################################################
# Equity
# equityapt, equityland
#######################################################
{
  # HPI <-  0.067
  
  equityapt <- cbind(PurchaseApt[,1], HPI*mySummation(PurchaseApt[,2], years, inf, def))
  equityland <- cbind(Purchaseland[,1], HPI*mySummation(Purchaseland[,2], years, inf, def))
}

# #######################################################
# # Calculate Total Using Orig Values
# # 
########################################################
{
  # RentApt.total, Rentland.total
  # MtgApt, Mtgland, MtgTH
  # DPApt, DPland, DPthloan, avgTHprice
  # proptaxapt, proptaxland, regtaxTH
  # totalUtilapt, totalUtilTH
  # equityapt, equityland
  # commuteapt, commuteTH
  # health

  # OPTION1: RENT APARTMENT
  Opt1.cost <- as.data.frame(cbind(GSAregions.short, (RentApt.total[-(3),2] + totalUtilapt[-(3),2] + commuteapt)))
  Opt1.total <- Opt1.cost[,2]

  p1 <- ggplot(Opt1.cost)+
    geom_bar(aes(x = Regions, y = Opt1.cost[,2]), fill = "red" ,stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Rent Apartment", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  # OPTION2: RENT LAND AND PURCHASE TINY HOUSE
  Opt2.cost <- as.data.frame(cbind(GSAregions.short, (Rentland.total[-(3),2] + totalUtilTH[-(3),2] + avgTHprice + regtaxTH + commuteTH)))
  Opt2.benefit <- as.data.frame(cbind(GSAregions.short, (CO2avoidedcost[,1] + health + avoidedmoving)))
  Opt2.total <- Opt2.cost[,2] - Opt2.benefit[,2]

  p2 <- ggplot()+
    geom_bar(data = Opt2.cost, aes(x = Regions, y = Opt2.cost[,2]), fill = "red" ,stat="identity") +
    geom_bar(data = Opt2.benefit, aes(x = Regions, y = - Opt2.benefit[,2]), fill = "blue" ,stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Rent Land and Purchase Tiny House", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  # OPTION3: RENT LAND AND FINANCE TINY HOUSE
  Opt3.cost <- as.data.frame(cbind(GSAregions.short, (Rentland.total[-(3),2] + totalUtilTH[-(3),2] +DPthloan + MtgTH + regtaxTH + commuteTH)))
  Opt3.benefit <- as.data.frame(cbind(GSAregions.short, (CO2avoidedcost[,1] + health + avoidedmoving)))
  Opt3.total <- Opt3.cost[,2] - Opt3.benefit[,2]

  p3 <- ggplot()+
    geom_bar(data = Opt3.cost, aes(x = Regions, y = Opt3.cost[,2]), fill = "red" ,stat="identity") +
    geom_bar(data = Opt3.benefit, aes(x = Regions, y = - Opt3.benefit[,2]), fill = "blue" ,stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Rent Land and Finance Tiny House", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  # OPTION4: FINANCE APARTMENT
  Opt4.cost <- as.data.frame(cbind(GSAregions.short, (MtgApt[-(3),2] + DPApt[-(3),2] + proptaxapt[,2] + totalUtilapt[-(3),2] + commuteapt)))
  Opt4.benefit <- as.data.frame(cbind(GSAregions.short, (equityapt[-(3),2])))
  Opt4.total <- Opt4.cost[,2] - Opt4.benefit[,2]

  p4 <- ggplot()+
    geom_bar(data = Opt4.cost, aes(x = Regions, y = Opt4.cost[,2]), fill = "red" ,stat="identity") +
    geom_bar(data = Opt4.benefit, aes(x = Regions, y = - Opt4.benefit[,2]), fill = "blue" ,stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Finance Apartment", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  # OPTION5: FINANCE LAND AND PURCHASE TINY HOUSE
  Opt5.cost <- as.data.frame(cbind(GSAregions.short, (Mtgland[-(3),2] + DPland[-(3),2] + proptaxland[,2] + totalUtilTH[-(3),2] + avgTHprice + regtaxTH + commuteTH)))
  Opt5.benefit <- as.data.frame(cbind(GSAregions.short, (CO2avoidedcost[,1] + equityland[-(3),2] + health + avoidedmoving )))
  Opt5.total <- Opt5.cost[,2] - Opt5.benefit[,2]

  p5 <- ggplot()+
    geom_bar(data = Opt5.cost, aes(x = Regions, y = Opt5.cost[,2]), fill = "red" ,stat="identity") +
    geom_bar(data = Opt5.benefit, aes(x = Regions, y = - Opt5.benefit[,2]), fill = "Blue" ,stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Finance Land and Purchase Tiny House", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  # OPTION6: FINCANCE LAND AND FINANCE TINY HOUSE
  Opt6.cost <- as.data.frame(cbind(GSAregions.short, (Mtgland[-(3),2] + DPland[-(3),2] + proptaxland[,2] + totalUtilTH[-(3),2] +DPthloan + MtgTH + regtaxTH + commuteTH)))
  Opt6.benefit <- as.data.frame(cbind(GSAregions.short, (CO2avoidedcost[,1] + equityland[-(3),2] + health + avoidedmoving )))
  Opt6.total <- Opt6.cost[,2] - Opt6.benefit[,2]

  p6 <- ggplot()+
    geom_bar(data = Opt6.cost, aes(x = Regions, y = Opt6.cost[,2]), fill = "red", stat="identity") +
    geom_bar(data = Opt6.benefit, aes(x = Regions, y = - Opt6.benefit[,2]), fill = "blue", stat="identity") +
    scale_y_continuous(limits = c(-600000, 5000000)) +
    labs(title = "Cost/Benefit To Finance Land and Finance Tiny House", x = "Regions", y = "2017 USD") +
    theme(legend.position="none")

  plot_grid(p1,p2,p3,p4,p5,p6, nrow = 3, ncol = 2)
  total.cost <- as.data.frame(cbind(Opt1.cost[,2], Opt2.cost[,2], Opt3.cost[,2], Opt4.cost[,2], Opt5.cost[,2], Opt6.cost[,2])) 
  write.csv(total.cost, "cost.csv")
  total.benefit  <- as.data.frame(cbind(Opt2.benefit[,2], Opt3.benefit[,2], Opt4.benefit[,2], Opt5.benefit[,2], Opt6.benefit[,2]))
  write.csv(total.benefit, "benefit.csv")
  total.diff <- cbind(GSAregions.short, rowMeans(total.cost) - rowMeans(total.benefit))
  total <- cbind(GSAregions.short, Opt1.cost[,2], Opt2.benefit[,2]/Opt2.cost[,2], Opt3.benefit[,2]/Opt3.cost[,2], Opt4.benefit[,2]/Opt4.cost[,2], Opt5.benefit[,2]/Opt5.cost[,2], Opt6.benefit[,2]/Opt6.cost[,2])
  write.csv(total, "ratios.csv")
  min(total[,2:7])
}

#------------------------------------------------------------------------------------------------
# Graphical Breakdown of costs and benefits
#------------------------------------------------------------------------------------------------
# COSTS par(mfrow=c(1,2))
# Rent Apartment
#---------------------------------------------------------------------------------------------------------------
Opt1.cost.slices <- c(mean(RentApt.total$avg[-(3)]) , mean(totalUtilapt[-(3),2]),  commuteapt)
Opt1.cost.lbls <- paste(paste(c("Rent", "Utilities", "Commute"), 
                              round(Opt1.cost.slices/sum(Opt1.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt1.cost.slices, labels = NA, col=rainbow(length(Opt1.cost.lbls)), main="Option 1 Cost Distribution") 
legend("topleft", legend = Opt1.cost.lbls, cex = 0.8, fill = rainbow(length(Opt1.cost.lbls)))

# Rent land purchase TH
#---------------------------------------------------------------------------------------------------------------
Opt2.cost.slices <- c(mean(Rentland.total$avg[-(3)]) , mean(totalUtilTH[-(3),2]), avgTHprice, regtaxTH, commuteTH)
Opt2.cost.lbls <- paste(paste(c("LandRent", "Utilities", "TinyHouseCost", "RegistrationTax", "Commute"), 
                              round(Opt2.cost.slices/sum(Opt2.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt2.cost.slices, labels = NA, col=rainbow(length(Opt2.cost.lbls)), main="Option 2 Cost Distribution") + 
  legend("topleft", legend = Opt2.cost.lbls, cex = 0.8, fill = rainbow(length(Opt2.cost.lbls)))

# Rent land finance TH
#---------------------------------------------------------------------------------------------------------------
Opt3.cost.slices <- c(mean(Rentland.total$avg[-(3)]), mean(totalUtilTH[-(3),2]), DPthloan, MtgTH, regtaxTH, commuteTH)
Opt3.cost.lbls <- paste(paste(c("LandRent", "Utilities", "TinyHouseDownPayment", "TinyHomeMortgage", "RegistrationTax", "Commute"), 
                              round(Opt3.cost.slices/sum(Opt3.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt3.cost.slices, labels = NA, col=rainbow(length(Opt3.cost.lbls)), main="Option 3 Cost Distribution") + 
  legend("topleft", legend = Opt3.cost.lbls, cex = 0.8, fill = rainbow(length(Opt2.cost.lbls)))

# buy apartment
#---------------------------------------------------------------------------------------------------------------
Opt4.cost.slices <- c(mean(MtgApt[-(3),2]), mean(DPApt[-(3),2]), mean(proptaxapt[,2]), mean(totalUtilapt[-(3),2]) + commuteapt)
Opt4.cost.lbls <- paste(paste(c("ApartmentMortgage", "ApartmentDownPayment", "PropertyTax", "Utilities", "Commute"), 
                              round(Opt4.cost.slices/sum(Opt4.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt4.cost.slices, labels = NA, col=rainbow(length(Opt4.cost.lbls)), main="Option 4 Cost Distribution") +
  legend("topleft", legend = Opt4.cost.lbls, cex = 0.8, fill = rainbow(length(Opt4.cost.lbls)))


# buy land buy TH
#---------------------------------------------------------------------------------------------------------------
Opt5.cost.slices <- c(mean(Mtgland[-(3),2]), mean(DPland[-(3),2]), mean(proptaxland[,2]), mean(totalUtilTH[-(3),2]), avgTHprice, regtaxTH, commuteTH)
Opt5.cost.lbls <- paste(paste(c("LandMortage", "LandDownPayment", "LandProperyTax", "Utilities", "TinyHouseCost", "RegistrationTax", "Commute"), 
                              round(Opt5.cost.slices/sum(Opt5.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt5.cost.slices, labels = NA, col=rainbow(length(Opt5.cost.lbls)), main="Option 5 Cost Distribution") + 
  legend("topleft", legend = Opt5.cost.lbls, cex = 0.8, fill = rainbow(length(Opt5.cost.lbls)))

# buy land finance TH
#---------------------------------------------------------------------------------------------------------------
Opt6.cost.slices <- c(mean(Mtgland[-(3),2]), mean(DPland[-(3),2]), mean(proptaxland[,2]), mean(totalUtilTH[-(3),2]), DPthloan, MtgTH, regtaxTH, commuteTH)
Opt6.cost.lbls <- paste(paste(c("LandMortage", "LandDownPayment", "LandProperyTax", "Utilities", "TinyHomeDownPayment", "TinyHouseMortgage", "RegistrationTax", "Commute"), 
                              round(Opt6.cost.slices/sum(Opt6.cost.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt6.cost.slices, labels = NA, col=rainbow(length(Opt6.cost.lbls)), main="Option 6 Cost Distribution") + 
  legend("topleft", legend = Opt6.cost.lbls, cex = 0.8, fill = rainbow(length(Opt6.cost.lbls)))


# Benefits
# Rent land buy tiny home and Rent land finance TH
#---------------------------------------------------------------------------------------------------------------
Opt2.ben.slices <- c(health, avoidedmoving)
Opt2.ben.lbls <- paste(paste(c("ImprovedHealth", "AvoidedMovingCosts"), 
                             round(Opt2.ben.slices/sum(Opt2.ben.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt2.ben.slices, labels = NA, col=rainbow(length(Opt2.ben.lbls)), main="Option 2 and 3 Benefit Distribution") + 
  legend("topleft", legend = Opt2.ben.lbls, cex = 0.8, fill = rainbow(length(Opt2.ben.lbls)))

# buy apartment
#---------------------------------------------------------------------------------------------------------------
Opt4.ben.slices <- c(mean(equityapt[-(3),2]))
Opt4.ben.lbls <- paste(paste(c("Equity"), 
                             round(Opt4.ben.slices/sum(Opt4.ben.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt4.ben.slices, labels = NA, col=rainbow(length(Opt4.ben.lbls)), main="Option 4 Benefit Distribution") + 
  legend("topleft", legend = Opt4.ben.lbls, cex = 0.8, fill = rainbow(length(Opt4.ben.lbls)))

# Buy land buy TH and Buy land finance TH
#---------------------------------------------------------------------------------------------------------------
Opt5.ben.slices <- c(mean(equityland[-(3),2]), health, avoidedmoving)
Opt5.ben.lbls <- paste(paste(c("LandEquity", "ImprovedHealth", "AvoidedMovingCosts"), 
                             round(Opt5.ben.slices/sum(Opt5.ben.slices)*100)), "%", sep = "") # add percents to labels
pie(Opt5.ben.slices, labels = NA, col=rainbow(length(Opt5.ben.lbls)), main="Option 5 and 6 Benefit Distribution") + 
  legend("topleft", legend = Opt5.ben.lbls, cex = 0.8, fill = rainbow(length(Opt5.ben.lbls)))



# ---------------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS
# ---------------------------------------------------------------------------------------

orig.variables <- as.vector(c(0.07, 0.04, 23000, 150, 
                         0.05, .2, 0.15,
                         0.0415, 0.055, 0.05, 
                         6.4, 0.13, 23.4,
                         0.0012, 
                         5630, 0.1125,
                         38))
diff.cost <- GSAregions.short
diff.benefit <- GSAregions.short
diff.total <- GSAregions.short

for(j in c(0.2, -0.2)){

  variables <- orig.variables

for(i in 1:length(variables)){

  variables[i] <- variables[i] + j * variables[i]
  # Variables instantiation
  {  
    years <- 13
    months <- years*12
    days <- 260
    inf <- variables[1]
    def <- variables[2]
    avgTHprice <- variables[3]
    avgTHsqft <- variables[4]
    
    DPmtg <- variables[5]
    DPll <- variables[6]
    DPrv <- variables[7]
    
    Rmtg <-  variables[8]
    Rll <- variables[9]
    Rrv <- variables[10]
    
    avgcommute <- variables[11] * 260 # for 206 work days in a year
    percentincrease <- variables[12]
    avgprice <- 2.415
    avgmpg <- variables[13]
    
    gasbtu <- 120476
    gaslbperbtu <- 157.2
    electricbtu <- 3412
    electriclbperbtu <- 215
    ngasbtu <- 1037
    ngaslbperbtu <- 117
    USsqft <- 924.59
    
    redDeath <- variables[14]
    SVL <- 7400000 # in 2006 dollars
    
    movingcost <- variables[15]
    chance <- variables[16] # 0.1125 (0.15*0.75)
    
    WTP <- variables[17] *360 # convert per day to year
    
    HPI <-  0.067
  }
  
  #######################################################
  # Rental Prices for Apartments and Land
  # RentApt.total, Rentland.total, Purchaseland, state
  #######################################################
  {
    Rent1bed <- read.csv("State_MedianRentalPrice_1Bedroom.csv")
    state <- Rent1bed[-(50),1]
    Rent1bed <- by.region.name(Rent1bed,1)
    Rent1bed <- groupAndAvg.mult(Rent1bed)
    Rent1bed.total <- cbind(Rent1bed[,1], mySummation(Rent1bed[,2]*12, years, inf, def))
    
    Rentstudio <- read.csv("State_MedianRentalPrice_Studio.csv")
    Rentstudio <- by.region.name(Rentstudio, 1)
    Rentstudio <- groupAndAvg.mult(Rentstudio)
    Rentstudio.total <- cbind(Rentstudio[,1], mySummation(Rentstudio[,2]*12, years, inf, def))
    
    RentApt.total <-  cbind(Rent1bed.total, Rentstudio.total[,2])
    RentApt.total <- groupAndAvg.mult(RentApt.total)
    
    RPratio <-  0.0418
    Purchaseland <- read.csv("LandValue_byState.csv")
    colnames(Purchaseland) <- c("RegionName", "Year", "Variable")
    Purchaseland[,2] <- sapply(strsplit(as.character(Purchaseland[,2]), split='Q'), function(x) (x[1]))
    Purchaseland <- Purchaseland %>% 
      filter(Purchaseland[,2] == 2016) %>% 
      select(-Year) 
    Purchaseland <- by.region.abv(Purchaseland,1) 
    Purchaseland <- groupAndAvg.sing(Purchaseland)
    
    Rentland.total <- cbind(Purchaseland[,1], mySummation.withvalue(Purchaseland[,2]*12, RPratio, years, inf, def))
    # these values seem rather high to me, maybe check to make sure?
  }
  
  # GSAregions <- as.data.frame(as.factor(Rentland.total[,1]))
  # colnames(GSAregions) <- c("Regions")
  # GSAregions.short <- as.data.frame(GSAregions[-(3),])
  # colnames(GSAregions.short) <- c("Regions")
  
  #######################################################
  # Purchase Prices Prices for Apartments
  #######################################################
  {
    PurchaseApt <- read.csv("Purchase_1bed.csv")
    PurchaseApt <- by.region.name(PurchaseApt,1)
    PurchaseApt <- groupAndAvg.mult(PurchaseApt)
    PurchaseApt.total <- cbind(PurchaseApt[,1], mySummation(PurchaseApt[,2]*12, years, inf, def))
  }
  
  #######################################################
  # Down Payment
  # DPApt, DPland, DPthloan, avgTHprice
  #######################################################
  {
    # DPmtg <- 0.05
    # DPll <- 0.2
    # DPrv <- 0.15
    # don't forget that down payment of TH if no loan is avgTHprice
    
    DPApt <- cbind(PurchaseApt[,1], DPmtg * PurchaseApt[,2])
    DPland <- cbind(Purchaseland[,1], (DPll * Purchaseland[,2]))
    DPthloan <- DPrv * avgTHprice
    
  }
  
  #######################################################
  # Loan Payments
  # MtgApt, Mtgland, MtgTH
  #######################################################
  {
    # Rmtg <-  0.0415
    # Rll <- 0.055
    # Rrv <- 0.05
    
    MtgApt <- cbind(PurchaseApt[,1], mySummation(mortgage(PurchaseApt[,2], Rmtg, 30)*12, years, inf, def))
    Mtgland <- cbind(Purchaseland[,1], mySummation(mortgage(Purchaseland[,2], Rll, 30)*12, years, inf, def))
    MtgTH <- mySummation.onevalue(mortgage(avgTHprice, Rrv, 20)*12, years, inf, def)
  }
  
  #######################################################
  # Property Taxes
  # proptaxapt, proptaxland, regtaxTH
  #######################################################
  {
    proptax <- read.csv("prop_tax.csv")
    proptax <- groupAndAvg.sing(by.region.name(proptax,1))
    
    tempapt <- PurchaseApt[-(3),]
    proptaxapt <- cbind(proptax[,1], mySummation.withvector(tempapt[,2],proptax[,2], years, inf, def))
    templand <- Purchaseland[-(3),]
    proptaxland <- cbind(proptax[,2], mySummation.withvector(templand[,2], proptax[,2], years, inf, def))
    regtaxTH <- mySummation.onevalue(120, years, inf, def)
  }
  
  #######################################################
  # Total Utilities
  # totalUtilapt, totalUtilTH
  ######################################################
  # check unit conversions, add sqft reqs
  # may have to adjust inf and def values
  {
    electric <- read.csv("electricity_state.csv")
    electric <- groupAndAvg.sing(by.region.name(electric,1))
    
    electricprice <- read.csv("electric_price.csv")
    electricprice <- groupAndAvg.sing(by.region.name(electricprice,1))
    electricprice$avg <- electricprice$avg /100
    
    ngas <- read.csv("gas_consumption.csv")
    ngas[,1] <- sapply(strsplit(as.character(ngas[,1]), split=' N'), function(x) (x[1]))
    ngas <- groupAndAvg.sing(by.region.name(ngas, 1))
    
    ngasprice <-  read.csv("gas_price.csv")
    ngasprice[,1] <- sapply(strsplit(as.character(ngasprice[,1]), split=' P'), function(x) (x[1]))
    ngasprice <- groupAndAvg.sing(by.region.name(ngasprice, 1))
    ngasprice$avg <- ngasprice$avg /1000
    
    totalUtilapt <- cbind(electric[,1], mySummation.util(electric[,2]*12, electricprice[,2], ngas[,2]*12, ngasprice[,2], years, inf, def))
    totalUtilTH <- cbind(electric[,1], mySummation.util(electric[,2]*12, electricprice[,2], ngas[,2]*12, ngasprice[,2], years, inf, def))
  }
  
  #######################################################
  # Commute
  # commuteapt, commuteTH
  # https://www.sciencedirect.com/science/article/pii/S0264275115300226#fn0050
  #######################################################
  {
    # avgcommute <- 6.4 * 260 # for 206 work days in a year
    # percentincrease <- 0.13
    # avgprice <- 2.415
    # avgmpg <- 23.4
    
    commuteapt <- (avgcommute/avgmpg)*mySummation.onevalue(avgprice, years, inf, def)
    commuteTH <- ((avgcommute+(percentincrease*avgcommute))/avgmpg)*mySummation.onevalue(avgprice, years, inf, def)
  }
  
  #######################################################
  # Avoided Environmental
  # CO2avoidedcost, Sqft
  # https://www.eia.gov/consumption/residential/data/2015/#squarefootage
  #######################################################
  {
    # gasbtu <- 120476
    # gaslbperbtu <- 157.2
    # electricbtu <- 3412
    # electriclbperbtu <- 215
    # ngasbtu <- 1037
    # ngaslbperbtu <- 117
    # USsqft <- 924.59
    #reference avgmpg if necessary
    
    # Sqft <- read.csv("aptsqft.csv")
    # Sqft <- divtoGSA(Sqft, as.data.frame(state))
    # colnames(Sqft) <- c("RegionName", "Variable")
    # Sqft <- by.region.name(Sqft,1)
    # Sqft$Variable <- as.numeric(as.character(Sqft$Variable))
    # Sqft <- groupAndAvg.sing(Sqft)
    # 
    # CO2emitapt <- years*((avgcommute/avgmpg) * gaslbperbtu * gasbtu) + ((Sqft[,2]/USsqft) * ((electric[-(3),2] * electriclbperbtu * electricbtu)+ (ngas[-(3),2] * ngaslbperbtu * ngasbtu)))
    # CO2emitTH <- years*((avgcommute/avgmpg) * gaslbperbtu * gasbtu) + ((avgTHsqft/USsqft) * ((electric[-(3),2] * electriclbperbtu * electricbtu)+ (ngas[-(3),2] * ngaslbperbtu * ngasbtu)))
    # 
    # # redDeath <- 0.0012
    # # SVL <- 7400000 # in 2006 dollars
    # SVL <- inflate.deflate.value(SVL, inf, def, 2017-2006)
    # 
    # CO2avoidedcost <-  (CO2emitapt/CO2emitTH) * redDeath * mySummation.onevalue(SVL, years, inf, def) 
    CO2avoidedcost <- as.data.frame(cbind(0, 0 ))
  }
  
  #######################################################
  # Avoided Moving Costs
  # 
  # https://www.fmcsa.dot.gov/sites/fmcsa.dot.gov/files/pictures/FMCSA%20PYM%20Infographic%202016.pdf
  # https://usa.ipums.org/usa/resources/voliii/pubdocs/2000/censr-12.pdf
  #   75% move between 1995-2000, so approx 15% per yr
  #######################################################
  # do this; requires calculations
  {
    # movingcost <- 5630
    # chance <- 0.15*0.75 # 0.1125 
    
    avoidedmoving <- chance * mySummation.onevalue(movingcost, years, inf, def)
  }
  
  #######################################################
  # Avoided Health Costs
  # health
  #######################################################
  {
    # WTP <- 38 *360 # convert per day to year
    health <-  mySummation.onevalue(WTP, years, inf, def)
  }
  
  #######################################################
  # Equity
  # equityapt, equityland
  #######################################################
  {
    # HPI <-  0.067
    
    equityapt <- cbind(PurchaseApt[,1], HPI*mySummation(PurchaseApt[,2], years, inf, def))
    equityland <- cbind(Purchaseland[,1], HPI*mySummation(Purchaseland[,2], years, inf, def))
  }
  
  
  #######################################################
  # Calculate Total Using Mod Values
  # 
  #######################################################
  {
    
    # OPTION1: RENT APARTMENT
    Opt1.costmod <- cbind(GSAregions.short,(Opt1.cost[,2] - as.data.frame((RentApt.total[-(3),2] + totalUtilapt[-(3),2] + commuteapt))) / Opt1.cost[,2])

    # OPTION2: RENT LAND AND PURCHASE TINY HOUSE
    Opt2.costmod <- cbind(GSAregions.short, (Opt2.cost[,2] - as.data.frame((Rentland.total[-(3),2] + totalUtilTH[-(3),2] + avgTHprice + regtaxTH + commuteTH))) / Opt2.cost[,2])
    Opt2.benefitmod <- cbind(GSAregions.short, (Opt2.benefit[,2] - as.data.frame((CO2avoidedcost + health + avoidedmoving))) / Opt2.benefit[,2])

    # OPTION3: RENT LAND AND FINANCE TINY HOUSE
    Opt3.costmod <- cbind(GSAregions.short, (Opt3.cost[,2] - as.data.frame((Rentland.total[-(3),2] + totalUtilTH[-(3),2] +DPthloan + MtgTH + regtaxTH + commuteTH))) / Opt3.cost[,2])
    Opt3.benefitmod <- cbind(GSAregions.short, (Opt3.benefit[,2] - as.data.frame((CO2avoidedcost + health + avoidedmoving))) / Opt3.benefit[,2] )

    # OPTION4: FINANCE APARTMENT
    Opt4.costmod <- cbind(GSAregions.short, (Opt4.cost[,2] - as.data.frame((MtgApt[-(3),2] + DPApt[-(3),2] + proptaxapt[,2] + totalUtilapt[-(3),2] + commuteapt))) / Opt4.cost[,2])
    Opt4.benefitmod <- cbind(GSAregions.short, (Opt4.benefit[,2] - as.data.frame((equityapt[-(3),2]))) / Opt4.benefit[,2])

    # OPTION5: FINANCE LAND AND PURCHASE TINY HOUSE
    Opt5.costmod <- cbind(GSAregions.short, (Opt5.cost[,2] - as.data.frame((Mtgland[-(3),2] + DPland[-(3),2] + proptaxland[,2] + totalUtilTH[-(3),2] + avgTHprice + regtaxTH + commuteTH))) / Opt5.cost[,2])
    Opt5.benefitmod <- cbind(GSAregions.short,(Opt5.benefit[,2] - as.data.frame((CO2avoidedcost + equityland[-(3),2] + health + avoidedmoving))) / Opt5.benefit[,2])

    # OPTION6: FINCANCE LAND AND FINANCE TINY HOUSE
    Opt6.costmod <- cbind(GSAregions.short, (Opt6.cost[,2] - as.data.frame((Mtgland[-(3),2] + DPland[-(3),2] + proptaxland[,2] + totalUtilTH[-(3),2] +DPthloan + MtgTH + regtaxTH + commuteTH))) / Opt6.cost[,2])
    Opt6.benefitmod <- cbind(GSAregions.short, (Opt6.benefit[,2] - as.data.frame((CO2avoidedcost + equityland[-(3),2] + health + avoidedmoving))) / Opt6.benefit[,2])

    total.costmod <- as.data.frame(cbind(Opt1.costmod[,2], Opt2.costmod[,2], Opt3.costmod[,2], Opt4.costmod[,2], Opt5.costmod[,2], Opt6.costmod[,2])) 
    total.costmod <- cbind(GSAregions.short, rowMeans(total.costmod))
    total.benefitmod  <- as.data.frame(cbind(Opt2.benefitmod[,2], Opt3.benefitmod[,2], Opt4.benefitmod[,2], Opt5.benefitmod[,2], Opt6.benefitmod[,2]))
    total.benefitmod <- cbind(GSAregions.short, rowMeans(total.benefitmod))
    
    diff.cost <- cbind(diff.cost, total.costmod[,2])
    diff.benefit <- cbind(diff.benefit, total.benefitmod[,2])
    diff.total <- cbind(diff.total, diff.cost[,ncol(diff.cost)] + diff.benefit[,ncol(diff.cost)])
  }
  
} } # 3nd of for loops

diff.total <- diff.total[,2:35]
temp <- cbind(GSAregions.short, diff.total[,1:17])
colnames(temp) <- c("Region", "Inflation", "Discount", "TH Price", "TH Sqft", "Mortgage Downpayment", "Land Downpayment", "RV Downpayment",
                    "Mortgage Loan Rate", "Land Loan Rate", "RV Loan Rate", "Commute Distance", "Communte Variation", "Avg Car Mpg",
                    "Reduction In Death Rate", "Willingness To Pay", "Moving Cost", "Probability of Moving")
diff.totaltemp <- cbind(GSAregions.short, diff.total[,18:34])
colnames(diff.totaltemp) <- c("Region", "Inflation", "Discount", "TH Price", "TH Sqft", "Mortgage Downpayment", "Land Downpayment", "RV Downpayment",
                    "Mortgage Loan Rate", "Land Loan Rate", "RV Loan Rate", "Commute Distance", "Communte Variation", "Avg Car Mpg",
                    "Reduction In Death Rate", "Willingness To Pay", "Moving Cost", "Probability of Moving")
diff.total <- as.data.frame(rbind(temp, diff.totaltemp)) 
diff.min <- diff.total %>%  group_by(Region) %>% summarize_all(min)
diff.min <- colMeans(diff.min[,2:18])
diff.max <- diff.total %>%  group_by(Region) %>% summarize_all(max) 
diff.max <- colMeans(diff.max[,2:18])

tempnames <- c("Inflation", "Discount", "TH Price", "TH Sqft", "Mortgage Downpayment", "Land Downpayment", "RV Downpayment",
               "Mortgage Loan Rate", "Land Loan Rate", "RV Loan Rate", "Commute Distance", "Communte Variation", "Avg Car Mpg",
               "Reduction In Death Rate", "Willingness To Pay", "Moving Cost", "Probability of Moving")
diff.total <- as.data.frame(cbind(tempnames, cbind(diff.min, diff.max)))
diff.total$diff.min <- as.numeric(as.character(diff.total$diff.min))
diff.total$diff.max <- as.numeric(as.character(diff.total$diff.max))
diff.total <- diff.total %>% arrange((abs(diff.total$diff.min) + abs(diff.total$diff.max)))
rownames(diff.total) <- tempnames
diff.min <- diff.total$diff.min
diff.max <- diff.total$diff.max

op <- par(mar = c(4,10,4,10) + 0.1)
barplot(diff.min, horiz = T, las=2, xlim = c(-0.8,0.4), beside=F, col=c('blue'), names.arg=rownames(diff.total))
barplot(diff.max, horiz = T, las=2, xlim = c(-0.8,0.5), beside=T, col=c('blue'), names.arg=rownames(diff.total), add = TRUE)
title(main="Sensitivities")
par(op)


