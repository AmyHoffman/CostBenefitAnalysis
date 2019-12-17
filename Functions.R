##############################################
# EIA Division to GSA Region
#############################################
divtoGSA <- function(Dataset, statesVector){
  newRegions <- rep(0, nrow(statesVector))
  newRegions <- cbind(statesVector, newRegions)
  
  newRegions[which(newRegions[,1] %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")),2] <- as.character(Dataset[1,2])
  newRegions[which(newRegions[,1] %in% c("New York", "New Jersey", "Pennsylvania")),2] <- as.character(Dataset[2,2])
  newRegions[which(newRegions[,1] %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")),2] <- as.character(Dataset[3,2])
  newRegions[which(newRegions[,1] %in% c("Nebraska", "Kansas", "Missouri", "Iowa", "North Dakota", "South Dakota", "Minnesota")),2] <- as.character(Dataset[4,2])
  newRegions[which(newRegions[,1] %in% c("Delaware", "Maryland", "Virginia", "West Virginia", "Florida", "Georgia", "North Carolina", "South Carolina")),2] <- as.character(Dataset[5,2])
  newRegions[which(newRegions[,1] %in% c("Kentucky", "Mississippi", "Alabama", "Tennessee")),2] <- as.character(Dataset[6,2])
  newRegions[which(newRegions[,1] %in% c("Texas", "Oklahoma", "Arkansas", "Louisiana")),2] <- as.character(Dataset[7,2])
  newRegions[which(newRegions[,1] %in% c("Colorado", "Utah", "Wyoming", "Montana","Arizona", "Nevada","New Mexico", "Idaho")),2] <- as.character(Dataset[8,2])
  newRegions[which(newRegions[,1] %in% c("Washington", "Oregon", "Hawaii", "Alaska", "California")),2] <- as.character(Dataset[2,2])
  
  return(newRegions)
}

##############################################
# Puts into GSA Regions by Name
#############################################
by.region.name <- function(Dataset, cnum){
  Dataset$RegionName <- as.character(Dataset$RegionName)
  Dataset[which(Dataset$RegionName %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")),cnum] <- as.character(1)
  Dataset[which(Dataset$RegionName %in% c("New York", "New Jersey")),cnum] <- as.character(2)
  Dataset[which(Dataset$RegionName %in% c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia", "West Virginia")),cnum] <- as.character(3)
  Dataset[which(Dataset$RegionName %in% c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee")),cnum] <- as.character(4)
  Dataset[which(Dataset$RegionName %in% c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin")),cnum] <- as.character(5)
  Dataset[which(Dataset$RegionName %in% c("Nebraska", "Kansas", "Missouri", "Iowa")),cnum] <- as.character(6)
  Dataset[which(Dataset$RegionName %in% c("New Mexico", "Texas", "Oklahoma", "Arkansas", "Louisiana")),cnum] <- as.character(7)
  Dataset[which(Dataset$RegionName %in% c("Colorado", "Utah", "Wyoming", "Montana", "North Dakota", "South Dakota")),cnum] <- as.character(8)
  Dataset[which(Dataset$RegionName %in% c("Arizona", "Nevada", "California", "Hawaii")),cnum] <- as.character(9)
  Dataset[which(Dataset$RegionName %in% c("Washington", "Oregon", "Idaho", "Alaska")),cnum] <- as.character(10)
  Dataset[which(Dataset$RegionName %in% c("District of Columbia")),cnum] <- as.character(11)
  return(Dataset)
}

##############################################
# Puts into GSA Regions by Abrev
#############################################
by.region.abv <- function(Dataset, cnum){
  Dataset$RegionName <- as.character(Dataset$RegionName)
  Dataset[which(Dataset$RegionName %in% c("ME", "VT", "NH", "MA", "CT", "RI")),cnum] <- as.character(1)
  Dataset[which(Dataset$RegionName %in% c("NY", "NJ")),cnum] <- as.character(2)
  Dataset[which(Dataset$RegionName %in% c("DE", "MD", "PA", "VA", "WV")),cnum] <- as.character(3)
  Dataset[which(Dataset$RegionName %in% c("AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN")),cnum] <- as.character(4)
  Dataset[which(Dataset$RegionName %in% c("IL", "IN", "MI", "MN", "OH", "WI")),cnum] <- as.character(5)
  Dataset[which(Dataset$RegionName %in% c("NE", "KS", "MO", "IA")),cnum] <- as.character(6)
  Dataset[which(Dataset$RegionName %in% c("NM", "TX", "OK", "AR", "LA")),cnum] <- as.character(7)
  Dataset[which(Dataset$RegionName %in% c("CO", "UT", "WY", "MT", "ND", "SD")),cnum] <- as.character(8)
  Dataset[which(Dataset$RegionName %in% c("AZ", "NV", "CA", "HI")),cnum] <- as.character(9)
  Dataset[which(Dataset$RegionName %in% c("WA", "OR", "ID", "AK")),cnum] <- as.character(10)
  Dataset[which(Dataset$RegionName %in% c("DC")),cnum] <- as.character(11)
  return(Dataset)
}

###########################################################
# Groups by GSA Region then averages over rows and columns
  # for multiple columns
###########################################################
groupAndAvg.mult <- function(Dataset){
  avgvec <- rep(0, nrow(Dataset))

  for(i in 1:nrow(Dataset)){
    avgvec[i] <- sum(Dataset[i,(2:ncol(Dataset))], na.rm = TRUE)/sum( !is.na(Dataset[i,(2:ncol(Dataset))]))
  }

  Dataset <- data.frame(cbind(Dataset, avgvec))
  Final_Avg <- Dataset %>%
    group_by(RegionName) %>%
    summarize(avg = mean(avgvec))

  return(Final_Avg)
}

###########################################################
# Groups by GSA Region then averages over rows and columns
# for single column
# note the variable must be name Variable
###########################################################
groupAndAvg.sing <- function(Dataset){

  Final_Avg <- Dataset %>%
    group_by(RegionName) %>%
    summarize(avg = mean(Variable))
  
  return(Final_Avg)
}

##############################################
# Inflates and Deflates Dataset by Year
#############################################
inflate.deflate.dataset <- function(Dataset, inf, def){
  for(i in 2:ncol(Dataset)){
    Dataset[,i] <- (((1+inf)^(i-1))/((1+def)^(i-1)))*Dataset[,i]
  }
  return(Dataset)
}

##############################################
# Inflates and Deflates Value
#############################################
inflate.deflate.value <- function(Value, inf, def, yr){
  Value <- (((1+inf)^(yr))/((1+def)^(yr)))*Value
  return(Value)
}

##############################################
# Inflates and Deflates Vector
#############################################
inflate.deflate.vector <- function(Vector, inf, def, yr){
  Vector <- (((1+inf)^(yr))/((1+def)^(yr)))*Vector
  return(Vector)
}

##############################################
# Summation vector
#############################################
mySummation <- function(DataVector, duration, inf, def){
  totalvec <- rep(0, nrow(DataVector))
  
  for(i in 1:duration){
    totalvec <- totalvec + inflate.deflate.vector(DataVector, inf, def, i)
  }
  return(totalvec)
}

##############################################
# Summation vector and value
#############################################
mySummation.withvalue <- function(DataVector, value, duration, inf, def){
  totalvec <- rep(0, nrow(DataVector))
  
  for(i in 1:duration){
    totalvec <- totalvec + (as.numeric(as.character(inflate.deflate.value(value, inf, def, i))) * inflate.deflate.vector(DataVector, inf, def, i))
  }
  return(totalvec)
}

##############################################
# Summation two vectors 
#############################################
mySummation.withvector <- function(DataVector, ScaleVector, duration, inf, def){
  totalvec <- rep(0, nrow(DataVector))
  
  for(i in 1:duration){
    totalvec <- totalvec + (inflate.deflate.vector(ScaleVector, inf, def, i) * inflate.deflate.vector(DataVector, inf, def, i))
  }
  return(totalvec)
}

##############################################
# Summation two values
#############################################
mySummation.values <- function(value, scalervalue, duration, inf, def){
  totalvec <- rep(0, 1)
  
  for(i in 1:duration){
    totalvec <- totalvec + (as.numeric(as.character(inflate.deflate.value(scalervalue, inf, def, i))) * as.numeric(as.character(inflate.deflate.vector(value, inf, def, i))))
  }
  return(totalvec)
}

##############################################
# Summation one value
#############################################
mySummation.onevalue <- function(value, duration, inf, def){
  totalvec <- rep(0, 1)
  
  for(i in 1:duration){
    totalvec <- totalvec + as.numeric(as.character(inflate.deflate.value(value, inf, def, i))) 
  }
  return(totalvec)
}

##############################################
# Summation util
#############################################
mySummation.util <- function(Vector1, scalarvalue1, Vector2, scalarvalue2, duration, inf, def){
  totalvec <- rep(0, 1)
  
  for(i in 1:duration){
    totalvec <- totalvec + ((inflate.deflate.value(scalarvalue1, inf, def, i) * Vector1) + (inflate.deflate.value(scalarvalue2, inf, def, i) * Vector2))
  }
  return(totalvec)
}

##############################################
# Monthly Mortgage
#############################################
mortgage <-  function(DataVector, rate, yrs){
  rate <- (rate * (1+rate)^yrs)/((1+rate)^yrs - 1)
  payment <- rate * DataVector
  
  return(payment)
}

