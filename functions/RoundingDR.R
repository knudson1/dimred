library(plyr)
library(dbplyr)
## Function Definition ##
ReduceDim <- function(dataCol, pqlVal, roundDigits = 1){
  # Error regarding mismatch of lengths
  if(length(unique(dataCol))!= length(pqlVal)){
    stop('Length of PQL Values and the number of levels of the column do not match.')
  }
  # Rounded PQL Values
  pqlVal <- round(pqlVal, digits = roundDigits)
  # Mapping from rounded pql values to levels of dataCol
  dataPqlRound <- data.frame(dataCol = as.numeric(levels(dataCol)),pqlVal = pqlVal)
  
  # Finding the level id for a particular pqlVal
  findID <- function(x){
    
    return(dataPqlRound[dataPqlRound$pqlVal == x,]$dataCol[1])
  }
  
  # Applying the findId to the mapping dataPqlRound
  reducedLevels<-sapply(dataPqlRound$pqlVal, findID)
  
  # Mapping the reducedLevels to dataCol
  return(mapvalues(dataCol,from=levels(dataCol),to=reducedLevels))
}