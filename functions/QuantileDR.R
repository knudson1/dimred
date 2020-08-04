library(plyr)
library(dbplyr)
### Function for dimension reduction using quantiles
QuantileDR <- function(IDcolumn, pqls, numQuantiles, std){
  
  #creating a vector the length of the desired number of quantiles entered by the user
  sections <- c(1:(numQuantiles))
  
  #creates all the probabilities needed for the qnorm function
  z <- (c(1:numQuantiles)/numQuantiles)
  
  #goes through each probability and calculates the quantile cutoff
  for (i in 1:(numQuantiles)){
    sections[i] <- qnorm(z[i], sd = std)
  }
  
  #this function takes a  single pql value and goes through each quantile cutoff and returns the appropriate     section it belongs to 
  AssignPQL <- function(x){
    for (j in 1:numQuantiles){
      if(x <= sections[j])
        return(j)
    }
  }
  
  #need to apply the function above to all the pqls
  assigned <- (sapply(pqls,AssignPQL))
  
  #map the levels of the ID column to the pqls that have been newly assigned to a quantile and return
  return(mapvalues(IDcolumn, from = levels(IDcolumn), to = assigned))
}