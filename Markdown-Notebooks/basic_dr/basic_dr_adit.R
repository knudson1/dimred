# Setting Random Seed
set.seed(1234)
# Importing Libraries
library(glmm)
library(dbplyr)
library(plyr)
# Salamander Data
data("salamander")
# Initial GLMM Model
sal <- glmm(Mate ~ 0 + Cross, random = list(~ 0 + Female, ~ 0 + Male), varcomps.names = c("F", "M"), data = salamander, family.glmm = bernoulli.glmm, m = 10^3)
### Reducing Dimensions
#### Reducing Female Dimensions:

# Creating a dataframe which contains the coeffs and the sal_ids and rounding the coeffs to 1 decimal digit
f_c<-data.frame(coeffs=sapply(sal$u.pql[0:60],round,digits=1),sal_id<-c(10:69))
# Taking one sal_id for each coeff
f_c_min<-unique(f_c,by='coeffs')
# Function to find the sal_id for each coeff
find_id<-function(x){
  return(f_c_min$sal_id[match(x,f_c_min$coeffs)])
}
# Filling 'similar' sal_ids with the one from f_c_min , and then creating a new Female_red column in the dataset
salamander$Female_red<-mapvalues(salamander$Female,from=c(10:69),to=f_c$coeffs%>%sapply(find_id))
cat("Original Dimension:", length(unique(salamander$Female)))
cat("Reduced Dimension:",length(unique(salamander$Female_red)))
#Reducing Male Dimensions:

#Similar to Female
m_c<-data.frame(coeffs=sapply(sal$u.pql[61:120],round,digits=1),sal_id<-c(10:69))
m_c_min<-unique(m_c,by='coeffs')
find_id<-function(x){
  return(m_c_min$sal_id[match(x,m_c_min$coeffs)])
}
salamander$Male_red<-mapvalues(salamander$Male,from=c(10:69),to=m_c$coeffs%>%sapply(find_id))
cat("Original Dimension:", length(unique(salamander$Male)))
cat("Reduced Dimension:",length(unique(salamander$Male_red)))
# Process Time Without Dimension Reduction
ptm <- proc.time()
sal <- glmm(Mate ~ 0 + Cross, random = list(~ 0 + Female, ~ 0 + Male), varcomps.names = c("F", "M"),
            data = salamander, family.glmm = bernoulli.glmm, m = 10^3)
proc.time() - ptm
# Process Time With Dimension Reduction
ptm <- proc.time()
sal <- glmm(Mate ~ 0 + Cross, random = list(~ 0 + Female_red, ~ 0 + Male_red), varcomps.names = c("F", "M"),
            data = salamander, family.glmm = bernoulli.glmm, m = 10^3)
proc.time() - ptm