# Return prediction for glmm's salamander based models
expit <- function(x){
  return((exp(x)/(exp(x)+1)))
}
predict.glmm<- function(sal,col1,col2){
  return(expit(salamander$Cross %>% sal$beta[.]
               + (salamander[,col1] %>% sal$u.pql[1:length(unique(salamander[,col1]))][.]) 
               + (salamander[,col2] %>% sal$u.pql[length(unique(salamander[,col1])):(length(unique(salamander[,col1]))+length(unique(salamander[,col2])))][.])))
}