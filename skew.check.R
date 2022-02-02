skew.check <- function(data, 
                       hist = FALSE,
                       main = NULL)
{ 
  if (any(data>1)  | any(data<0)) stop('The dataframe/set you have provided contains uncalibrated values (outside the range 0-1) or non-numeric values. Please use a subset of your dataframe or provide a dataframe or set that contains only calibrated data.')
  if (is.data.frame(data)) {
    SKEW <- NULL
    for (i in (1:length(data))){
      sh <- NULL
      all <- NULL
      sh <- sum(as.numeric(data[,i]>0.5))
      all <- length(data[,i])
      SKEW[i] <- paste('Set', names(data)[i],'- Cases > 0.5 / Total number of cases:', sh,"/",all, "=", round(sh/all*100, digits=2), "%")
      if (hist == TRUE){
        hist(data[,i], 
             main = colnames(data)[i],
             xlab = "Set Membership")
      }
        
    }
    return(SKEW)
  }
  else {
    sh <- sum(as.numeric(data>0.5))
    all <- length(data)
    skew <- paste('Cases > 0.5 / Total number of cases:', sh,"/",all, "=", round(sh/all*100, digits=2), "%")
    if (hist == TRUE){
      hist(data,
           xlab = "Set Membership",
           main = main)
    }
    return(skew)
  }
}