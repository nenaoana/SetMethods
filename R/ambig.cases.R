ambig.cases <- function(data)
{
  if (any(data>1)  | any(data<0)) stop('The dataframe/set you have provided contains uncalibrated values (outside the range 0-1). Please use a subset of your dataframe or provide a calibrated dataframe or set.')
  if (is.data.frame(data)) {
    c <- which(data == 0.5, arr.ind = TRUE)
    if (length(c)==0){return("There are no cases with fuzzy-set scores of 0.5.")}
    else{return(c)}
    }
  else {
    c <- which(data == 0.5, arr.ind = TRUE)
    if (length(c)==0){return("There are no cases with fuzzy-set scores of 0.5.")}
    else{return(c)}
  }
}



