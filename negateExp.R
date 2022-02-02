negateExp <- function(expression)
{
  negl <- gsub('\\s', '', expression)
  negl <- unlist(strsplit(negl, '\\+'))
  negl <- strsplit(negl, '\\*') 
  
  negatesin <- function(x) {
    x <- unlist(x)
    x <- sapply(x, function(y) if(length(grep("~",y)) > 0) {y <- gsub('\\~', '',y)}
                else {y <- paste("~", y, sep = "")})
    x <- list(x)
    return(x)
  }
  
  for (k in 1:length(negl))
  {
    negl[k] <- sapply(negl[k], negatesin)
  }
  
  while (length(negl) > 1) {
    x<-unlist(negl[1])
    y<-unlist(negl[2])
    negcomb <- unlist(sapply(x, FUN = function(x) paste(x,"*", y, sep = ""), simplify=FALSE))
    negcomb <- as.vector(negcomb)
    negl[1] <- list(negcomb)
    negl[2] <-NULL
  }
  
  negl <- unlist(negl)
  
  for (i in 1:length(negl))
  {
    neglc <- unique(unlist(strsplit(negl[i], '\\*')))
    t_neg<-character(0)
    t_pre<-character(0)
    if(length(grep("~",neglc)) > 0){
      t_neg<-neglc[grep("~",neglc)]
      t_neg<-gsub('\\~', '', t_neg)
      t_neg<-unlist(t_neg)
      t_pre<-neglc[!neglc %in% neglc[grep("~",neglc)]]
      if (!all(is.na(match(t_pre,t_neg)))) {negl[i] <- NA}
      else {negl[i] <- paste(neglc, collapse = '*')}
    }
    else {negl[i] <- paste(neglc, collapse = '*')}
  }
  
  negl <- (unique(unlist(negl)))
  negl <- negl[!is.na(negl)]
  negl <- paste(negl, collapse = ' + ')
  return(negl)
}
