intersectExp <- function (expression1, expression2) 
{
  tl <- gsub('\\s', '', expression1)
  tl <- unlist(strsplit(tl, '\\+'))
  el <- gsub('\\s', '', expression2)
  el <- unlist(strsplit(el, '\\+')) 
  comb <- c()
  for (i in 1:length(tl)){
    for (j in 1:length(el)){
      comb <- c(comb, paste(tl[i], el[j], sep = "*"))
    }
  }
  
  combl <- strsplit(comb, '\\*') 
  
  for (k in 1:length(combl))
  {
    combuni <- unique(unlist(combl[k]))
    t_neg<-character(0)
    t_pre<-character(0)
    if(length(grep("~",combuni)) > 0){
      t_neg<-combuni[grep("~",combuni)]
      t_neg<-gsub('\\~', '', t_neg)
      t_neg<-unlist(t_neg)
      t_pre<-combuni[!combuni %in% combuni[grep("~",combuni)]]
      if (!all(is.na(match(t_pre,t_neg)))) {combl[k] <- NA}
      else {combl[k] <- paste(combuni, collapse = '*')}
    }
    else {combl[k] <- paste(combuni, collapse = '*')}
  }
  combf <- (unique(unlist(combl)))
  combf <- combf[!is.na(combf)]
  
  if (length(combf)==0) {combff = "Empty Set"}
  
  else{
  combfl <- strsplit(combf, '\\*') 
  
  
 
  for (l in 1:length(combfl)) {
    for (m in 1:length(combfl)) {
      if (all(combfl[[l]] %in% combfl[[m]])) { # here wrong, need to check if vector is subset of another vector
        if (length(unlist(combfl[[l]])) >= length(unlist(combfl[[m]])))
        {combfl[[l]] <- combfl[[m]]}
        else {combfl[[m]] <- combfl[[l]]}
      }
    }}
  
  
  combff <- unique(combfl)

  for (i in 1: length(combff)){ 
    for (j in 1: length(combff)){
      if (length(setdiff(combff[[i]], combff[[j]])) == 1 & 
          length(setdiff(combff[[j]], combff[[i]])) == 1)
      {
        if (gsub('\\~', '', setdiff(combff[[i]], combff[[j]]))
            == gsub('\\~', '', setdiff(combff[[j]], combff[[i]]))) 
        {
          if (length(grep("~", setdiff(combff[[i]], combff[[j]])))==1) {
            combff[[j]] <- combff[[j]][!combff[[j]] %in% c(gsub('\\~', '', setdiff(combff[[i]], combff[[j]])),
                                                           gsub('\\~', '', setdiff(combff[[j]], combff[[i]])))]
            combff[[i]] <- combff[[j]]
          }
          else{
            combff[[i]] <- combff[[i]][!combff[[i]] %in% c(gsub('\\~', '', setdiff(combff[[i]], combff[[j]])),
                                                           gsub('\\~', '', setdiff(combff[[j]], combff[[i]])))]
            combff[[j]] <- combff[[i]]
          }
        }
      }
    }
  }
  
  combff <- unique(combff)
  combff <- sapply(combff, function(x) paste(x, collapse = "*"))
  combff <- paste(combff, collapse = " + ")
  if (combff!="Empty Set"){combff <- simplify(combff)[[1]]}
  }
  return(combff)
}
