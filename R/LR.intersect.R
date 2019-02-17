LR.intersect <-
  function(results1, 
           results2,
           sol1=1,
           sol2=1)
  {		
    if (is.null(results1$i.sol)){
      if (is.character(sol1)) stop('For parsimonious solutions, the model must be specificied numerically (e.g. sol1=2).')
      mod1 <- paste('M',sol1, sep="")
      CF1 <- get(mod1, pos = results1$SA)
    }
    else{
      if (is.numeric(sol1)){
        CF1 <- results1$i.sol$C1P1$EC}
      else {
        if (is.character(sol1)){
          if (!nchar(sol1)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
          sol1 <- toupper(sol1)  
          int1 <- as.numeric(unlist(strsplit(sol1, "I"))[2])
          mod1 <- toupper(unlist(strsplit(sol1, "I"))[1])
          if (int1 > length(get(mod1, pos = results1$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
          CF1 <- get(mod1, pos = results1$i.sol)$EC
        }
        else {return("The model given to argument sol1= is invalid or in the wrong format. Please check the helpfile for LR.intersect using ?LR.intersect for the appropiate format.")}
      }
    }
    
    if (is.null(results2$i.sol)){
      if (is.character(sol2)) stop('For parsimonious solutions, the model must be specificied numerically (e.g. sol2=2).')
      mod2 <- paste('M',sol2, sep="")
      CF2 <- get(mod2, pos = results2$SA)
    }
    else{
      if (is.numeric(sol2)){
        CF2 <- results2$i.sol$C1P1$EC}
      else {
        if (is.character(sol2)){
          if (!nchar(sol2)==6) stop('The model is specified in the wrong format. Please check the helpfile for LR.intersect using ?LR.intersect for the appropiate format.')
          sol2 <- toupper(sol2)  
          int2 <- as.numeric(unlist(strsplit(sol2, "I"))[2])
          mod2 <- toupper(unlist(strsplit(sol2, "I"))[1])
          if (int2 > length(get(mod2, pos = results2$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
          CF2 <- get(mod2, pos = results2$i.sol)$EC
        }
        else {return("The model given to argument sol2= is invalid or in the wrong format. Please check the helpfile for LR.intersect using ?LR.intersect for the appropiate format.")}
      }
    }
    CCF <- intersect(rownames(CF1),rownames(CF2))
    return(CCF)
  }
