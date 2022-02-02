rob.singletest <- 
  function(test_sol, initial_sol, outcome)
  { 
    robdf <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("Model", "SSR", "SC")
    colnames(robdf) <- x
    
    for (i in 1:length(test_sol)){
    test_ind_sol <- test_sol[[i]]
    RCR <- rob.case.ratio(test_sol= test_ind_sol, initial_sol = initial_sol, outcome = outcome)
    RF <- rob.fit(test_sol= test_ind_sol, initial_sol = initial_sol, outcome = outcome)
    #robdf[i,1] <- i
    robdf[i,1] <- paste(test_ind_sol$solution[[1]],collapse = "+")
    robdf[i,2] <- RCR[,3]
    robdf[i,3] <- RF[,3]
    }
    names(robdf) <- c("Model", "RCC_Rank", "SC")
    robdf <- robdf[order(-robdf$RCC_Rank,robdf$SC),]
    return(robdf)
  }
