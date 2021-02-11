rob.testrank <- 
  function(test_sol, initial_sol, outcome)
  { 
    robdf <- data.frame(matrix(ncol = 4, nrow = 0))
    x <- c("Number","Model", "SSR", "SC")
    colnames(robdf) <- x
    
    for (i in 1:length(test_sol)){
    test_ind_sol <- test_sol[[i]]
    RCR <- rob.case.ratio(test_sol= test_ind_sol, initial_sol = initial_sol, outcome = outcome)
    RF <- rob.fit(test_sol= test_ind_sol, initial_sol = initial_sol, outcome = outcome)
    robdf[i,1] <- i
    robdf[i,2] <- paste(test_ind_sol$solution[[1]],collapse = "+")
    robdf[i,3] <- RCR[,3]
    robdf[i,4] <- RF[,3]
    }
    names(robdf) <- c("Number","Model", "SSR", "SC")
    robdf <- robdf[order(robdf$SSR,robdf$SC),]
    return(robdf)
  }
