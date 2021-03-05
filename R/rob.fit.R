rob.fit <- 
  function(test_sol, initial_sol, outcome)
  { 
    all_sols = list()
    if (class(test_sol) == "list"){all_sols <- test_sol}
    else{all_sols[[1]] <- test_sol}
    all_sols[[length(all_sols)+1]] <- initial_sol
    rcf <- rob.corefit(test_sol, initial_sol, outcome)
    isf <- QCAfit(initial_sol, outcome, necessity = FALSE)
    rob_cons <- isf["solution_formula",1]/rcf[1]
    rob_cov <- rcf[2]/isf["solution_formula",2]
    if (class(test_sol) == "list")
    {
      P2 <- pimdata(results = test_sol[[1]], outcome = outcome)
      for (i in length(test_sol))
      {
        Pi <- pimdata(results = test_sol[[i]], outcome = outcome)
        P2$solution_formula <- pmin(Pi$solution_formula, P2$solution_formula)
      }
    }
    else {
      P2 <- pimdata(results = test_sol, outcome = outcome)
    }
    if (class(test_sol) == "list")
    {
      P3 <- pimdata(results = test_sol[[1]], outcome = outcome)
      for (j in length(test_sol))
      {
        Pj <- pimdata(results = test_sol[[j]], outcome = outcome)
        P3$solution_formula <- pmax(Pj$solution_formula, P3$solution_formula)
      }
    }
    else {
      P3 <- pimdata(results = test_sol, outcome = outcome)
    }
    P1 <- pimdata(results = initial_sol, outcome = outcome)
    rob_sc_min <- sum(pmin(P1$solution_formula, P2$solution_formula))/
      sum(pmax(P1$solution_formula, P2$solution_formula))
    rob_sc_max <- sum(pmin(P1$solution_formula, P3$solution_formula))/
      sum(pmax(P1$solution_formula, P3$solution_formula))
    ND <- rob.evaluation(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    rank <- robust.rank(test_sol = test_sol, 
                       initial_sol = initial_sol, 
                       outcome=outcome)
    #SSrel <- sum(ND$'S1*s2'>0.5)*sum(ND$'s1*S2' >0.5)
    #SSrel <- ifelse(SSrel==0, TRUE, FALSE)
    #rob <- data.frame("Robustness_Fit"= c(rob_cov, rob_cons, rob_sc, SSrel))
    rob <- data.frame("Robustness_Fit"= c(rob_cov, rob_cons, rob_sc_min, rob_sc_max))
    rob <- t(rob)
    colnames(rob) <- c("RF_cov","RF_cons","RF_SC_minTS","RF_SC_maxTS")
    rob[,1:4] <- round(rob[,1:4], digits = 3)
    #rob[,4] <- as.logical(rob[,4])
    return(rob)
  }