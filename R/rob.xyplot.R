rob.xyplot <- 
  function (test_sol, 
            initial_sol, 
            outcome,
            all_labels = FALSE,
            jitter = TRUE,
            fontsize = 3,
            labs = TRUE)
  {
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
    P1 <- pimdata(results = initial_sol, outcome = outcome)
    
    #CF <-  core.fit(test_sol, initial_sol, outcome)
    RF <-  rob.fit(test_sol, initial_sol, outcome)
    RCR <- rob.case.ratio(test_sol, initial_sol, outcome)
    
    m2 <-deparse(substitute(initial_sol))
    PS <- data.frame(P1$solution_formula, P2$solution_formula, P1$out)
    names(PS) <- c("is", "ts", "out")
    rownames(PS) <- rownames(P1)
    
    # Format Pofs for plot printing:
    #Ccon <- CF[1]
    #Ccov <- CF[2]
    Rcon <- format(RF[,2], digits = 3)
    Rcov <- format(RF[,1], digits = 3)
    RSC <- format(RF[,3], digits = 3)
    RCRtyp <- format(RCR[,1], digits = 3)
    RCRcons <- format(RCR[,2], digits = 3)
    SSrel <- RCR[,3]
    subtt <- paste("RF_cons: ", Rcon, "; RF_cov: ", Rcov, "; RF_SC: ", RSC, "; RCR_typ: ", RCRtyp, "; RCR_dev: ", RCRcons, "; SSR: ", SSrel,sep = "")
    rob=TRUE
    if (labs == TRUE){
      if (all_labels) {fil <- rownames(PS)}
      else {fil <- rownames(PS)
      fil[with(PS, (PS[1] < 0.5  & PS[2]<0.5))] <- ""
      fil[with(PS, (PS[1] > 0.5  & PS[2]>0.5))] <- ""}
      xy.plot("is", "ts",
              data = PS,
              xlab = "Initial Solution (IS)", ylab = "Test Set (TS)", main = "Robustness Plot",
              labs = fil,
              shape = ifelse(PS$out > 0.5, 19, 9), jitter=jitter, fontsize = fontsize,
              rob = TRUE, rbfit = subtt)}
    else {
      xy.plot("is", "ts",
              data = PS,
              xlab = "Initial Solution (IS)", ylab = "Test Set (TS)", main = "Robustness Plot", 
              labs=NULL,
              shape = ifelse(PS$out > 0.5, 19, 9), fontsize = fontsize,
              rob = TRUE, rbfit = subtt)}
  }