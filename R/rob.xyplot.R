rob.xyplot <- 
  function (test_sol, 
            initial_sol, 
            outcome,
            all_labels = FALSE,
            jitter = TRUE,
            fontsize = 3,
            labs = TRUE,
            area_lab=TRUE)
  { 
    if ("list" %in% class(test_sol))
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
      if ("list" %in% class(test_sol))
      {
        P3 <- pimdata(results = test_sol[[1]], outcome = outcome)
        for (i in length(test_sol))
        {
          Pi <- pimdata(results = test_sol[[i]], outcome = outcome)
          P3$solution_formula <- pmax(Pi$solution_formula, P3$solution_formula)
        }
      }
      else {
        P3 <- pimdata(results = test_sol, outcome = outcome)
      }
    
    
    
    P1 <- pimdata(results = initial_sol, outcome = outcome)
    
    #CF <-  core.fit(test_sol, initial_sol, outcome)
    RF <-  rob.fit(test_sol, initial_sol, outcome)
    RCR <- rob.case.ratio(test_sol, initial_sol, outcome)
    
    m2 <-deparse(substitute(initial_sol))
    PS <- data.frame(P1$solution_formula, P2$solution_formula, P3$solution_formula, P1$out)
    names(PS) <- c("is", "mints", "maxts", "out")
    PS$ts <- NA
    PS$ts <- ifelse(PS$is>0.5,PS$mints, PS$maxts)
    rownames(PS) <- rownames(P1)
    
    # Format Pofs for plot printing:
    #Ccon <- CF[1]
    #Ccov <- CF[2]
    Rcon <- format(RF[,2], digits = 3)
    Rcov <- format(RF[,1], digits = 3)
    RSC_minTS <- format(RF[,3], digits = 3)
    RSC_maxTS <- format(RF[,4], digits = 3)
    RCRtyp_minTS <- format(RCR[,1], digits = 3)
    RCRcons_minTS <- format(RCR[,2], digits = 3)
    #RCRtyp_maxTS <- format(RCR[,4], digits = 3)
    #RCRcons_maxTS <- format(RCR[,5], digits = 3)
    RCC_Rank <- RCR[,3]
    #SSR_maxTS <- RCR[,6]
    #subtt <- paste("RF_cons: ", Rcon, "; RF_cov: ", Rcov, "; RF_SC_minTS: ", RSC_minTS, "; RF_SC_maxTS: ", RSC_maxTS, "\n" ,"RCR_typ: ", RCRtyp_minTS, "; RCR_dev: ", RCRcons_minTS)
    subtt <- paste("RF_cons: ", Rcon, "; RF_cov: ", Rcov, "; RF_SC_minTS: ", RSC_minTS, "; RF_SC_maxTS: ", RSC_maxTS, "\n" ,"RCR_typ: ", RCRtyp_minTS, "; RCR_dev: ", RCRcons_minTS, "; RCC_Rank: ", RCC_Rank,sep = "")
    rob=TRUE
    if (labs == TRUE){
      if (all_labels) {fil <- rownames(PS)}
      else {fil <- rownames(PS)
      fil[with(PS, (PS[1] < 0.5  & PS[5]<0.5))] <- ""
      fil[with(PS, (PS[1] > 0.5  & PS[5]>0.5))] <- ""}
      p <- xy.plot("is", "ts",
              data = PS,
              xlab = "Initial Solution (IS)", ylab = "Min/Max Test Set (TS)", main = "Robustness Plot",
              labs = fil,
              shape = ifelse(PS$out > 0.5, 19, 9), jitter=jitter, fontsize = fontsize,
              rob = TRUE, rbfit = subtt)
    }
    else {
      p<-xy.plot("is", "ts",
              data = PS,
              xlab = "Initial Solution (IS)", ylab = "Min/Max Test Set (TS)", main = "Robustness Plot", 
              labs=NULL,
              shape = ifelse(PS$out > 0.5, 19, 9), fontsize = fontsize,
              rob = TRUE, rbfit = subtt)
    }
      
      if (area_lab==TRUE){
        p+annotate ("rect", xmin = 0.5, xmax = 1, ymin = -Inf, ymax = 0.5, alpha = .2)+
          annotate ("rect", xmin = -Inf, xmax = 0.5, ymin = 0.5, ymax = 1, alpha = .2)+
          annotate("label", x = 0.75, y = 0, label = "minTS<0.5", size = 4, fontface = "bold", alpha = .1)+
          annotate("label", x = 0.25, y = 0, label = "maxTS<0.5", size = 4, fontface = "bold", alpha = .1)+
          annotate("label", x = 0.75, y = 1, label = "minTS>0.5", size = 4, fontface = "bold", alpha = .1)+
          annotate("label", x = 0.25, y = 1, label = "maxTS>0.5", size = 4, fontface = "bold", alpha = .1)# Label to horizontal cut-off line.
      }
      else{p}
  }