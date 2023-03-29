stargazerSol <- 
  function(results,
           outcome,
           sol = 1,
           show.cases = FALSE,
           type = "latex", 
           title = "",
           out = NULL,
           digits = 3)
  {
    if (!("QCA_min" %in% class(results))) stop ("The result provided must be a solution obtained with the minimize function!")
    
    PD <- pimdata(results=results, outcome=outcome, sol = sol)
    
    # Create solution table:
    # Parsimonious
    if (is.null(results$i.sol)){
      if (is.null(results$IC$overall$incl.cov)){
        tabp = results$IC$incl.cov
        tabs = results$IC$sol.incl.cov
      }
      else{
        tabp = results$IC$overall$incl.cov
        tabs = results$IC$overall$sol.incl.cov}
    tabs$covU <- NA
    tabs$cases <- NA
    rownames(tabs) <- "solution_formula"
    tab = rbind(tabp,tabs)
    model = names(PD[,-length(PD)])
    tabmodel = tab[model,]
    rownames(tabmodel)[which(rownames(tabmodel)=="solution_formula")]= "Solution"
    }
    # Intermediate
    else{
      if (is.numeric(sol)){
        if (is.null(results$i.sol$C1P1$IC$overall)){
          tabp = results$i.sol$C1P1$IC$incl.cov
          tabs = results$i.sol$C1P1$IC$sol.incl.cov
        }
        else{
          tabp = results$i.sol$C1P1$IC$overall$incl.cov
          tabs = results$i.sol$C1P1$IC$overall$sol.incl.cov
        }
      }
      else{
        if (!nchar(sol)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol <- toupper(sol)  
        mod <- toupper(unlist(strsplit(sol, "I"))[1])
        s <- get(mod, pos = results$i.sol)$IC
        if (is.null(s$overall)){
          tabp = s$incl.cov
          tabs = s$sol.incl.cov
        }
        else{
          tabp = s$overall$incl.cov
          tabs = s$overall$sol.incl.cov
        }
      }
    tabs$covU <- NA
    tabs$cases <- NA
    rownames(tabs) <- "solution_formula"
    tab = rbind(tabp,tabs)
    model = names(PD[,-length(PD)])
    tabmodel = tab[model,]
    rownames(tabmodel)[which(rownames(tabmodel)=="solution_formula")]= "Solution" 
    }
    
    rownames(tabmodel) <- gsub("~","~ ",rownames(tabmodel))
    
    #print(xtable(tabmodel), hline.after = c(-1,1))
    
    suppressWarnings(tabmodel[,1:4] <- round(tabmodel[,1:4], digits = digits))
    
    if (show.cases == FALSE){tabmodel <- tabmodel[,-5]}
    
    suppressWarnings(stargazer(tabmodel, summary = FALSE, type = type, title = title, out = out))
  }
