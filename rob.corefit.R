rob.corefit <- 
  function(test_sol, initial_sol, outcome)
  { all_sols = list()
  if (class(test_sol) == "list"){all_sols <- test_sol}
  else{all_sols[[1]] <- test_sol}
  all_sols[[length(all_sols)+1]] <- initial_sol
  PD <- pimdata(results = all_sols[[1]], outcome = outcome)
  SF <- PD[,"solution_formula", drop=FALSE]
  SF$out <- PD$out
  for (i in 2:length(all_sols))
  {
    PDi <- pimdata(results = all_sols[[i]], outcome = outcome)
    SF$solution_formula <- pmin(PDi$solution_formula, SF$solution_formula)
  }
  
  core_fit <- QCAfit(x = SF$solution_formula, SF$out, necessity = FALSE, cond.lab = "Core.Fit")
  return(core_fit)
  }

IS <- c(0,0,1,0,0,0,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0)
RC <- c(0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0)
OUT <- c(1,0,1,0,0,1,1,1,1,1,1,1,1,0,1,0,0,0,1,1,1,1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,1,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0)
