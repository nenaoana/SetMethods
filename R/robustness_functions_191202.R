###
# New robustness functions:
###

core.fit <- 
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

robustness.fit <- 
  function(test_sol, initial_sol, outcome)
  { 
    all_sols = list()
    if (class(test_sol) == "list"){all_sols <- test_sol}
    else{all_sols[[1]] <- test_sol}
    all_sols[[length(all_sols)+1]] <- initial_sol
    rcf <- core.fit(test_sol, initial_sol, outcome)
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
    P1 <- pimdata(results = initial_sol, outcome = outcome)
    rob_sc <- sum(pmin(P1$solution_formula, P2$solution_formula))/
              sum(pmax(P1$solution_formula, P2$solution_formula))
    ND <- robustness.evaluation(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    SSrel <- sum(ND$'S1*s2'>0.5)*sum(ND$'s1*S2' >0.5)
    SSrel <- ifelse(SSrel==0, TRUE, FALSE)
    rob <- data.frame("Robustness_Fit"= c(rob_cov, rob_cons, rob_sc, SSrel))
    rob <- t(rob)
    colnames(rob) <- c("RF_cov","RF_cons","RF_SC", "Subset_Relation")
    rob[,1:3] <- round(rob[,1:3], digits = 3)
    rob[,4] <- as.logical(rob[,4])
    return(rob)
  }


robustness.xyplot <- 
  function (test_sol, 
            initial_sol, 
            outcome,
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
    RF <-  robustness.fit(test_sol, initial_sol, outcome)
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
    RCRtyp <- format(RCR[1,], digits = 4)
    RCRcons <- format(RCR[2,], digits = 4)
    subtt <- paste("RFcons: ", Rcon, "; RFcov: ", Rcov, "; RFcoinc: ", RSC, "; RCRtyp: ", RCRtyp, "; RCRcons: ", RCRcons, sep = "")
    rob=TRUE
    if (labs == TRUE){
    xy.plot("is", "ts",
            data = PS,
            xlab = "Initial Solution (IS)", ylab = "Test Solution (TS)", main = "Robustness Plot",
            shape = ifelse(PS$out > 0.5, 19, 9), jitter=jitter, fontsize = fontsize,
            rob = TRUE, rbfit = subtt)}
    else {
      xy.plot("is", "ts",
              data = PS,
              xlab = "Initial Solution (IS)", ylab = "Test Solution (TS)", main = "Robustness Plot", 
              labs=NULL,
              shape = ifelse(PS$out > 0.5, 19, 9), fontsize = fontsize,
              rob = TRUE, rbfit = subtt)}
  }

robustness.evaluation <-
  function(test_sol, 
           initial_sol, 
           outcome)
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
    
    P <- data.frame(P1$solution_formula, P2$solution_formula)
    names(P) <- c("Sol.Formula1", "Sol.Formula2")
    row.names(P) <- row.names(P1)
    P$'S1*S2' <- pmin( P$Sol.Formula1, P$Sol.Formula2)
    P$'s1*S2' <- pmin(1-P$Sol.Formula1,   P$Sol.Formula2)
    P$'S1*s2' <- pmin(  P$Sol.Formula1, 1-P$Sol.Formula2)
    P$'s1*s2' <- pmin(1-P$Sol.Formula1, 1-P$Sol.Formula2)
    P$'Outcome' <- P1$out
    
    
    return(P)
  }

rob.eval.fit <-
  function(robust_eval)
  {            
    n_c <- ncol(robust_eval)-1
    rob_fit <- data.frame(matrix(NA, ncol=4, nrow=0))  
    for (i in (1:n_c)){
      rob_fit <- rbind(rob_fit, QCAfit(robust_eval[,i], robust_eval[, ncol(robust_eval)], necessity = FALSE))}
    rownames(rob_fit) <- names(robust_eval[1:n_c])
    return(rob_fit)
  }

rob.case.ratio <-
  function(test_sol, 
            initial_sol, 
            outcome)
  {
    ND <- robustness.evaluation(test_sol = test_sol, 
                                 initial_sol = initial_sol, 
                                 outcome=outcome)
    RCR_typ <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/
               (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)))
    RCR_cons <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/
      (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)))
    RCR <- data.frame("Robustness_Case_Ratio"= c(RCR_typ, RCR_cons))
    row.names(RCR) <- c("RCR_typ","RCR_cons")
    RCR <- round(RCR, digits = 3)
    return(RCR)
  }

robustness.cases <-
  function(test_sol, 
           initial_sol, 
           outcome,
           use.tilde=TRUE)
  {
    ND <- robustness.evaluation(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    INT <- robust.intersections(test_sol = test_sol, 
                                initial_sol = initial_sol,
                                sol_i = 1, 
                                use.tilde = use.tilde)
    RCR <- rob.case.ratio(test_sol = test_sol, 
                          initial_sol = initial_sol, 
                          outcome=outcome)
    CTE <- list(
      'S1S2Y'=list('Intersection'='Robust Typical Cases (IS*TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1S2y'=list('Intersection'='Robust Deviant Cases (IS*TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
      'S1s2Y'=list('Intersection'='Shaky Typical Cases (IS*ts and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1s2y'=list('Intersection'='Shaky Deviant Cases(IS*ts and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)]}),
      's1S2Y'=list('Intersection'='Possible Typical Cases (is*TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$s1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'s1*S2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
      's1S2y'=list('Intersection'='Possible Deviant Cases (is*TS and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                             sum((ND$'s1*S2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                             "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
                's1s2Y'=list('Intersection'='Extreme Deviant Coverage Cases (is*ts and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                             sum((ND$'s1*s2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                             "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
                's1s2y'=list('Intersection'='Irrelevant Cases (is*ts and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                             sum((ND$'s1*s2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                             "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)]}))
    class(CTE) <- "casestheoryeval"
    CR = list('CaseRatio' = RCR,
              'CaseNames' = CTE)
    return(CR)
  }

robust.intersections <- function(test_sol, initial_sol, sol_i = 1, use.tilde = TRUE)
{ 
  if (class(test_sol) == "list"){
    results1 = test_sol[[1]]
  }
  else results1 = test_sol
  
  if (is.null(results1$i.sol)){
    s2 <- results1$solution[[1]]}
  else{
    s2 <- results1$i.sol$C1P1$solution[[1]]}
  
  results2 = initial_sol
  if (is.null(results2$i.sol)){
    if (is.character(sol_i)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s1 <- results2$solution[[sol_i]]}
  else{
    if (is.numeric(sol_i)){
      s1 <- results2$i.sol$C1P1$solution[[sol_i]]}
    else {
      if (is.character(sol_i)){
        if (!nchar(sol_i)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol_i <- toupper(sol_i)  
        int2 <- as.numeric(unlist(strsplit(sol_i, "I"))[2])
        mod2 <- toupper(unlist(strsplit(sol_i, "I"))[1])
        if (int2 > length(get(mod2, pos = results2$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s1 <- get(mod2, pos = results2$i.sol)$solution[[int2]]
      }
      else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
    }
  }
  
  if (class(test_sol) == "list")
  {
    test_int <- s2
    for (i in length(test_sol))
    {
      test_int <- intersection(test_int, test_sol[[i]])
      test_int <- test_int[1]
    }
  }
  
  else {test_int <- s2}
  
  tild <- function(x)
  {
    x <- unlist(strsplit(x, '\\*'))
    x <- as.vector(unlist(sapply(x, function (y) 
      if (!y==toupper(y)){y <- paste("~",toupper(y),sep="")} 
      else { y <- y})))
    x <- paste(x, collapse = "*")
  }
  
  if (!use.tilde){  
    emp2 <- as.vector(unlist(sapply(test_int, function(x)  tild(x))))
    emp2 <- paste(emp2, collapse = "+")
    emp1 <- as.vector(unlist(sapply(s1, function(x)  tild(x))))
    emp1 <- paste(emp1, collapse = "+")}
  else {
    emp2 <- toupper(test_int)
    emp1 <- toupper(s1)}
  
  thintersect <- list()
  
  thintersect$S1S2 <- intersectExp(emp1,emp2)
  thintersect$s1S2 <- intersectExp(negateExp(emp1),emp2)
  thintersect$S1s2 <- intersectExp(emp1,negateExp(emp2))
  thintersect$s1s2 <- intersectExp(negateExp(emp1),negateExp(emp2))
  
  class(thintersect) <- 'robtersect'
  return(thintersect)
}

print.robtersect <-
  function(x, ...)
  {
    cat("\nIS*TS:\n--------------------\n\n")
    print(x$S1S2)
    cat("\n~IS*TS:\n--------------------\n\n")
    print(x$s1S2)
    cat("\nIS*~TS:\n--------------------\n\n")
    print(x$S1s2)
    cat("\n~IS*~TS:\n--------------------\n\n")
    print(x$s1s2)
    cat('\n') }  

calib.range <-
  function(
    raw.data,
    calib.data,
    test.cond,
    test.thresholds,
    step = 1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    dir.exp = "",
    ...
  )
  {
    calib.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = test.thresholds)
    suppressWarnings(init.sol <- minimize(data = calib.data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          dir.exp = dir.exp))
    
    # Testing the 0.5 range:
    tu.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu.thresholds[2] = tu.thresholds[2] + step;
      c.data = calib.data;
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tu.thresholds);
      suppressWarnings(sol <- minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp));
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu.thresholds[2] - test.thresholds[2]) == max.runs*step) 
      {tu.thresholds[2] = NA
      break}
      if (tu.thresholds[2]>= range(raw.data[,test.cond])[2]) {break}
    }
    
    tl.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tl.thresholds[2] = tl.thresholds[2] - step
      c.data = calib.data
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tl.thresholds)
      suppressWarnings(sol <- minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[2]-tl.thresholds[2]) == max.runs*step) 
      {tl.thresholds[2] = NA
      break}
      if (tl.thresholds[2]<= range(raw.data[,test.cond])[1]) {break}
    }
    
    # Testing the 1  range:
    tu1.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu1.thresholds[3] = tu1.thresholds[3] + step
      c.data = calib.data
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tu1.thresholds)
      suppressWarnings(sol <- minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu1.thresholds[3]-test.thresholds[3]) == max.runs*step) 
      {tu1.thresholds[3] = NA
      break}
    }
    
    tl1.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...");
      tl1.thresholds[3] = tl1.thresholds[3] - step
      c.data = calib.data
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tl1.thresholds)
      suppressWarnings(sol <- minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[3]-tl1.thresholds[3]) == max.runs*step) 
      {tl1.thresholds[3] = NA
      break}
    }
    
    # Testing the 0 range:
    tu0.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu0.thresholds[1] = tu0.thresholds[1] + step
      c.data = calib.data
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tu0.thresholds)
      suppressWarnings(sol <- minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu0.thresholds[1]-test.thresholds[1]) == max.runs*step) 
      {tu0.thresholds[1] = NA
      break}
    }
    
    tl0.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(data = calib.data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tl0.thresholds[1] = tl0.thresholds[1] - step
      c.data = calib.data
      c.data[,test.cond] <- calibrate(raw.data[,test.cond], type="fuzzy", thresholds = tl0.thresholds)
      sol <- suppressWarnings(minimize(data = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[1]-tl0.thresholds[1]) == max.runs*step) 
      {tl0.thresholds[1] = NA
      break}
    }
    
    E = c(tl0.thresholds[1]+step, tu0.thresholds[1]-step)
    C = c(tl.thresholds[2]+step, tu.thresholds[2]-step)
    I = c(tl1.thresholds[3]+step, tu1.thresholds[3]-step)
    TH <- data.frame(E,C,I)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("Exclusion: ","Lower bound ", tl0.thresholds[1]+step, "Threshold ", test.thresholds[1] , "Upper bound ", tu0.thresholds[1]-step, "\n"))
    cat(c("Crossover: ","Lower bound ", tl.thresholds[2]+step, "Threshold ", test.thresholds[2] , "Upper bound ", tu.thresholds[2]-step, "\n"))
    cat(c("Inclusion: ","Lower bound ", tl1.thresholds[3]+step, "Threshold ", test.thresholds[3] , "Upper bound ", tu1.thresholds[3]-step, "\n"))
    invisible(TH)
  }  


incl.range <-
  function(
    data,
    step = 0.1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    dir.exp = "",
    ...
  )
  {
    
    suppressWarnings(init.sol <- minimize(data = data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          dir.exp = dir.exp))
    # Test range raw consistency threshold lower:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    incl.cut.tl = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tl = incl.cut.tl - step
      sol <- suppressWarnings(minimize(data = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut.tl,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    
    # Test range raw consistency threshold upper:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    incl.cut.tu = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tu = incl.cut.tu + step
      sol <- try(suppressWarnings(minimize(data = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut.tu,
                                       n.cut = n.cut,
                                       include = include,
                                       dir.exp = dir.exp)), silent = TRUE)
      if (class(sol) == "try-error") {break}
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    RCT = c(incl.cut.tl+step, incl.cut.tu-step)
    TH <- data.frame(RCT)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("Raw Consistency T.: ","Lower bound ", incl.cut.tl+step, "Threshold ", incl.cut , "Upper bound ", incl.cut.tu - step, "\n"))
    invisible(TH)
  }  

ncut.range <-
  function(
    data,
    step = 1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    dir.exp = "",
    ...
  )
  {
    
    suppressWarnings(init.sol <- minimize(data = data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          dir.exp = dir.exp))
    
    # Test range n.cut lower:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    n.cut.tl = n.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      n.cut.tl = n.cut.tl - step
      if (n.cut.tl < 1) { break }
      sol <- suppressWarnings(minimize(data = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut.tl,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    
    # Test range n.cut upper:
    suppressWarnings(sol <- minimize(data = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     dir.exp = dir.exp))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    n.cut.tu = n.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      n.cut.tu = n.cut.tu + step
      if (n.cut.tl == nrow(data)) { break }
      sol <- suppressWarnings(minimize(data = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut.tu,
                                       include = include,
                                       dir.exp = dir.exp))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
    }
    
    NCUT = c(n.cut.tl+step, n.cut.tu-step)
    TH <- data.frame(NCUT)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("N.Cut: ","Lower bound ", n.cut.tl+step, "Threshold ", n.cut , "Upper bound ", n.cut.tu - step, "\n"))
    invisible(TH)
  }  




