# Robustness Helpers:

rob.evaluation <-
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
    ND <- rob.evaluation(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    SSrel <- sum(ND$'S1*s2'>0.5)*sum(ND$'s1*S2' >0.5)
    SSrel <- ifelse(SSrel==0, TRUE, FALSE)
    RCR_typ <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/
      (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)))
    RCR_cons <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/
      (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)))
    RCR <- data.frame("Robustness_Case_Ratio"= c(RCR_typ, RCR_cons,SSrel))
    RCR <- t(RCR)
    colnames(RCR) <- c("RCR_typ","RCR_dev","SSR")
    RCR[,1:2] <- round(RCR[,1:2], digits = 3)
    RCR[,3] <- as.logical(RCR[,3])
    return(RCR)
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

