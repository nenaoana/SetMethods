# Robustness Helpers:
rob.union <- function(test_sol)
{
  ts <- ""
  for (i in 1:length(test_sol)){
    if (is.null(test_sol[[i]]$i.sol)){
      ts <- c(ts,test_sol[[i]]$solution[[1]])
    }
    else{
      ts <- c(ts,test_sol[[i]]$i.sol$C1P1$solution[[1]])
      }
    }
  ts <- paste(ts[2:length(ts)], collapse = "+")
  ts <- simplify(ts)
  return(ts)
}

rob.evaluation <-
  function(test_sol, 
           initial_sol, 
           outcome)
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
    
    P <- data.frame(P1$solution_formula, P2$solution_formula, P3$solution_formula)
    names(P) <- c("Sol.Formula1", "Sol.Formula2", "Sol.Formula3")
    row.names(P) <- row.names(P1)
    P$'S1*S2' <- pmin( P$Sol.Formula1, P$Sol.Formula2)
    P$'S1*S3' <- pmin( P$Sol.Formula1, P$Sol.Formula3) # here we apply the max S1S3
    P$'s1*S2' <- pmin(1-P$Sol.Formula1,   P$Sol.Formula2) 
    P$'s1*S3' <- pmin(1-P$Sol.Formula1,   P$Sol.Formula3) # here we apply the max s1S3
    P$'S1*s2' <- pmin(  P$Sol.Formula1, 1-P$Sol.Formula2)
    P$'S1*s3' <- pmin(  P$Sol.Formula1, 1-P$Sol.Formula3) # here we apply the max S1s3
    P$'s1*s2' <- pmin(1-P$Sol.Formula1, 1-P$Sol.Formula2)
    P$'s1*s3' <- pmin(1-P$Sol.Formula1, 1-P$Sol.Formula3) # here we apply the max s1s3
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
    #SSrel_minTS <- sum(ND$'S1*s2'>0.5)*sum(ND$'s1*S3' >0.5)
    #SSrel_minTS <- ifelse(SSrel_minTS==0, TRUE, FALSE)
    #SSrel_maxTS <- sum(ND$'S1*s3'>0.5)*sum(ND$'s1*S3' >0.5)
    #SSrel_maxTS <- ifelse(SSrel_maxTS==0, TRUE, FALSE)
    Shaky <- sum(ND$'S1*s2'>0.5)
    Poss <- sum(ND$'s1*S3'>0.5)
    if (Shaky==0 & Poss==0){RCC_Rank<- 1}
    if (Shaky==0 & Poss>0){RCC_Rank<- 2}
    if (Shaky>0 & Poss==0){RCC_Rank<- 3}
    if (Shaky>0 & Poss>0){RCC_Rank<- 4}
    RCR_typ_minTS <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/
      (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)))
    #RCR_typ_maxTS <- sum((ND$'S1*S3'>0.5)&(ND$'Outcome' >0.5))/
    #  (sum((ND$'S1*s3'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5))+sum((ND$'S1*S3'>0.5)&(ND$'Outcome' >0.5)))
    RCR_cons_minTS <- sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/
      (sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)))
    #RCR_cons_maxTS <- sum((ND$'S1*S3'>0.5)&(ND$'Outcome' <0.5))/
    # (sum((ND$'S1*s3'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5))+sum((ND$'S1*S3'>0.5)&(ND$'Outcome' <0.5)))
    RCR <- data.frame("Robustness_Case_Ratio"= c(RCR_typ_minTS, RCR_cons_minTS,RCC_Rank))#, RCR_typ_maxTS, RCR_cons_maxTS, SSrel_maxTS))
    RCR <- t(RCR)
    colnames(RCR) <- c("RCR_typ","RCR_dev","RCC_Rank")#,"RCR_typ_maxTS","RCR_dev_maxTS","SSR_maxTS")
    RCR[,c(1:2)] <- round(RCR[,c(1:2)], digits = 3)
    return(RCR)
  }

robust.intersections <- function(test_sol, initial_sol, sol_i = 1, use.tilde = TRUE, maxTS = FALSE)
{ 
  if ("list" %in% class(test_sol)){
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
  
  if ("list" %in% class(test_sol))
  {
    test_int <- s2
    for (i in length(test_sol))
    {
      test_int <- intersection(test_int, test_sol[[i]])
      test_int <- test_int[1]
    }
  }
  
  else {test_int <- s2}
  
  if ("list" %in% class(test_sol))
  {
      test_union <- rob.union(test_sol)
  }
  else {test_union <- s2}
  
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
    emp3 <- as.vector(unlist(sapply(test_union, function(x)  tild(x))))
    emp3 <- paste(emp3, collapse = "+")
    emp1 <- as.vector(unlist(sapply(s1, function(x)  tild(x))))
    emp1 <- paste(emp1, collapse = "+")}
  else {
    emp2 <- paste(toupper(test_int), collapse = "+")
    emp3 <- paste(toupper(test_union), collapse = "+")
    emp1 <- paste(toupper(s1), collapse = "+")}
  
  thintersect <- list()
  
  if(maxTS==FALSE){
  thintersect$S1S2 <- intersection(emp1,emp2)[[1]][1]
  thintersect$s1S2 <- intersection(negate(emp1)[[1]][1],emp2)[[1]][1]
  thintersect$S1s2 <- intersection(emp1,negate(emp2)[[1]][1])[[1]][1]
  thintersect$s1s2 <- intersection(negate(emp1)[[1]][1],negate(emp2)[[1]][1])[[1]][1]}
  else{
  thintersect$S1S2 <- intersection(emp1,simplify(emp3)[[1]][1])[[1]][1]
  thintersect$s1S2 <- intersection(negate(emp1)[[1]][1],simplify(emp3)[[1]][1])[[1]][1]
  thintersect$S1s2 <- intersection(emp1,negate(simplify(emp3)[[1]][1])[[1]][1])[[1]][1]
  thintersect$s1s2 <- intersection(negate(emp1)[[1]][1],negate(simplify(emp3)[[1]][1])[[1]][1])[[1]][1]}
  
  class(thintersect) <- 'robtersect'
  return(thintersect)
}

robust.rank<-function(test_sol, 
                      initial_sol, 
                      outcome)

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
  
  P <- data.frame(P1$solution_formula, P2$solution_formula, P3$solution_formula)
  
  rankis<-NA
  if(all(P[,1]==P[,2]) & all(P[,1]==P[,3])){rankis<-1}
  if(all(P[,1]==P[,2]) & all(P[,1]<P[,3])){rankis<-2}
  if(all(P[,1]<P[,2]) & all(P[,1]<P[,3])){rankis<-3}
  if(all(P[,1]>P[,2]) & (all(P[,1]>=P[,3]))){rankis<-4}
  if(all(P[,1]>P[,2]) & (all(P[,1]<P[,3]))){rankis<-5}
  if(all(P[,1]>P[,2]) & !(all(P[,1]<=P[,3])|all(P[,1]>=P[,3]))){rankis<-6}
  if(!(all(P[,1]<=P[,2])|all(P[,1]>=P[,2])) & all(P[,1]<P[,3])){rankis<-6}
  if(!(all(P[,1]<=P[,2])|all(P[,1]>-P[,2])) & !(all(P[,1]<=P[,3])|all(P[,1]>=P[,3]))){rankis<-7}
  return(rankis)
}

