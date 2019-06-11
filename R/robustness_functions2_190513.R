###
# New robustness functions:
###

core.fit <- 
  function(all_sols, outcome)
  {
    PD <- pimdata(results = all_sols[[1]], outcome = outcome)
    SF <- PD[,"solution_formula", drop=FALSE]
    SF$out <- PD$out
    for (i in length(all_sols))
    {
      PDi <- pimdata(results = all_sols[[i]], outcome = outcome)
      SF$solution_formula <- pmin(PDi$solution_formula, SF$solution_formula)
    }
    
    core_fit <- QCAfit(x = SF$solution_formula, SF$out, necessity = FALSE, cond.lab = "Core.Fit")
    return(core_fit)
  }

robust.fit <- 
  function(all_sols, initial_sol, outcome)
  {
    rcf <- core.fit(all_sols, outcome)
    isf <- QCAfit(initial_sol, outcome, necessity = FALSE)
    rob_cons <- isf["solution_formula",1]/rcf[1]
    rob_cov <- rcf[2]/isf["solution_formula",2]
    rob <- data.frame("Fit Robustness"= c(rob_cov, rob_cons))
    row.names(rob) <- c("Coverage","Consistency")
    return(rob)
  }

xy.plot2 <-
  function(x, y, data, 
           labcol = "black",
           main = "XY plot", 
           ylab = "Outcome", 
           xlab = "Condition",
           necessity = FALSE,
           jitter = FALSE, 
           font = "sans",
           fontface = "italic", 
           fontsize = 3,
           labs = rownames(data),
           shape = 19)
  { if (is.data.frame(x)){ x <- colnames(x)}
    else{
      if(length(grep("~",x)) > 0){
        x<-gsub('\\~', '', x)
        x<-unlist(x)
        data[,x] <- 1-data[,x]}}
    if (is.data.frame(y)){ y <- colnames(y)}
    else{
      if(length(grep("~",y)) > 0){
        y<-gsub('\\~', '', y)
        y<-unlist(y)
        data[,y] <- 1-data[,y]}}
    if (necessity == TRUE) {   
      # Necessity
      con <- sum(pmin(data[,x], data[,y]))/sum(data[,y])
      cov <- sum(pmin(data[,x], data[,y]))/sum(data[,x])
      ron <- sum(1-data[,x])/sum(1-pmin(data[,x], data[,y]))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      rons <- format(ron, digits = 3)
      storage.mode(rons) <- "numeric"
      lab <- sprintf("Cons.Nec: %.3f; Cov.Nec: %.3f; RoN: %.3f", con, cov, ron)
      cons.c <- paste("Cons.Nec",
                      cons, sep = ": ")
      cove.c <- paste("Cov.Nec",
                      cove, sep = ": ")
      rons.c<- paste("RoN",
                     rons, sep = ": ")
      
    } 
    else {
      # Sufficiency
      con <- sum(pmin(data[,x], data[,y]))/sum(data[,x])
      cov <- sum(pmin(data[,x], data[,y]))/sum(data[,y])
      pri <- (sum(pmin(data[,x], data[,y]))-sum(pmin(data[,x], data[,y],1-data[,y])))/(sum(data[,x])-sum(pmin(data[,x], data[,y],1-data[,y])))
      hcon <- sum(pmin(data[,x], data[,y]))/sum(pmin(data[,x], data[,y]) + sqrt(pmax((data[,x]-data[,y]), 0)*data[,x]))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      pris <- format(pri, digits = 3)
      storage.mode(pris) <- "numeric"
      hcons <- format(hcon, digits = 3)
      storage.mode(hcons) <- "numeric"
      lab <- sprintf("Cons.Suf: %.3f; Cov.Suf: %.3f; PRI: %.3f; Cons.Suf(H): %.3f", con, cov, pri, hcon)
      cons.c <- paste("Cons.Suf",
                      cons, sep = ": ")
      cove.c <- paste("Cov.Suf",
                      cove, sep = ": ")
      pris.c<- paste("PRI",
                     pris, sep = ": ")
      hcons.c<- paste("Cons.Suf(H)",
                      hcons, sep = ": ")
    }
    if (jitter == TRUE) {
      ggplot(data) +
        geom_point(aes(data[,x], data[,y]), color = 'black', shape = shape) +
        geom_text_repel(aes(data[,x], data[,y], label = labs),
                        size = fontsize,
                        family = font,
                        fontface = fontface,
                        segment.size = 0.04,
                        force = 0.07,
                        max.iter = 2e3) +
        xlim(0,1)+
        ylim(0,1)+
        theme_classic(base_size = 16) +
        geom_vline(xintercept = 0.5)+
        geom_vline(xintercept = 1)+
        
        geom_hline(yintercept = 0.5)+
        geom_hline(yintercept = 1)+
        
        geom_abline(intercept = 0)+
        labs(title = main, subtitle = lab , x=xlab,y=ylab)+
        theme(plot.title = element_text(family = "Palatino", color="#666666", face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(family = "Palatino", color="#666666", face="bold", size=12))+
        theme(plot.subtitle = element_text(family = "Palatino", color="#666666", face="bold", size=10)) 
    }
    else {
      ggplot(data) +
        geom_point(aes(data[,x], data[,y]), color = 'black', shape = shape) +
        geom_text(aes(data[,x] , data[,y],label=labs),hjust=0, vjust=0, color = labcol, fontface = fontface, size = fontsize, family = font)+
        xlim(0,1)+
        ylim(0,1)+
        theme_classic(base_size = 16) +
        geom_vline(xintercept = 0.5)+
        geom_vline(xintercept = 1)+
        
        geom_hline(yintercept = 0.5)+
        geom_hline(yintercept = 1)+
        
        geom_abline(intercept = 0)+
        labs(title = main, subtitle = lab , x=xlab,y=ylab)+
        theme(plot.title = element_text(family = "Palatino", color="#666666", face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(family = "Palatino", color="#666666", face="bold", size=12))+
        theme(plot.subtitle = element_text(family = "Palatino", color="#666666", face="bold", size=10)) 
      
    }
  }

robustness.plot2 <- 
  function (test_sol, 
            initial_sol, 
            outcome,
            jitter = TRUE,
            fontsize = 3,
            labs = TRUE
            
  )
  {
    if (class(test_sol) == "list")
    {
      P1 <- pimdata(results = test_sol[[1]], outcome = outcome)
      for (i in length(test_sol))
      {
        Pi <- pimdata(results = test_sol[[i]], outcome = outcome)
        P1$solution_formula <- pmin(Pi$solution_formula, P1$solution_formula)
      }
    }
    else {
      P1 <- pimdata(results = test_sol, outcome = outcome)
    }
    P2 <- pimdata(results = initial_sol, outcome = outcome)
    fit12 <- QCAfit(P1$solution_formula, P2$solution_formula, necessity = FALSE)
    fit21 <- QCAfit(P2$solution_formula, P1$solution_formula, necessity = FALSE)
    #nm1 <-deparse(substitute(results1))
    m2 <-deparse(substitute(initial_sol))
    PS <- data.frame(P1$solution_formula, P2$solution_formula, P1$out)
    names(PS) <- c("sol1", "sol2", "out")
    rownames(PS) <- rownames(P1)
    
    
    if (fit12[1] > fit21[1]) {xy.plot2("sol1", "sol2",
                                       data = PS,
                                       xlab = "initial_sol", ylab = "test_sol", main = "Robustness Plot",
                                       shape = ifelse(PS$out >= 0.5, 19, 10), jitter=jitter, fontsize = fontsize)}
    else {xy.plot2("sol2","sol1",
                   data = PS,
                   xlab = "test_sol", ylab = "initial_sol",
                   main = "Robustness Plot", shape = ifelse(PS$out >= 0.5, 19, 10), jitter=jitter, fontsize = fontsize)}
    
  }

robustness.evaluation2 <-
  function(test_sol, 
           initial_sol, 
           outcome)
  {		
    if (class(test_sol) == "list")
    {
      P1 <- pimdata(results = test_sol[[1]], outcome = outcome)
      for (i in length(test_sol))
      {
        Pi <- pimdata(results = test_sol[[i]], outcome = outcome)
        P1$solution_formula <- pmin(Pi$solution_formula, P1$solution_formula)
      }
    }
    else {
      P1 <- pimdata(results = test_sol, outcome = outcome)
    }
    P2 <- pimdata(results = initial_sol, outcome = outcome)
    
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

robustness.fit2 <-
  function(robust_eval)
  {            
    n_c <- ncol(robust_eval)-1
    rob_fit <- data.frame(matrix(NA, ncol=4, nrow=0))  
    for (i in (1:n_c)){
      rob_fit <- rbind(rob_fit, QCAfit(robust_eval[,i], robust_eval[, ncol(robust_eval)], necessity = FALSE))}
    rownames(rob_fit) <- names(robust_eval[1:n_c])
    return(rob_fit)
  }

cases.robustness2 <-
  function(test_sol, 
           initial_sol, 
           outcome,
           use.tilde=TRUE)
  {
    ND <- robustness.evaluation2(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    INT <- robust.intersections2(test_sol = test_sol, 
                                initial_sol = initial_sol,
                                sol_i = 1, 
                                use.tilde = use.tilde)
    CTE <- list('s1S2y'=list('Intersection'='Deviant cases for TS (is*TS and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
                's1S2Y'=list('Intersection'='Covered by TS, but uncovered by IS (is*TS and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
                'S1S2Y'=list('Intersection'='Covered by both TS and IS (IS*TS and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"), 
                             'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
                'S1s2Y'=list('Intersection'='Covered by IS, but uncovered by TS (IS*ts and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
                's1s2Y'=list('Intersection'='Uncovered cases for both solutions (is*ts and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
                'S1S2y'=list('Intersection'='Deviant cases for both solutions (IS*TS and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"), 
                             'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
                'S1s2y'=list('Intersection'='Deviant cases only IS (IS*ts and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)]}),
                's1s2y'=list('Intersection'='IIR cases for both IS and TS (is*ts and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)]}))
    class(CTE) <- "casestheoryeval"
    return(CTE)
  }

robust.intersections2 <- function(test_sol, initial_sol, sol_i = 1, use.tilde = TRUE)
{ 
  if (class(test_sol) == "list"){
    results1 = test_sol[[1]]
  }
  else results1 = test_sol
  
  if (is.null(results1$i.sol)){
    s1 <- results1$solution[[1]]}
  else{
    s1 <- results1$i.sol$C1P1$solution[[1]]}
  
  results2 = initial_sol
  if (is.null(results2$i.sol)){
    if (is.character(sol_i)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s2 <- results2$solution[[sol_i]]}
  else{
    if (is.numeric(sol_i)){
      s2 <- results2$i.sol$C1P1$solution[[sol_i]]}
    else {
      if (is.character(sol_i)){
        if (!nchar(sol_i)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol_i <- toupper(sol_i)  
        int2 <- as.numeric(unlist(strsplit(sol_i, "I"))[2])
        mod2 <- toupper(unlist(strsplit(sol_i, "I"))[1])
        if (int2 > length(get(mod2, pos = results2$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s2 <- get(mod2, pos = results2$i.sol)$solution[[int2]]
      }
      else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
    }
  }
  
  if (class(test_sol) == "list")
  {
    test_int <- s1
    for (i in length(test_sol))
    {
      test_int <- intersection(test_int, test_sol[[i]])
      test_int <- test_int[1]
    }
  }
  
  else {test_int <- s1}
  
  tild <- function(x)
  {
    x <- unlist(strsplit(x, '\\*'))
    x <- as.vector(unlist(sapply(x, function (y) 
      if (!y==toupper(y)){y <- paste("~",toupper(y),sep="")} 
      else { y <- y})))
    x <- paste(x, collapse = "*")
  }
  
  if (!use.tilde){  
    emp1 <- as.vector(unlist(sapply(test_int, function(x)  tild(x))))
    emp1 <- paste(emp1, collapse = "+")
    emp2 <- as.vector(unlist(sapply(s2, function(x)  tild(x))))
    emp2 <- paste(emp2, collapse = "+")}
  else {
    emp1 <- toupper(test_int)
    emp2 <- toupper(s2)}
  
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
    cat("\nTS*IS:\n--------------------\n\n")
    print(x$S1S2)
    cat("\n~TS*IS:\n--------------------\n\n")
    print(x$s1S2)
    cat("\nTS*~IS:\n--------------------\n\n")
    print(x$S1s2)
    cat("\n~TS*~IS:\n--------------------\n\n")
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
    cat(c("Raw Consistency T.: ","Lower bound ", incl.cut.tl+0.01, "Threshold ", incl.cut , "Upper bound ", incl.cut.tu - 0.01, "\n"))
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
    cat(c("N.Cut: ","Lower bound ", n.cut.tl+1, "Threshold ", n.cut , "Upper bound ", n.cut.tu - 1, "\n"))
    invisible(TH)
  }  




