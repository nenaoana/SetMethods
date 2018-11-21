# EXTRA ROBUSTNESS FUNCTIONS:

robustness.plot <- 
  function (results1, results2, outcome1, outcome2, 
            sol1 = 1, sol2 = 1, 
            case.lab = TRUE)
  {
    # OLD XY.PLOT:
    xy.plotold <-
      function(x, y,
               ylim = c(-.05, 1.05), xlim = c(-.05, 1.05),
               pch = 19, col = "black",
               main = "XY plot", ylab = "Outcome", xlab = "Condition",
               mar = c(4, 4, 4, 1), mgp = c(2.2, .8, 0),
               cex.fit = .6, cex.axis = .7, cex.main = 1,
               necessity = FALSE, 
               show.hv = TRUE, show.fit = TRUE, pos.fit = "top",
               case.lab = TRUE, labs = NULL, cex.lab = .8, 
               offset.x = 0, offset.y = 0, 
               pos = 4, srt = 0,
               ident = FALSE)
      {	
        
        par(mar = mar, mgp = mgp)
        plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
             axes = FALSE, pch = pch, main = main, cex.main = cex.main,
             col = col)
        axis(1, at = seq(0, 1, .1), labels = seq(0, 1, .1),
             cex.axis = cex.axis)
        axis(2, at = seq(0, 1, .1), labels = seq(0, 1, .1),
             cex.axis = cex.axis, las=2)
        box(); abline(0, 1); 
        
        if (show.hv == TRUE) {
          abline(v = .5, lty = 2); abline(h = .5, lty = 2)
        }
        
        if (necessity == TRUE) {   
          # Necessity
          con <- sum(pmin(x, y))/sum(y)
          cov <- sum(pmin(x, y))/sum(x)
          ron <- sum(1-x)/sum(1-pmin(x, y))
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
          
        } else {
          # Sufficiency
          con <- sum(pmin(x, y))/sum(x)
          cov <- sum(pmin(x, y))/sum(y)
          pri <- (sum(pmin(x,y))-sum(pmin(x,y,1-y)))/(sum(x)-sum(pmin(x,y,1-y)))
          hcon <- sum(pmin(x,y))/sum(pmin(x,y) + sqrt(pmax((x-y), 0)*x))
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
        
        if (show.fit == TRUE) {
          if (pos.fit == "top") {	
            mtext(lab, line = 0.3, cex = cex.fit)
          }
          if (pos.fit == "corner") {
            text(-.05, 1.05, cons, cex = cex.fit, adj = 0)
            text(1.05, -.05, cove, cex = cex.fit, adj = 1)
          }
        }
        
        if (case.lab == TRUE) {
          text(x + offset.x, y + offset.y, 
               labels = labs, cex = cex.lab, pos = pos, srt = srt)
        } 
        
        if (ident == TRUE) {
          id <- identify(x, y, labels = labs, cex = cex.lab)
        }
      }
    #########
    P1 <- pimdata(results = results1, outcome = outcome1,  sol = sol1)
    P2 <- pimdata(results = results2, outcome = outcome2, sol = sol2)
    fit12 <- QCAfit(P1$solution_formula, P2$solution_formula, necessity = FALSE)
    fit21 <- QCAfit(P2$solution_formula, P1$solution_formula, necessity = FALSE)
    nm1 <-deparse(substitute(results1))
    nm2 <-deparse(substitute(results2))
    
    if (fit12[1] > fit21[1]) {xy.plotold(P1$solution_formula, P2$solution_formula, 
                                         xlab = nm1, ylab = nm2, case.lab = case.lab, 
                                         labs = row.names(P1), main = "Robustness Plot")}
    else {xy.plotold(P2$solution_formula, P1$solution_formula, 
                     xlab = nm2, ylab = nm1, case.lab = case.lab, 
                     labs = row.names(P1), main = "Robustness Plot") }
  }

robustness.evaluation <-
  function(results1, 
           results2, 
           outcome1,
           outcome2,
           sol1=1,
           sol2=1)
  {		
    if (is.null(results1$i.sol)){
      s1 <- results1$solution[[sol1]]
      P1 <- results1$pims[colnames(results1$pims)%in%s1]}
    else{
      s1 <- results1$i.sol$C1P1$solution[[sol1]]
      P1 <- results1$i.sol$C1P1$pims[colnames(results1$i.sol$C1P1$pims)%in%s1]}
    
    if (is.null(results2$i.sol)){
      s2 <- results2$solution[[sol2]]
      P2 <- results2$pims[colnames(results2$pims)%in%s2]}
    else{
      s2 <- results2$i.sol$C1P1$solution[[sol2]]
      P2 <- results2$i.sol$C1P1$pims[colnames(results2$i.sol$C1P1$pims)%in%s2]}
    
    if (identical(P1,P2)) {  
      names(P1) <- paste("S1", names(P1), sep=".")
      names(P2) <- paste("S2", names(P2), sep=".")}
    
    P <- merge(P1, P2, by=0, all=TRUE)
    rownames(P) <- P[,1]
    P[,1] <- NULL
    
    P$Sol.Formula1 <- apply(P1, 1, max)
    P$Sol.Formula2 <- apply(P2, 1, max)
    P$'S1*S2' <- pmin( P$Sol.Formula1,   P$Sol.Formula2)
    P$'s1*S2' <- pmin(1-P$Sol.Formula1,   P$Sol.Formula2)
    P$'S1*s2' <- pmin(  P$Sol.Formula1, 1-P$Sol.Formula2)
    P$'s1*s2' <- pmin(1-P$Sol.Formula1, 1-P$Sol.Formula2)
    if (results1$options$neg.out) {
      P$Outcome1<-1-results1$tt$initial.data[,outcome1]
    } else {
      P$Outcome1<-results1$tt$initial.data[,outcome1]
    }
    
    if (results2$options$neg.out) {
      P$Outcome2<-1-results2$tt$initial.data[,outcome2]
    } else {
      P$Outcome2<-results2$tt$initial.data[,outcome2]
    }
    return(P)
  }

robustness.fit <-
  function(robust_eval)
  {            
    n_c <- ncol(robust_eval)-2
    rob_fit <- data.frame(matrix(NA, ncol=4, nrow=0))  
    for (i in (1:n_c)){
      rob_fit <- rbind(rob_fit, QCAfit(robust_eval[,i], robust_eval[, ncol(robust_eval)], necessity = FALSE))}
    rownames(rob_fit) <- names(robust_eval[1:n_c])
    return(rob_fit)
  }

cases.robustness <-
  function(results1, 
           results2, 
           outcome1,
           outcome2,
           sol1=1,
           sol2=1)
  {
    ND <- robustness.evaluation(results1=results1, 
                                results2=results2, 
                                outcome1=outcome1,
                                outcome2=outcome2,
                                sol1=sol1,
                                sol2=sol2)
    INT <- robust.intersections(results1=results1, 
                                 results2=results2,
                                 sol1=sol1,
                                 sol2=sol2)
    CTE <- list('s1S2y'=list('Intersection'='Deviant cases only for S2 (s1*S2 and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)]}),
                's1S2Y'=list('Intersection'='Covered by S2, but uncovered by S1 (s1*S2 and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)]}),
                'S1S2Y'=list('Intersection'='Covered by both S1 and S2 (S1*S2 and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5))/nrow(ND))*100, digits=2), "%"), 
                             'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)]}),
                'S1s2Y'=list('Intersection'='Covered by S1, but uncovered by S2 (S1*s2 and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)]}),
                's1s2Y'=list('Intersection'='Uncovered cases for both solutions (s1*s2 and Y > 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome1'>0.5)&(ND$'Outcome2'>0.5)]}),
                'S1S2y'=list('Intersection'='Deviant cases for both solutions (S1*S2 and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1S2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                            sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)),"/",nrow(ND),
                                            "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5))/nrow(ND))*100, digits=2), "%"), 
                             'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)]}),
                'S1s2y'=list('Intersection'='Deviant cases only S1 (S1*s2 and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$S1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)]}),
                's1s2y'=list('Intersection'='IIR cases for both S1 and S2 (s1*s2 and Y < 0.5)',
                             'Boolean'=paste('Boolean Expression:', INT$s1s2),
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)),"/", nrow(ND),
                                            "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5))/nrow(ND))*100, digits=2), "%"),
                             'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)) == 0) {'No cases in this intersection'} 
                             else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome1'<0.5)&(ND$'Outcome2'<0.5)]}))
    class(CTE) <- "casestheoryeval"
    return(CTE)
  }

robust.intersections <- function(results1, results2, sol1 = 1, sol2=1, use.tilde = TRUE)
{
  if (is.null(results1$i.sol)){
    if (is.character(sol1)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s1 <- results1$solution[[sol1]]}
  else{
    if (is.numeric(sol1)){
      s1 <- results1$i.sol$C1P1$solution[[sol1]]}
    else {
      if (is.character(sol1)){
        if (!nchar(sol1)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol1 <- toupper(sol1)  
        int1 <- as.numeric(unlist(strsplit(sol1, "I"))[2])
        mod1 <- toupper(unlist(strsplit(sol1, "I"))[1])
        if (int1 > length(get(mod1, pos = results1$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s1 <- get(mod1, pos = results1$i.sol)$solution[[int]]
      }
      else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
    }
  }
  
  if (is.null(results2$i.sol)){
    if (is.character(sol2)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s2 <- results2$solution[[sol2]]}
  else{
    if (is.numeric(sol2)){
      s2 <- results2$i.sol$C1P1$solution[[sol2]]}
    else {
      if (is.character(sol2)){
        if (!nchar(sol2)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol2 <- toupper(sol2)  
        int2 <- as.numeric(unlist(strsplit(sol2, "I"))[2])
        mod2 <- toupper(unlist(strsplit(sol2, "I"))[1])
        if (int2 > length(get(mod2, pos = results2$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s2 <- get(mod2, pos = results2$i.sol)$solution[[int2]]
      }
      else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
    }
  }
  tild <- function(x)
  {
    x <- unlist(strsplit(x, '\\*'))
    x <- as.vector(unlist(sapply(x, function (y) 
      if (!y==toupper(y)){y <- paste("~",toupper(y),sep="")} 
      else { y <- y})))
    x <- paste(x, collapse = "*")
  }
  
  if (!use.tilde){  
    emp1 <- as.vector(unlist(sapply(s1, function(x)  tild(x))))
    emp1 <- paste(emp1, collapse = "+")
    emp2 <- as.vector(unlist(sapply(s2, function(x)  tild(x))))
    emp2 <- paste(emp2, collapse = "+")}
  else {
    emp1 <- toupper(s1)
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
    cat("\nS1*S2:\n--------------------\n\n")
    print(x$S1S2)
    cat("\n~S1*S2:\n--------------------\n\n")
    print(x$s1S2)
    cat("\nS1*~S2:\n--------------------\n\n")
    print(x$S1s2)
    cat("\n~S1*~S2:\n--------------------\n\n")
    print(x$s1s2)
    cat('\n') }  

