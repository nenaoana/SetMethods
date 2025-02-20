# Theory  Evaluation Helpers:
theory.data <-
  function(theory, 
           empirics, 
           outcome,
           sol=1, 
           use.tilde = TRUE)
  { if(length(grep("~",outcome)) > 0){
    outcome<-outcome[grep("~",outcome)]
    outcome<-gsub('\\~', '', outcome)
    outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    
    
    # TRANSFORM TO TILDE --------
    tild <- function(x)
    {
      x <- unlist(strsplit(x, '\\*'))
      x <- as.vector(unlist(sapply(x, function (y) 
        if (!y==toupper(y)){y <- paste("~",toupper(y),sep="")} 
        else { y <- y})))
      x <- paste(x, collapse = "*")
    }
    # ------------
    
    if (!use.tilde){
      th <- unlist(strsplit(theory, '\\+'))
      th <- as.vector(unlist(sapply(th, function(x)  tild(x))))
      theory <- paste(th, collapse = "+")}
    else {theory <- toupper(theory)}
    
    tl <- gsub('\\s', '', theory)
    tl <- unlist(strsplit(tl, '\\+')) 
    tl <- strsplit(tl, '\\*') 
    tn <- unique(unlist(tl))
    t_neg<-character(0)
    t_pre<-character(0)
    if(length(grep("~",tn)) > 0){
      t_neg<-tn[grep("~",tn)]
      t_neg<-gsub('\\~', '', t_neg)
      t_neg<-unlist(t_neg)
      t_pre<-tn[!tn %in% tn[grep("~",tn)]]
    }
    else {t_pre<- toupper(tn)}
    
    if (length(t_pre) > 0) {
      PRE <- empirics$tt$initial.data[t_pre] ; names(PRE) <- t_pre      
    }
    if (length(t_neg) > 0) {
      NEG <- 1 - empirics$tt$initial.data[t_neg] ; names(NEG) <- paste("~", t_neg, sep="") 
    }
    
    if ((length(t_pre)>0)&(length(t_neg)>0)){
      ALL <- cbind(PRE, NEG)	
    } else if ((length(t_pre)>0)&(length(t_neg)==0)){
      ALL <- PRE
    } else if ((length(t_pre)==0)&(length(t_neg)>0)){
      ALL <- NEG
    } else if ((length(t_pre)==0)&(length(t_neg)==0)){
      stop('Missing theory.\n')	
    }
    
    THEORY <- as.data.frame(matrix(nrow=nrow(empirics$tt$initial.data), ncol=length(tl)))
    
    for (j in 1:length(tl)) {
      if (length(tl[[j]])>1){
        THEORY[, j] <- apply(ALL[, tl[[j]]], 1, min)
      }
      else {THEORY[, j] <- ALL[, tl[[j]]] }
    }
    
    tv <- apply(THEORY, 1, max)
    
    if (is.null(empirics$i.sol)){
      if (is.character(sol)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
      s <- empirics$solution[[sol]]
      P <- empirics$pims[colnames(empirics$pims)%in%s]}
    else{
      if (is.numeric(sol)){
        s <- empirics$i.sol$C1P1$solution[[sol]]
        P <- empirics$i.sol$C1P1$pims[colnames(empirics$i.sol$C1P1$pims)%in%s]}
      else {
        if (is.character(sol)){
          if (!nchar(sol)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
          sol <- toupper(sol)  
          int <- as.numeric(unlist(strsplit(sol, "I"))[2])
          mod <- toupper(unlist(strsplit(sol, "I"))[1])
          if (int > length(get(mod, pos = empirics$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
          s <- get(mod, pos = empirics$i.sol)$solution[[int]]
          P <- get(mod, pos = empirics$i.sol)$pims[colnames(get(mod, pos = empirics$i.sol)$pims)%in%s]  
        }
        else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
      }
    }
    
    if (!use.tilde){
      colnames(P) <- as.vector(unlist(sapply(colnames(P), function(x)  tild(x))))}
    
    P$Sol.Formula <- apply(P, 1, max)
    P$Theory <- as.numeric(tv)
    P$'T*S' <- pmin(  as.numeric(tv),   P$Sol.Formula)
    P$'~T*S' <- pmin(1-as.numeric(tv),   P$Sol.Formula)
    P$'T*~S' <- pmin(  as.numeric(tv), 1-P$Sol.Formula)
    P$'~T*~S' <- pmin(1-as.numeric(tv), 1-P$Sol.Formula)
    if (empirics$options$neg.out) {
      P$Outcome<-empirics$tt$recoded.data[,outcome]
    } else {
      P$Outcome<-empirics$tt$recoded.data[,outcome]
    }
    return(P)
  }
cases.theory.evaluation <-
  
  function(theory, 
           
           empirics, 
           
           outcome,
           
           sol=1, 
           
           use.tilde = TRUE)
    
  {
    
    ND <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = use.tilde)
    
    INT <- theory.intersections(theory=theory, empirics=empirics, sol=sol, use.tilde = use.tilde)
    
    CTE <- list('TSY'=list('Intersection'='Covered Most Likely (T*S and Y > 0.5)',
                           
                           'Boolean'=paste('Boolean Expression:', INT$TE),
                           
                           'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                          
                                          sum((ND$'T*S'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),
                                          
                                          "=", round((sum((ND$'T*S'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                           
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                           
                                           sum((ND$'T*S'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                           
                                           "=", round((sum((ND$'T*S'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                           
                           'CaseNames'= if(sum((ND$'T*S'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                           
                           else {rownames(ND)[(ND$'T*S'>0.5)&(ND$'Outcome'>0.5)]}),
                
                '~TSY'=list('Intersection'='Covered Least Likely (~T*S and Y > 0.5)',
                            
                            'Boolean'=paste('Boolean Expression:', INT$tE),
                            
                            'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                           
                                           sum((ND$'~T*S'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),
                                           
                                           "=", round((sum((ND$'~T*S'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                            
                            'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                            
                                            sum((ND$'~T*S'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                            
                                            "=", round((sum((ND$'~T*S'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                            
                            'CaseNames'= if(sum((ND$'~T*S'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                            
                            else {rownames(ND)[(ND$'~T*S'>0.5)&(ND$'Outcome'>0.5)]}),
                
                'T~SY'=list('Intersection'='Uncovered Most Likely (T*~S and Y > 0.5)',
                            
                            'Boolean'=paste('Boolean Expression:', INT$Te),
                            
                            'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                           
                                           sum((ND$'T*~S'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),
                                           
                                           "=", round((sum((ND$'T*~S'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                            
                            'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                            
                                            sum((ND$'T*~S'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),
                                            
                                            "=", round((sum((ND$'T*~S'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                            
                            'CaseNames'= if(sum((ND$'T*~S'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                            
                            else {rownames(ND)[(ND$'T*~S'>0.5)&(ND$'Outcome'>0.5)]}),
                
                '~T~SY'=list('Intersection'='Uncovered Least Likely (~T*~S and Y > 0.5)',
                             
                             'Boolean'=paste('Boolean Expression:', INT$te),
                             
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            
                                            sum((ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),
                                            
                                            "=", round((sum((ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                             
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                             
                                             sum((ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),
                                             
                                             "=", round((sum((ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                             
                             'CaseNames'= if(sum((ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                             
                             else {rownames(ND)[(ND$'~T*~S'>0.5)&(ND$'Outcome'>0.5)]}),
                
                'TS~Y'=list('Intersection'='Inconsistent Most Likely (T*S and Y < 0.5)',
                            
                            'Boolean'=paste('Boolean Expression:', INT$TE),
                            
                            'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                           
                                           sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                           
                                           "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                            
                            'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                            
                                            sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                            
                                            "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                            
                            'CaseNames'= if(sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                            
                            else {rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'<0.5)]}),
                
                '~TS~Y'=list('Intersection'='Inconsistent Least Likely (~T*S and Y < 0.5)',
                             
                             'Boolean'=paste('Boolean Expression:', INT$tE),
                             
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            
                                            sum((ND$'~T*S'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                            
                                            "=", round((sum((ND$'~T*S'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                             
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                             
                                             sum((ND$'~T*S'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                             
                                             "=", round((sum((ND$'~T*S'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                             
                             'CaseNames'= if(sum((ND$'~T*S'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                             
                             else {rownames(ND)[(ND$'~T*S'>0.5)&(ND$'Outcome'<0.5)]}),
                
                'T~S~Y'=list('Intersection'='Consistent Most Likely (T*~S and Y < 0.5)',
                             
                             'Boolean'=paste('Boolean Expression:', INT$Te),
                             
                             'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                            
                                            sum((ND$'T*~S'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                            
                                            "=", round((sum((ND$'T*~S'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                             
                             'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                             
                                             sum((ND$'T*~S'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                             
                                             "=", round((sum((ND$'T*~S'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                             
                             'CaseNames'= if(sum((ND$'T*~S'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                             
                             else {rownames(ND)[(ND$'T*~S'>0.5)&(ND$'Outcome'<0.5)]}),
                
                '~T~S~Y'=list('Intersection'='Consistent Least Likely (~T*~S and Y < 0.5)',
                              
                              'Boolean'=paste('Boolean Expression:', INT$te),
                              
                              'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                             
                                             sum((ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                             
                                             "=", round((sum((ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                              
                              'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                              
                                              sum((ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                              
                                              "=", round((sum((ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                              
                              'CaseNames'= if(sum((ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                              
                              else {rownames(ND)[(ND$'~T*~S'>0.5)&(ND$'Outcome'<0.5)]}))
    
    class(CTE) <- 'casestheoryeval'
    
    return(CTE)
    
  }

theory.fit <-
  function(theory_data, consH = FALSE)
  {            
    n_c <- ncol(theory_data)-1
    if(consH == FALSE){
    theory_fit <- data.frame(matrix(NA, ncol=3, nrow=0))
    names(theory_fit) < c("Cons.Suf", "Cov.Suf", "PRI")
    }
    else{theory_fit <- data.frame(matrix(NA, ncol=4, nrow=0))
    names(theory_fit) < c("Cons.Suf", "Cov.Suf", "PRI","Cons.Suf(H)")
    }
    for (i in (1:n_c)){
      theory_fit <- rbind(theory_fit, data.frame(as.list(QCAfit(theory_data[,i], theory_data[, ncol(theory_data)], necessity = FALSE, consH = consH))))}
    rownames(theory_fit) <- names(theory_data[1:n_c])
    if(consH == FALSE){
      names(theory_fit) <- c("Cons.Suf","Cov.Suf","PRI")}
    else{names(theory_fit) <- c("Cons.Suf","Cov.Suf","PRI","Cons.Suf(H)")}
    return(theory_fit)
  }

theory.intersections <- function(theory, empirics, sol = 1, use.tilde = TRUE)
{
  if (is.null(empirics$i.sol)){
    if (is.character(sol)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s <- empirics$solution[[sol]]}
  else{
    if (is.numeric(sol)){
      s <- empirics$i.sol$C1P1$solution[[sol]]}
    else {
      if (is.character(sol)){
        if (!nchar(sol)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol <- toupper(sol)  
        int <- as.numeric(unlist(strsplit(sol, "I"))[2])
        mod <- toupper(unlist(strsplit(sol, "I"))[1])
        if (int > length(get(mod, pos = empirics$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s <- get(mod, pos = empirics$i.sol)$solution[[int]]
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
    emp <- as.vector(unlist(sapply(s, function(x)  tild(x))))
    emp <- paste(emp, collapse = "+")
    th <- unlist(strsplit(theory, '\\+'))
    th <- as.vector(unlist(sapply(th, function(x)  tild(x))))
    theory <- paste(th, collapse = "+")}
  else {
    emp <- paste(toupper(s), collapse = "+")
    theory <- paste(toupper(theory), collapse = "+")}
  
  thintersect <- list()
  
  n_c_l <- length(names(empirics$tt$tt))-5
  lab_conds <- names(empirics$tt$tt[,1:n_c_l])
  
  thintersect$TE <- as.character(intersection(theory,emp))
  thintersect$tE <- as.character(intersection(negate(theory, snames=lab_conds, simplify=FALSE)[[1]][1],emp))
  thintersect$Te <- as.character(intersection(theory,negate(emp, snames=lab_conds, simplify=FALSE)[[1]][1]))
  thintersect$te <- as.character(intersection(negate(theory, snames=lab_conds, simplify=FALSE),negate(emp, snames=lab_conds, simplify=FALSE)[[1]][1]))
  
  class(thintersect) <- 'thintersect'
  return(thintersect)
}
