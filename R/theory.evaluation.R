theory.evaluation <-
function(theory, 
           empirics, 
           outcome,
           intermed=FALSE,
           sol=1)
  {		
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
    else { t_pre<- toupper(tn) }
    
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
    
    if (!intermed){
      s <- empirics$solution[[sol]]
      P <- empirics$pims[colnames(empirics$pims)%in%s]}
    else{
      s <- empirics$i.sol$C1P1$solution[[sol]]
      P <- empirics$i.sol$C1P1$pims[colnames(empirics$i.sol$C1P1$pims)%in%s]}
    
    P$Sol.Formula <- apply(P, 1, max)
    P$Theory <- tv
    P$'T*E' <- pmin(  tv,   P$Sol.Formula)
    P$'t*E' <- pmin(1-tv,   P$Sol.Formula)
    P$'T*e' <- pmin(  tv, 1-P$Sol.Formula)
    P$'t*e' <- pmin(1-tv, 1-P$Sol.Formula)
    if (empirics$options$neg.out) {
      P$Outcome<-1-empirics$tt$initial.data[,outcome]
    } else {
      P$Outcome<-empirics$tt$initial.data[,outcome]
    }
    return(P)
  }
