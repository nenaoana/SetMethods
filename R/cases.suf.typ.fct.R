cases.suf.typ.fct <-
function(results,
		 outcome,
		 term=1,
		 neg.out=FALSE,
		 sol=1, use.tilde = TRUE)
    {if(length(grep("~",outcome)) > 0){
          outcome<-outcome[grep("~",outcome)]
          outcome<-gsub('\\~', '', outcome)
          outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    PD <- pimdata(results=results, outcome=outcome, sol=sol)
    nterm <- colnames(PD[term])
    DT <- results$tt$initial.data
    DT1 <- data.frame(matrix(NA,ncol=0,nrow=nrow(DT)))
    row.names(DT1)<-row.names(DT)
    tl <- gsub('\\s', '', nterm)
    tl <- strsplit(tl, '\\*')
    tn <- unique(unlist(tl))
    #Code for working with ~:
    if (use.tilde == TRUE) {
      t_neg<-character(0)
      t_pre<-character(0)
      
      if(length(grep("~",tn)) > 0){
        t_neg<-tn[grep("~",tn)]
        t_neg<-gsub('\\~', '', t_neg)
        t_neg<-unlist(t_neg)
        t_pre<-tn[!tn %in% tn[grep("~",tn)]]
      }
      else {t_pre<- toupper(tn)}
    }
    #Code for lower case:
    else{
    t_pre <- toupper(tn)[toupper(tn)==tn]
    t_neg <- toupper(tn)[tolower(tn)==tn]}
      
    if (length(t_pre) > 0) {
      DT1[t_pre] <- DT[t_pre]
      colnames(DT1[t_pre])<-toupper(colnames(DT1[t_pre]))
     }
     if (length(t_neg) > 0) {
      DT1[t_neg] <- 1 - DT[t_neg]
      colnames(DT1[t_neg])<-tolower(colnames(DT1[t_neg]))
    }
  
  if (!neg.out){
    Y <- DT[outcome]}
  else{
    Y <- 1-DT[outcome]}
    if (length(tn)==1) {
      fct <- paste("Typical Cases - Focal Conjunct", tn[1], sep = " ")
      X <-DT1[toupper(tn[1])]
      typical <-(X>0.5) & (Y>0.5) & (X<=Y)
      ty <- rownames(DT1)[typical]
        if (identical(ty, character(0))) {M[[i]] <-list(title=fct, results="no typical cases")}
        else {
          Z <- data.frame(
            x <- X[ty,toupper(tn[1])],
            y <- Y[ty,outcome],
            s=rep(FALSE))
          row.names(Z) <- ty
          s <- (abs(Z$y-Z$x) + (1-Z$x))
          suppressWarnings(Z$s[s==min(s)] <- TRUE)
          Z$St <- s
          colnames(Z)[1:3] <- c('Suff.Term/Focal Conjunct', outcome, 'most_typical')
          Z<-Z[, c( 1, 2, 4, 3)]
          Z <- Z[order(Z$St),]
          PDU <- as.data.frame(PD[ty,-c(ncol(PD), ncol(PD)-1, term)], row.names = ty)
          Z$uniquely_cov <- TRUE
          if (ncol(PDU)>1) {
            PDU <- apply(PDU, 1, function(x) sum(x>0.5))
            for (j in ty) {
            if (PDU[j]==0) {Z[j,"uniquely_cov"] <- TRUE}
              else {Z[j,"uniquely_cov"] <- FALSE}}}
          else { 
            if (ncol(PDU)==1) {
              for (j in ty) {
                if (PDU[j,]<=0.5) {Z[j,"uniquely_cov"] <- TRUE}
                else {Z[j,"uniquely_cov"] <- FALSE}}
              }
          }
          M <- list()
          M[[1]] <- list(title=fct, results=Z)
        }
      }
    else {
    M <- list()
    for (i in (1:length(tn)))
      {
      fct <- paste("Typical Cases - Focal Conjunct", tn[i], sep = " ")
      X <-DT1[toupper(tn[i])]
      cct<- tn[-grep(tn[i], tn)]
      cct<- toupper(cct)
      CCDT<-DT1[cct]
    if(ncol(CCDT)>1){
      a<-do.call(pmin, CCDT[,])
      CCDT1<-data.frame(a)
      row.names(CCDT1)<-row.names(CCDT)}
    else{
      CCDT1<-CCDT
      names(CCDT1)[1]<-"a"}
    
    CCDT$termm<-pmin(CCDT1$a,X[,])
    
    typical <-(CCDT$termm>0.5) & (Y>0.5) & (CCDT$termm<=Y)
    typ1 <- (X <= CCDT1$a)
    typ2 <- (X > CCDT1$a)
  
    ty <- rownames(DT1)[typical]
    ty1 <- rownames(DT1)[typical & typ1]
    ty2 <- rownames(DT1)[typical & typ2]
    
    if (identical(ty, character(0))) {M[[i]] <-list(title=fct, results="no typical cases")}
    else {
      Z <- data.frame(
        x <- X[ty,toupper(tn[i])],
        y <- Y[ty,outcome],
        cctm <- CCDT1[ty,"a"],
        termm <- CCDT[ty,"termm"],
        s=rep(FALSE))
      row.names(Z) <- ty
      s <- (abs(Z$y-Z$x) + (1-Z$term))
      suppressWarnings(Z$s[s==min(s)] <- TRUE)
      Z$St <- s
      colnames(Z)[1:5] <- c('Focal Conjunct', outcome, 'Comp. Conjunct','Term Membership', 'most_typical')
      Z<-Z[, c( 1, 3, 4, 2, 6, 5)]
      Z$Rank <- NA
      Z[ty1,7] <- 1
      Z[ty2,7] <- 2
      Z <- Z[order(Z$Rank, Z$St),]
      PDU <- PD[ty,-c(ncol(PD), ncol(PD)-1, term), drop = FALSE]
      PDU <- apply(PDU, 1, function(x) sum(x>0.5))
      Z$uniquely_cov <- TRUE
      for (j in ty) {
        if (PDU[j]==0) {Z[j,"uniquely_cov"] <- TRUE}
        else {Z[j,"uniquely_cov"] <- FALSE}}
      Z <- Z[c('Focal Conjunct', outcome, 'Comp. Conjunct','Term Membership', 'most_typical','uniquely_cov','Rank')]
      M[[i]] <- list(title=fct, results=Z)
    }
    }
    }
    class(M) <- 'matchessuf'
    return(M)}
