# Single Case Selection Helpers:

# DCN
cases.suf.dcn <-
  function(results,
           outcome,
           sol=1,
           ...)
  { 
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    aux <-
      function(i)
      {
        fil <- (X[,i] > 0.5) & (y < 0.5) 
        Z <- data.frame(x=X[fil, i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        Term=rep(colnames(X)[i], sum(fil)),
                        Cases=rownames(X)[fil])
        s <- (1 - (Z$x-Z$y) + (1-Z$x))
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$Sd <- s 
        colnames(Z)[1:3] <- c('TermMembership', outcome, 'MostDevCons')
        Z<-Z[, c(5, 4, 1, 2, 6, 3)]
        Z[,c(3:5)] <- round(Z[,c(3:5)], digits = 3)
        return(Z[order(Z$Sd),])
      }
    R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
    R <- R[order(R$Term,R$Sd,R$TermMembership),]
    names(R)[names(R)==outcome]<- "Outcome"
    names(R)[names(R)=="Sd"]<- "Best"
    M<-list()
    M[[1]] <- list(title="Deviant Consistency Cases", results=R[R$Term!='solution_formula', ])
    class(M) <- 'matchessuf'
    return(M)
  }
# DCV
cases.suf.dcv <-
  function(results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    CS <- results$tt$recoded.data
    CS <- CS[, -which(colnames(CS)==outcome)]
    TS <- CS
    TS[TS<0.50]<-1-TS[TS<0.50]
    CS[CS<0.50]<-0
    CS[CS>0.50]<-1
    CS["TT_row_membership"]<-do.call(pmin,TS)
    CS["TT_row_membership"] <- round(CS["TT_row_membership"], digits = 3)
    aux <-
      function(i)
      {
        fil <- (X[,i] < 0.5) & (y > 0.5) 
        Z <- data.frame(x=X[fil, i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        Term=rep(colnames(X)[i], sum(fil)),
                        Case=rownames(X)[fil], ttr=CS[rownames(X)[fil],"TT_row_membership"])
        #s <- abs(Z$ttr-Z$y) + (1-Z$ttr)
        s <- (1-Z$ttr)
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$Sd <- s 
        colnames(Z)[1:3] <- c('TermMembership', outcome, 'Most_deviant')
        return(Z[, c(5, 4, 1, 2, 7, 3)])
      }
    R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
    R <- R[R$Term=='solution_formula', c('Case', 'TermMembership', outcome,"Sd")]
    names(R)[2] <- 'SolMembership'
    R[,2:4] <- round(R[,2:4], digits = 3)
    Z <- merge(x=R, y=CS, by.x='Case', by.y='row.names')
    names(Z)[5:(ncol(Z)-1)] <- paste('TT_', names(Z)[5:(ncol(Z)-1)], sep='')
    O <-subset(Z,select=3)
    Z <-Z[,-c(3)]
    Z$Outcome <- O
    sortnames<-names(Z)[4:(ncol(Z)-2)]
    Z <- Z[do.call("order", c(Z[sortnames], Z["Sd"])), ]
    names(Z$Outcome)<- "Outcome"
    names(Z)[names(Z)=="Sd"]<- "Best"
    ttsplit <- aggregate(Z$Best,by=Z[sortnames],min, drop=FALSE)
    Z$MostDevCov <- FALSE
    for (n in 1:nrow(Z)){
      for (s in 1:nrow(ttsplit)){
        if(all(ttsplit[s,sortnames] == Z[n,sortnames]) & ttsplit[s,"x"] == Z[n, "Best"]){Z[n,"MostDevCov"] <- TRUE}
      }}
    Z$ConsTT <- FALSE
    for (n in 1:nrow(Z)){
      if(Z[n,"TT_row_membership"] <= Z[n,"Outcome"]){Z[n,"ConsTT"] <- TRUE}
    }
    Z <- Z[do.call("order", c(Z[sortnames], 1-Z["ConsTT"], Z["Best"])), ]
    Z <- cbind(Z[,c(1,2)], Z[sortnames],Z["TT_row_membership"], Z["Outcome"],Z["Best"], Z["MostDevCov"],Z["ConsTT"])
    M <- list()
    M[[1]] <- list(title="Deviant Coverage Cases", results=Z)
    class(M) <- 'matchessuf'
    return(M)
  }

# IIR
cases.suf.iir <-
  function(results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    CS <- results$tt$recoded.data
    CS <- CS[, -which(colnames(CS)==outcome)]
    TS <- CS
    TS[TS<0.50]<-1-TS[TS<0.50]
    CS[CS<0.50]<-0
    CS[CS>0.50]<-1
    CS["TT_row_membership"]<-do.call(pmin,TS)
    CS["TT_row_membership"] <- round(CS["TT_row_membership"], digits = 3)
    aux <-
      function(i)
      {
        fil <- (X[,i] < 0.5) & (y < 0.5) 
        Z <- data.frame(x=X[fil, i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        term=rep(colnames(X)[i], sum(fil)),
                        Case=rownames(X)[fil])
        s <- 1 - (Z$x-Z$y)/Z$x
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$Sd <- s 
        colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
        return(Z[, c(5, 4, 1, 2, 6, 3)])
      }
    R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
    R <- R[R$term=='solution_formula', c('Case', 'term_membership', outcome)]
    names(R)[2] <- 'Solution_membership'
    R[,c(2:3)] <- round(R[,c(2:3)], digits = 3)
    Z <- merge(x=R, y=CS, by.x='Case', by.y='row.names')
    names(Z)[4:(ncol(Z)-1)] <- paste('TT_', names(Z)[4:(ncol(Z)-1)], sep='')
    O <-subset(Z,select=3)
    Z <-Z[,-c(3)]
    Z$Outcome <- O
    sortnames<-names(Z)[3:(ncol(Z)-2)]
    Z <- Z[do.call("order", Z[sortnames]), ]
    names(Z$Outcome)<- "Outcome"
    M <- list()
    M[[1]] <- list(title="Individually Irrelevant Cases", results=Z)
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP FC
cases.suf.typ.fct <-
  function(results,
           outcome,
           term=1,
           sol=1,
           max_pairs=5,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    PD <- pimdata(results=results, outcome=outcome, sol=sol)
    if (term>(ncol(PD)-2)){stop("The term selected does not exist for the chosen model of the solution. Check the solution again and pick another term or change the model using the argument sol.")}
    nterm <- colnames(PD[term])
    DT <- results$tt$initial.data
    DT1 <- data.frame(matrix(NA,ncol=0,nrow=nrow(DT)))
    row.names(DT1)<-row.names(DT)
    tl <- gsub('\\s', '', nterm)
    tl <- strsplit(tl, '\\*')
    tn <- unique(unlist(tl))
    #Code for working with ~:
    #if (results$options$use.tilde == TRUE) {
      t_neg<-character(0)
      t_pre<-character(0)
      
      if(length(grep("~",tn)) > 0){
        t_neg<-tn[grep("~",tn)]
        t_neg<-gsub('\\~', '', t_neg)
        t_neg<-unlist(t_neg)
        t_pre<-tn[!tn %in% tn[grep("~",tn)]]
      }
      else {t_pre<- toupper(tn)}
    #}
    # #Code for lower case:
    # else{
    #   t_pre <- toupper(tn)[toupper(tn)==tn]
    #   t_neg <- toupper(tn)[tolower(tn)==tn]}
    
    if (length(t_pre) > 0) {
      DT1[t_pre] <- DT[t_pre]
      colnames(DT1[t_pre])<-toupper(colnames(DT1[t_pre]))
    }
    if (length(t_neg) > 0) {
      DT1[t_neg] <- 1 - DT[t_neg]
      colnames(DT1[t_neg])<-tolower(colnames(DT1[t_neg]))
    }
    
    Y <- PD[,"out", drop=FALSE]
    names(Y) <- outcome
    # For terms with a single condition:
    if (length(tn)==1) {
      fct <- paste("Typical Cases - Focal Conjunct", tn[1], sep = " ")
      X <-DT1[toupper(tn[1])]
      typical <-(X>0.5) & (Y>0.5) & (X<=Y)
      ty <- rownames(DT1)[typical]
      consfc <-(X<=Y)
      cfc <- rownames(DT1)[consfc]
      
      if (identical(ty, character(0))) {M[[i]] <-list(title=fct, results="no typical cases")}
      else {
        Z <- data.frame(
          x <- X[ty,toupper(tn[1])],
          y <- Y[ty,outcome],
          s=rep(FALSE))
        row.names(Z) <- ty
        s <- (2*abs(Z$y-Z$x) + (1-Z$x))
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$St <- s
        colnames(Z)[1:3] <- c('Suff.Term/Focal Conjunct', outcome, 'MostTyp')
        Z<-Z[, c( 1, 2, 4, 3)]
        Z <- Z[order(Z$St),]
        PDU <- as.data.frame(PD[ty,-c(ncol(PD), ncol(PD)-1, term)], row.names = ty)
        Z$UniqCov <- TRUE
        if (ncol(PDU)>1) {
          PDU <- apply(PDU, 1, function(x) sum(x>0.5))
          for (j in ty) {
            if (PDU[j]==0) {Z[j,"UniqCov"] <- TRUE}
            else {Z[j,"UniqCov"] <- FALSE}}}
        else { 
          if (ncol(PDU)==1) {
            for (j in ty) {
              if (PDU[j,]<=0.5) {Z[j,"UniqCov"] <- TRUE}
              else {Z[j,"UniqCov"] <- FALSE}}
          }
        }
        names(Z)[names(Z)==outcome]<- "Outcome"
        names(Z)[names(Z)=="St"]<- "Best"
        Z$Rank <- "-"
        Z$ConsFC <- FALSE
        for (h in 1:nrow(Z)){
          if (rownames(Z)[h] %in% cfc){Z$ConsFC[h] <- TRUE}
        }
        Z <- Z[order(1-Z$ConsFC,1-Z$UniqCov, Z$Best),]
        Z <- Z[1:(min(c(nrow(Z), max_pairs))), ]
        Z <- Z[, c(1, 2, 5, 3, 4, 6, 7)]
        M <- list()
        M[[1]] <- list(title=fct, results=Z)
      }
    }
    # For terms with multiple FCs:
    else {
      M <- list()
      for (i in (1:length(tn)))
      {
        fct <- paste("Typical Cases - Focal Conjunct", tn[i], sep = " ")
        if(length(grep("~",tn)) > 0){tnn<-unlist(gsub('\\~', '', tn))}
        else{tnn<-tn}
        X <- DT1[toupper(tnn[i])]
        cct<- tnn[-grep(tnn[i], tnn)]
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
        typ1 <- (X < CCDT1$a)
        typ2 <- (X >= CCDT1$a)
        
        ty <- rownames(DT1)[typical]
        ty1 <- rownames(DT1)[typical & typ1]
        ty2 <- rownames(DT1)[typical & typ2]
        
        consfc <-(X<=Y)
        cfc <- rownames(DT1)[consfc]
        
        
        if (identical(ty, character(0))) {M[[i]] <-list(title=fct, results="no typical cases")}
        else {
          Z <- data.frame(
            "x" = X[ty,toupper(tnn[i])],
            "y" = Y[ty,outcome],
            "cctm" = CCDT1[ty,"a"],
            "termm" = CCDT[ty,"termm"],
            "s" = rep(FALSE))
          row.names(Z) <- ty
          s <- (2*abs(Z$y-Z$x) + (1-Z$termm))
          suppressWarnings(Z$s[s==min(s)] <- TRUE)
          Z$St <- s
          colnames(Z) <- c('FocalConj', outcome, 'CompConj','Term', 'MostTypFC','Best')
          Z<-Z[, c( 1, 3, 4, 2, 6, 5)]
          #Z$MostTyp_Val <- s
          Z$Rank <- NA
          Z[ty1,7] <- 1
          Z[ty2,7] <- 2
          Z <- Z[order(Z$Rank, Z$Best),]
          PDU <- PD[ty,-c(ncol(PD), ncol(PD)-1, term), drop = FALSE]
          PDU <- apply(PDU, 1, function(x) sum(x>0.5))
          Z$UniqCov <- TRUE
          for (j in ty) {
            if (PDU[j]==0) {Z[j,"UniqCov"] <- TRUE}
            else {Z[j,"UniqCov"] <- FALSE}}
          Z<-Z[, c( 1, 4, 2, 3, 5, 6, 8, 7)]
          Z$ConsFC <- FALSE
          for (h in 1:nrow(Z)){
            if (rownames(Z)[h] %in% cfc){Z$ConsFC[h] <- TRUE}
          }
          Z <- Z[order(Z$Rank, 1-Z$ConsFC, 1-Z$UniqCov, Z$Best),]
          Z[,c(1:5,8)] <- round(Z[,c(1:5,8)], digits=3)
          names(Z)[names(Z)==outcome]<- "Outcome"
          Z$MostTypTerm <- FALSE
          mtt <- cases.suf.typ.most(results = results, outcome = outcome, sol = sol)
          mtt <- mtt[[1]]$results
          mttc <- mtt[mtt$term==colnames(PD)[term],"case"]
          for (h in 1:nrow(Z)){
            if (rownames(Z)[h] %in% mttc){Z$MostTypTerm[h] <- TRUE}
          }
          Z <- Z[1:(min(c(nrow(Z), max_pairs))), ]
          Z <- Z[, c(1, 2, 3, 4, 7, 5, 6, 8, 9, 10)]
          M[[i]] <- list(title=fct, results=Z)
        }
      }
    }
    class(M) <- 'matchessuf'
    return(M)}

# TYP MOST
cases.suf.typ.most <-
  function(results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }	
    R <- cases.suf.typ(results, outcome, sol)
    R <- R[[1]]$results
    M <- list()
    M[[1]] <- list(title="Most Typical Cases", results=R[R$MostTyp, ])
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP
cases.suf.typ <-
  function(results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    aux <-
      function(i)
      {
        fil <- (X[,i] > 0.5) & (y > 0.5) & (X[,i] <= y)
        Z <- data.frame(x=X[fil, i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        term=rep(colnames(X)[i], sum(fil)),
                        case=rownames(X)[fil])
        s <- (2*(Z$y-Z$x) + (1-Z$x))
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$St <- s 
        colnames(Z)[1:3] <- c('term_membership', outcome, 'MostTyp')
        Z<-Z[, c(5, 4, 1, 2, 6, 3)]
        return(Z[order(Z$St),])
      }
    R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
    R <- R[R$term!='solution_formula', ]
    cases <- unique(R$case)
    su <- vapply(cases, function(i) sum(R[R$case==i,3]>0.5), FUN.VALUE=numeric(1))
    R$UniqCov <- R$case %in% cases[su==1]
    R <- R[order(R$term,-R$UniqCov, R$St, R$term_membership),]
    names(R)[names(R)==outcome]<- "Outcome"
    names(R)[names(R)=="St"]<-"Best"
    R <- R[, c(1, 2, 3, 4, 7, 5, 6)]
    M <- list()
    M[[1]] <- list(title="Typical Cases", results=R)
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP UNIQUE
cases.suf.typ.unique <-
  function(results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }	
    R <- cases.suf.typ(results, outcome, sol)
    R <- R[[1]]$results
    M<-list()
    M[[1]] <- list(title="Uniquely Covered Typical Cases", results=R[R$UniqCov, ])
    class(M) <- 'matchessuf'
    return(M)
  }

