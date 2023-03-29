# Necessity Single Case + Matching Selection Helpers:

# DCN
cases.nec.dcn <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           ...)
  { 
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
      for (i in 1:length(nec.cond)){
        if(length(grep("~",nec.cond[i])) > 0){
          nec.c<-nec.cond[i][grep("~",nec.cond[i])]
          nec.c<-gsub('\\~', '', nec.c)
          nec.c<-unlist(nec.c)
          nec.c <- toupper(nec.c)
          NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
        else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    if (!is.null(suin)){
    nec.cond <- c(nec.cond,"MYSUIN")
    NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
    nec.cond <- c(nec.cond,"MYCONJUNC")
    NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
    nec.cond <- setdiff(nec.cond,conjunc)
    NC <- NC[,nec.cond,drop=FALSE]
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
    CS["TT_row"]<-do.call(pmin,TS)
    CS["TT_row"] <- round(CS["TT_row"], digits = 3)
    
    M <- list()
    for (i in 1:length(nec.cond)){
        fil <- (NC[,i,drop=FALSE] < 0.5) & (y > 0.5) & (CS[,"TT_row",drop=FALSE]>0.5)
        if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
        else{
        if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
          else{titl = nec.cond[i]}
        }
        if (sum(fil)==0) {M[[i]] <- list(title=titl, results="No cases")}
        else{
        Z <- data.frame(x=NC[fil,i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        NecCond=rep(nec.cond[i], sum(fil)),
                        Case=rownames(X)[fil], ttr=CS[rownames(X)[fil],"TT_row"])
        #s <- abs(Z$ttr-Z$y) + (1-Z$ttr)
        #s <- (1-Z$ttr) + Z$x
        s <- 1-(Z$y-Z$x)
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$Sd <- s 
        colnames(Z)[1:3] <- c('NecCond', outcome, 'Most_deviant')
        Z <- Z[, c(5, 1, 2, 7, 3)]

    Z <- merge(x=Z, y=CS, by.x='Case', by.y='row.names')
    names(Z)[6:(ncol(Z)-1)] <- paste('TT_', names(Z)[6:(ncol(Z)-1)], sep='')
    O <-subset(Z,select=3)
    Z <-Z[,-c(3)]
    Z$Outcome <- O
    sortnames<-names(Z)[5:(ncol(Z)-2)]
    Z <- Z[do.call("order", c(Z[sortnames], Z["Sd"])), ]
    names(Z$Outcome)<- "Outcome"
    names(Z)[names(Z)=="Sd"]<- "Best"
    ttsplit <- aggregate(Z$Best,by=Z[sortnames],min, drop=FALSE)
    Z$MostDCONS <- FALSE
    for (n in 1:nrow(Z)){
      for (s in 1:nrow(ttsplit)){
        if(all(ttsplit[s,sortnames] == Z[n,sortnames]) & ttsplit[s,"x"] == Z[n, "Best"]){Z[n,"MostDCONS"] <- TRUE}
      }}
    Z$`TT<=Y` <- FALSE
    for (n in 1:nrow(Z)){
      if(Z[n,"TT_row"] <= Z[n,"Outcome"]){Z$`TT<=Y`[n] <- TRUE}
    }
    
    if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
      Z$GlobUncov <- FALSE
      if (names(NC[,i,drop=FALSE]) %in% suin){
        compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
        gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
        colnms=names(gufill)
        gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
        gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
        for (m in  1:nrow(Z))
        {Z$GlobUncov[m] <- gufill[as.character(Z$Case[m]),"sum"]}
      }
      Z <- Z[do.call("order", c(Z[sortnames], 1-Z["GlobUncov"], 1-Z["TT<=Y"], Z["Best"])), ]
      Z <- cbind(Z[,c(1,2)], Z[sortnames],Z["TT_row"], Z["Outcome"],Z["GlobUncov"],Z["TT<=Y"],Z["Best"], Z["MostDCONS"])
    }
    else{
      
      Z <- Z[do.call("order", c(Z[sortnames], 1-Z["TT<=Y"], Z["Best"])), ]
      Z <- cbind(Z[,c(1,2)], Z[sortnames],Z["TT_row"], Z["Outcome"],Z["TT<=Y"],Z["Best"], Z["MostDCONS"])
    }
    
    M[[i]] <- list(title=titl, results=Z) 
    }}
    class(M) <- 'matchessuf'
    return(M)
  }


# DREL
cases.nec.drel <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           ...)
  { 
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    suin <- NULL
    nec.cond_or <- nec.cond
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
      if (!is.null(suin)){
        nec.cond <- c(nec.cond,"MYSUIN")
        NC$MYSUIN <- apply(NC[, suin], 1, max)}
      if (!is.null(conjunc)){
        nec.cond <- c(nec.cond,"MYCONJUNC")
        NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
        nec.cond <- setdiff(nec.cond,conjunc)
        NC <- NC[,nec.cond,drop=FALSE]
      }
    
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    
    M <- list()
    for (i in 1:length(nec.cond)){
    fil <- (NC[,i,drop=FALSE] > 0.5) & (y < 0.5)
    
    if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
    else{
      if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
      else{titl = nec.cond[i]}
    }
    if (sum(fil)==0) {M[[i]] <- list(title=titl, results="No cases")}
    else{
    Z <- data.frame(x=NC[fil,i],
                    y=y[fil],
                    s=rep(FALSE, sum(fil)),
                    NecCond=rep(nec.cond[i], sum(fil)),
                    Case=rownames(X)[fil])
    s <- (1-(Z$x - Z$y)) + (1-Z$x)
    suppressWarnings(Z$s[s==min(s)] <- TRUE)
    Z$Sd <- s 
    Z <- Z[, c(5, 1, 2, 6, 3)]
    colnames(Z) <- c('Case','NecCond', "Outcome",'Best','MostDREL')
    Z  <-  Z[order(Z$Best,1-Z$MostDREL),]
    M[[i]] <- list(title=titl, results=Z) 
    }}
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP
cases.nec.typ <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    
    if (!is.null(suin)){
      nec.cond <- c(nec.cond,"MYSUIN")
      NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
      nec.cond <- c(nec.cond,"MYCONJUNC")
      NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
      #nec.cond <- setdiff(nec.cond,conjunc)
      #NC <- NC[,nec.cond,drop=FALSE]
    }
    
    
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    
    M <- list()
    for (i in 1:length(nec.cond)){
      
    if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
      fil <- (NC[,"MYCONJUNC",drop=FALSE] > 0.5) & (y > 0.5) & (NC[,"MYCONJUNC",drop=FALSE]>=y)
    }
     else{ 
    fil <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)}
    
    if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
    else{
      if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
      else{titl = nec.cond[i]}
    }
    if (sum(fil)==0) {M[[i]] <- list(title=titl, results="No cases")}
    else{
    
    Z <- data.frame(x=NC[fil, i],
                        y=y[fil],
                        s=rep(FALSE, sum(fil)),
                        case=rownames(X)[fil])
        s <- (2*abs(Z$x-Z$y) + (1-Z$x))
        suppressWarnings(Z$s[s==min(s)] <- TRUE)
        Z$St <- s 
        Z<-Z[, c(4, 1, 2, 5, 3)]
        colnames(Z) <- c('Case','NecCond', "Outcome",'Best','MostTyp')
        if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
          Z$UniqCov <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(Z))
            {Z$UniqCov[m] <- gufill[as.character(Z$Case[m]),"sum"]}
          }
        Z <- Z[order(1-Z$UniqCov,Z$Best,Z$NecCond),]
        Z<-Z[, c(1, 2, 3, 6, 4, 5)]
        
        }
        else{
          if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
            Z$Rank <- FALSE
            if (names(NC[,i,drop=FALSE]) %in% conjunc){
              compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
              gufill <- as.data.frame(NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE])
              colnms=names(gufill)
              gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
              gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1 , 2)
              for (m in  1:nrow(Z))
              {Z$Rank[m] <- gufill[as.character(Z$Case[m]),"sum"]}
            }
            
            Z$CleanCorr <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(((NC[,compconj,drop=FALSE] > NC[,i,drop=FALSE]) & (NC[,i,drop=FALSE] >= y[,1,drop=FALSE])) | ((NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE]) & (NC[,compconj,drop=FALSE] < y[,1,drop=FALSE])))
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE , FALSE)
            for (m in  1:nrow(Z))
            {
              if (Z$Rank[m]==1){Z$CleanCorr[m] <- gufill[as.character(Z$Case[m]),"sum"]}
            else{Z$CleanCorr[m] <- TRUE}
              }
            
            Z$UniqCov <- FALSE
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] <0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(Z))
            {Z$UniqCov[m] <- gufill[as.character(Z$Case[m]),"sum"]}
            
            
            Z <- Z[order(Z$Rank,1-Z$CleanCorr,1-Z$UniqCov,Z$Best,Z$NecCond,1-Z$MostTyp),]
            Z<-Z[, c(1, 2, 3, 6,7,8, 4, 5)]
            
          }
          else{
            Z$UniqCov <- FALSE
            if (nec.cond[i] == "MYSUIN"){
              compsuin <- setdiff(names(NC), c(suin, names(NC[,i,drop=FALSE])))
            }
            else{
              if (nec.cond[i] == "MYCONJUNC"){
                compsuin <- setdiff(names(NC), c(conjunc, names(NC[,i,drop=FALSE])))
              }
              else{
                compsuin <- setdiff(names(NC), names(NC[,i,drop=FALSE]))
              }
            }
              gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
              colnms=names(gufill)
              gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
              gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
              for (m in  1:nrow(Z))
              {Z$UniqCov[m] <- gufill[as.character(Z$Case[m]),"sum"]}
          Z <- Z[order(1-Z$UniqCov,Z$Best,Z$NecCond,1-Z$MostTyp),]
          Z <- Z[, c(1, 2, 3, 6, 4, 5)]
          }
        }
    
    M[[i]] <- list(title=titl, results=Z)
    }}
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP - DREL matching

matches.nec.typdrel <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           max_pairs=5,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    if (!is.null(suin)){
      nec.cond <- c(nec.cond,"MYSUIN")
      NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
      nec.cond <- c(nec.cond,"MYCONJUNC")
      NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
      nec.cond <- setdiff(nec.cond,conjunc)
      NC <- NC[,nec.cond,drop=FALSE]
    }
    
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    
    M <- list()
    for (i in 1:length(nec.cond)){
      
      typ <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)
      drel <- (NC[,i,drop=FALSE] > 0.5) & (y < 0.5)
      
      if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
      else{
        if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
        else{titl = nec.cond[i]}
      }
      rnt <- rownames(NC)[typ]
      rnd <- rownames(NC)[drel]
      K <- expand.grid(rnt, rnd)
      DT <- results$tt$recoded.data
      if (nrow(K)>0){
        aux.f <-
          function(p)
          {
            t <- which(rownames(X)==p[1])
            d <- which(rownames(X)==p[2])
            s <- (1-NC[t,i]) + (1-NC[d,i]) + abs(1-(y[t,]-y[d,])) + abs(NC[t,i]-NC[d,i])
            return(s)
          }
        s <- apply(K, 1, aux.f)
        R <- data.frame(TYP=K[,1],
                        DREL=K[,2],
                        Best=s,
                        Best_matching_pair=rep(FALSE, length(s)))	  
        
      R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
      R$Best <- round(R$Best, digits = 3)
      rownames(R) <- NULL
      R <- R[,-c(4)]
      
      R$MostTyp <- FALSE
      mtt <- cases.nec.typ(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
      mtt <- mtt[[i]]$results
      mttc <- mtt$Case[(mtt$MostTyp==TRUE)]
      for (h in 1:nrow(R)){
        if (R$TYP[h] %in% mttc){R$MostTyp[h] <- TRUE}
      }
      
      
      R$MostDREL <- FALSE
      mtt <- cases.nec.drel(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
      mtt <- mtt[[i]]$results
      mttc <- mtt$Case[(mtt$MostDREL==TRUE)]
      for (h in 1:nrow(R)){
        if (R$DREL[h] %in% mttc){R$MostDREL[h] <- TRUE}
      }
      
      if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
        R$UniqCov <- FALSE
        if (names(NC[,i,drop=FALSE]) %in% suin){
          compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
          gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
          colnms=names(gufill)
          gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
          gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
          for (m in  1:nrow(R))
          {R$UniqCov[m] <- gufill[as.character(R$TYP[m]),"sum"]}
        }
        R <- R[order(-R$UniqCov, R$Best, -R$MostTyp, -R$MostDREL),]
        R<-R[, c(1, 2, 6, 3, 4, 5)]
        
        }
        else{
      R <- R[order(R$Best, -R$MostTyp, -R$MostDREL),]}
      M[[i]] <- list(title=titl, results=R[1:(min(c(nrow(R), max_pairs))), ])
      }
      else{
        R <- data.frame(TYP=NULL,
                        DREL=NULL,
                        Best=NULL,
                        MostTyp=NULL,
                        MostDREL= NULL)
      M[[i]] <- list(title=titl, results=R)
      }
      }
    class(M) <- 'matchessuf'
    return(M)
  }


# TYP - DCONS matching

matches.nec.typdcn <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           max_pairs=5,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    if (!is.null(suin)){
      nec.cond <- c(nec.cond,"MYSUIN")
      NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
      nec.cond <- c(nec.cond,"MYCONJUNC")
      NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
      nec.cond <- setdiff(nec.cond,conjunc)
      NC <- NC[,nec.cond,drop=FALSE]
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
    tt_row <- apply(CS, 1,	function(i) paste(i, collapse=''))
    TS <- CS
    TS[TS<0.50]<-1-TS[TS<0.50]
    CS[CS<0.50]<-0
    CS[CS>0.50]<-1
    CS["TT_row"]<-do.call(pmin,TS)
    CS["TT_row"] <- round(CS["TT_row"], digits = 3)
    
    
    M <- list()
    for (i in 1:length(nec.cond)){
      
      if (nec.cond[i] == "MYSUIN"){
        titl = nec.cond_or[grepl("\\+",nec.cond_or)]
        suinti<-unlist(gsub('\\~', '', suin))
        FS <- round(results$tt$recoded.data)
        FS <- FS[, -which(colnames(FS)%in% c(outcome,suinti))]
        tt_row_n <- apply(FS, 1,	function(i) paste(i, collapse=''))
        }
      else{
        if (nec.cond[i] == "MYCONJUNC"){
          titl = nec.cond_or[grepl("\\*",nec.cond_or)]
          conjuncti<-unlist(gsub('\\~', '', conjunc))
          FS <- round(results$tt$recoded.data)
          FS <- FS[, -which(colnames(FS)%in% c(outcome,conjuncti))]
          tt_row_n <- apply(FS, 1,	function(i) paste(i, collapse=''))
          }
        else{titl = nec.cond[i]
        FS <- round(results$tt$recoded.data)
        FS <- FS[, -which(colnames(FS)%in% c(outcome,nec.cond[i]))]
        tt_row_n <- apply(FS, 1,	function(i) paste(i, collapse=''))
        }
      }
      
      typ <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)
      dcn <- (NC[,i,drop=FALSE] < 0.5) & (y > 0.5)
      rnt <- rownames(NC)[typ]
      rnd <- rownames(NC)[dcn]
      K <- expand.grid(rnt, rnd)
      
      # Keep only pairrs in same tt row
      fil <- apply(K, 1, function(p) tt_row_n[p[1]]==tt_row_n[p[2]] )            
      K_fill <- K[fil, ]
      
      #if (nrow(K_fill)==0){print("There are no pairs in the same TT row!")}
        K<-K_fill
      #else{K<-K_fill}
      
      DT <- results$tt$recoded.data
      if (nrow(K)>0){
        aux.f <-
          function(p)
          {
            t <- which(rownames(X)==p[1])
            d <- which(rownames(X)==p[2])
            s <- (1-(NC[t,i]-NC[d,i])) + abs(y[t,]-y[d,])
            return(s)
          }
        s <- apply(K, 1, aux.f)
        R <- data.frame(TYP=K[,1],
                        DCONS=K[,2],
                        Best=s,
                        Best_matching_pair=rep(FALSE, length(s)))	  
        
        R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-c(4)]
        
        if (nrow(K)>0){
        FS$ids <- rownames(FS)
        R <- merge(R, FS, by.x='DCONS', by.y='ids')
        colnames(R)[4:ncol(R)] <- paste('TT_', colnames(R)[4:ncol(R)], sep='') 
        tt_row_fil <- apply(R[, grep('TT_', colnames(R))], 1, 
                            function(r) paste(r, collapse=''))
        R <- R[order(tt_row_fil), ]
        tt_row_fil <- tt_row_fil[order(tt_row_fil)]
        sortnames<-names(R)[4:(ncol(R))]}
        
        R$MostTyp <- FALSE
        mtt <- cases.nec.typ(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[i]]$results
        mttc <- mtt$Case[(mtt$MostTyp==TRUE)]
        for (h in 1:nrow(R)){
          if (R$TYP[h] %in% mttc){R$MostTyp[h] <- TRUE}
        }
        
        
        R$MostDCONS <- FALSE
        mtt <- cases.nec.dcn(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[i]]$results
        mttc <- mtt$Case[(mtt$MostDCONS==TRUE)]
        for (h in 1:nrow(R)){
          if (R$DCONS[h] %in% mttc){R$MostDCONS[h] <- TRUE}
        }
        
        if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
          R$UniqCov <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < NC[,i,drop=FALSE])
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov[m] <- gufill[as.character(R$TYP[m]),"sum"]}
          }
          
          R$GlobUncov <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < NC[,i,drop=FALSE])
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$GlobUncov[m] <- gufill[as.character(R$DCONS[m]),"sum"]}
          }
          
          aux.list <-
            function(x)
            {
              x <- x[order(-x$UniqCov, -x$GlobUncov, x$Best, -x$MostTyp, -x$MostDCONS), ]
              return(x[1:min(c(nrow(x), max_pairs)), ])
            }
          
          R_list <- lapply(split(R, tt_row_fil), aux.list)
          R <- do.call(rbind, R_list)
          R$Best <- round(R$Best, digits = 3)
          rownames(R) <- NULL
          R <- cbind(R[,1:2],R[sortnames],R["UniqCov"],R["GlobUncov"],R["Best"], R["MostTyp"], R["MostDCONS"])
          
          #R <- R[order(-R$UniqCov, -R$GlobUncov, R$Best, -R$MostTyp, -R$MostDCONS),]
          #R<-R[, c(1, 2, 3, 6, 7, 4, 5)]
          
        }
        else{
          #	find the best match for each TT row	
          aux.list <-
            function(x)
            {
              x <- x[order(x$Best, -x$MostTyp, -x$MostDCONS), ]
              return(x[1:min(c(nrow(x), max_pairs)), ])
            }
          
          R_list <- lapply(split(R, tt_row_fil), aux.list)
          R <- do.call(rbind, R_list)
          R$Best <- round(R$Best, digits = 3)
          rownames(R) <- NULL
          R <- cbind(R[,1:2],R[sortnames],R["Best"],R["MostTyp"],R["MostDCONS"])
          }
        M[[i]] <- list(title=titl, results=R[1:(min(c(nrow(R), max_pairs))), ])
      }
      else{
        R <- "There are no pairs in the same TT row"
        M[[i]] <- list(title=titl, results=R)
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP - TYP matching

matches.nec.typtyp <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           max_pairs=5,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    if (!is.null(suin)){
      nec.cond <- c(nec.cond,"MYSUIN")
      NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
      nec.cond <- c(nec.cond,"MYCONJUNC")
      NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
      #nec.cond <- setdiff(nec.cond,conjunc)
      #NC <- NC[,nec.cond,drop=FALSE]
    }
    
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    
    M <- list()
    for (i in 1:length(nec.cond)){
      
      if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
      else{
        if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
        else{titl = nec.cond[i]}
      }
      
      if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
        TYP1 <-  (NC[,"MYCONJUNC",drop=FALSE] > 0.5) & (y > 0.5) & (NC[,"MYCONJUNC",drop=FALSE]>=y)
        TYP2 <-  (NC[,"MYCONJUNC",drop=FALSE] > 0.5) & (y > 0.5) & (NC[,"MYCONJUNC",drop=FALSE]>=y)
      }
      else{ 
        TYP1 <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)
        TYP2 <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)
        }
      
      rnt <- rownames(NC)[TYP1]
      rnd <- rownames(NC)[TYP2]
      K <- expand.grid(rnt, rnd)
      DT <- results$tt$recoded.data
      
      if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
          compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
          NC_CC <-  NC[,compconj,drop=FALSE]
          NC_CC$sum <- pmin(NC_CC[,compconj,drop=FALSE])
          names(NC_CC[,ncol(NC_CC)])<-"sum"
          }
      
      if (nrow(K)>0){
        
        if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
          aux.f <-
            function(p)
            {
              t <- which(rownames(X)==p[1])
              d <- which(rownames(X)==p[2])
              s <- (0.5-(NC[t,i]-NC[d,i])) + (0.5-(y[t,]-y[d,])) + abs(NC_CC[t,"sum"]-NC_CC[d,"sum"]) + 2*(NC[t,i]-y[t,]) + 2*(NC[d,i]-y[d,])
              return(s)
            }
        }
        else{
        aux.f <-
          function(p)
          {
            t <- which(rownames(X)==p[1])
            d <- which(rownames(X)==p[2])
            s <- (0.5-(NC[t,i]-NC[d,i])) + (0.5-(y[t,]-y[d,])) + 2*(NC[t,i]-y[t,]) + 2*(NC[d,i]-y[d,])
            return(s)
          }
        }
        aux.f2 <-
          function(p)
          {
            t <- which(rownames(X)==p[1])
            d <- which(rownames(X)==p[2])
            sm <- 2*abs(NC[t,i]-y[t,]) + (1-NC[t,i])
            sn <- 2*abs(NC[d,i]-y[d,]) + (1-NC[d,i])     
            return(sm<=sn)
          }
        s <- apply(K, 1, aux.f)
        mt <- apply(K, 1, aux.f2)
        R <- data.frame(TYP1=K[,1],
                        TYP2=K[,2],
                        Best=s,
                        Best_matching_pair=rep(FALSE, length(s)),
                        TYP1MoreTypical=mt)	  
        R <- R[R$TYP1MoreTypical==TRUE,]
        R <- R[R$TYP1!=R$TYP2,]
        R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-c(4,5)]
        
        R$MostTYP1 <- FALSE
        mtt <- cases.nec.typ(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[i]]$results
        mttc <- mtt$Case[(mtt$MostTyp==TRUE)]
        for (h in 1:nrow(R)){
          if (R$TYP1[h] %in% mttc){R$MostTYP1[h] <- TRUE}
        }
        
        
        R$MostTYP2 <- FALSE
        mtt <- cases.nec.typ(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[i]]$results
        mttc <- mtt$Case[(mtt$MostTyp==TRUE)]
        for (h in 1:nrow(R)){
          if (R$TYP2[h] %in% mttc){R$MostTYP2[h] <- TRUE}
        }
        
        if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
          R$UniqCov1 <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov1[m] <- gufill[as.character(R$TYP1[m]),"sum"]}
          }
          
          R$UniqCov2 <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov2[m] <- gufill[as.character(R$TYP2[m]),"sum"]}
          }
          
          R$MostTyp <- 0
          for (h in 1:nrow(R)){
            if (R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "both"}
            if (R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "typ1"}
            if (!R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "typ2"}
            if (!R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "none"}
          }
          
          R$UniqCov <- 0
          for (h in 1:nrow(R)){
            if (R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "both"}
            if (R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "typ1"}
            if (!R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "typ2"}
            if (!R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "none"}
          }
          
          
          R <- R[order(-R$UniqCov1, -R$UniqCov2, R$Best, -R$MostTYP1, -R$MostTYP2),]
          R <- R[,c(1,2,9,3,8)]
          
        }
        else{
          if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
            R$maxFCTYP1 <- 0
              compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
              gufill <- as.data.frame(NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE])
              colnms=names(gufill)
              gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
              gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
              for (m in  1:nrow(R))
              {R$maxFCTYP1[m] <- gufill[as.character(R$TYP1[m]),"sum"]}
            
            
            R$maxFCTYP2 <- 0
              compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
              gufill <- as.data.frame(NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE])
              colnms=names(gufill)
              gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
              gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
              for (m in  1:nrow(R))
              {R$maxFCTYP2[m] <- gufill[as.character(R$TYP2[m]),"sum"]}
            
            
            R$MostTyp <- 0
            for (h in 1:nrow(R)){
              if (R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "both"}
              if (R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "typ1"}
              if (!R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "typ2"}
              if (!R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "none"}
            }
            
            R$PairRank <- 0
            for (h in 1:nrow(R)){
              if (R$maxFCTYP1[h]==1 & R$maxFCTYP2[h]==1){R$PairRank[h] <- 1}
              if (R$maxFCTYP1[h]==1 & R$maxFCTYP2[h]==2){R$PairRank[h] <- 2}
              if (R$maxFCTYP1[h]==2 & R$maxFCTYP2[h]==1){R$PairRank[h] <- 3}
              if (R$maxFCTYP1[h]==2 & R$maxFCTYP2[h]==2){R$PairRank[h] <- 4}
            }
            
            R$CleanCorr1 <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(((NC[,compconj,drop=FALSE] > NC[,i,drop=FALSE]) & (NC[,i,drop=FALSE] >= y[,1,drop=FALSE])) | ((NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE]) & (NC[,compconj,drop=FALSE] < y[,1,drop=FALSE])))
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE , FALSE)
            for (m in  1:nrow(R))
            {
              if (R$maxFCTYP1[m]==1){R$CleanCorr1[m] <- gufill[as.character(R$TYP1[m]),"sum"]}
              else{R$CleanCorr1[m] <- TRUE}
            }
            
            R$CleanCorr2 <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(((NC[,compconj,drop=FALSE] > NC[,i,drop=FALSE]) & (NC[,i,drop=FALSE] >= y[,1,drop=FALSE])) | ((NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE]) & (NC[,compconj,drop=FALSE] < y[,1,drop=FALSE])))
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE , FALSE)
            for (m in  1:nrow(R))
            {
              if (R$maxFCTYP2[m]==1){R$CleanCorr2[m] <- gufill[as.character(R$TYP2[m]),"sum"]}
              else{R$CleanCorr2[m] <- TRUE}
            }
            
            R$CleanCorr <- 0
            for (h in 1:nrow(R)){
              if (R$CleanCorr1[h] & R$CleanCorr2[h]){R$CleanCorr[h] <- "both"}
              if (R$CleanCorr1[h] & !R$CleanCorr2[h]){R$CleanCorr[h] <- "typ1"}
              if (!R$CleanCorr1[h] & R$CleanCorr2[h]){R$CleanCorr[h] <- "typ2"}
              if (!R$CleanCorr1[h] & !R$CleanCorr2[h]){R$CleanCorr[h] <- "none"}
            }
            
            R$UniqCov1 <- FALSE
            R$UniqCov2 <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] <0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov1[m] <- gufill[as.character(R$TYP1[m]),"sum"]}
            
            for (m in  1:nrow(R))
            {R$UniqCov2[m] <- gufill[as.character(R$TYP2[m]),"sum"]}
            
            R$UniqCov <- 0
            for (h in 1:nrow(R)){
              if (R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "both"}
              if (R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "typ1"}
              if (!R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "typ2"}
              if (!R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "none"}
            }
            
            R <- R[R$TYP1!=R$TYP2,]
            R <- R[order(R$PairRank, -R$CleanCorr1, -R$CleanCorr2, R$Best, -R$MostTYP1, -R$MostTYP2, -R$UniqCov1, -R$UniqCov2),]
            R <- R[,c(1,2,9,12,3,8,15)]
            
          }
        else{
          R$MostTyp <- 0
          for (h in 1:nrow(R)){
            if (R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "both"}
            if (R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "typ1"}
            if (!R$MostTYP1[h] & R$MostTYP2[h]){R$MostTyp[h] <- "typ2"}
            if (!R$MostTYP1[h] & !R$MostTYP2[h]){R$MostTyp[h] <- "none"}
          }
        
          R$UniqCov1 <- FALSE
          R$UniqCov2 <- FALSE
          if (nec.cond[i] == "MYSUIN"){
            compsuin <- setdiff(names(NC), c(suin, names(NC[,i,drop=FALSE])))
          }
          else{
            if (nec.cond[i] == "MYCONJUNC"){
              compsuin <- setdiff(names(NC), c(conjunc, names(NC[,i,drop=FALSE])))
            }
            else{
              compsuin <- setdiff(names(NC), names(NC[,i,drop=FALSE]))
            }
          }
          gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
          colnms=names(gufill)
          gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
          gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
          
          for (m in  1:nrow(R))
          {R$UniqCov1[m] <- gufill[as.character(R$TYP1[m]),"sum"]}
        
          for (m in  1:nrow(R))
          {R$UniqCov2[m] <- gufill[as.character(R$TYP2[m]),"sum"]}
          
          R$UniqCov <- 0
          for (h in 1:nrow(R)){
            if (R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "both"}
            if (R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "typ1"}
            if (!R$UniqCov1[h] & R$UniqCov2[h]){R$UniqCov[h] <- "typ2"}
            if (!R$UniqCov1[h] & !R$UniqCov2[h]){R$UniqCov[h] <- "none"}
          }
            
        R <- R[R$TYP1!=R$TYP2,]
        R <- R[order( -R$UniqCov1, -R$UniqCov2, R$Best, -R$MostTYP1, -R$MostTYP2),]
        R <- R[,c(1,2,9,3,6)]
        }
        }
        
        M[[i]] <- list(title=titl, results=R[1:(min(c(nrow(R), max_pairs))), ])
      }
      else{
        R <- data.frame(TYP1=NULL,
                        TYP2=NULL,
                        Best=NULL,
                        MostTyp=NULL)
        M[[i]] <- list(title=titl, results=R)
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP - IIR matching

matches.nec.typiir <-
  function(nec.cond,
           results,
           outcome,
           sol=1,
           max_pairs=5,
           ...)
  {
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    
    nec.cond_or <- nec.cond
    
    suin <- NULL
    if(length(grep("\\+",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\+'))
      suin <- unlist(strsplit(nec.cond_or[grepl("\\+",nec.cond_or)], '\\+'))
      #suin<-unlist(gsub('\\~', '', suin))
    }
    
    conjunc <- NULL
    if(length(grep("\\*",nec.cond)) > 0){
      nec.cond<-unlist(strsplit(nec.cond, '\\*'))
      conjunc <- unlist(strsplit(nec.cond_or[grepl("\\*",nec.cond_or)], '\\*'))
      #conjunc<-unlist(gsub('\\~', '', conjunc))
    }
    
    NC <- data.frame(matrix(NA,ncol=length(nec.cond),nrow=nrow(results$tt$recoded.data)))
    colnames(NC) <- nec.cond
    rownames(NC) <- rownames( results$tt$recoded.data)
    
    for (i in 1:length(nec.cond)){
      if(length(grep("~",nec.cond[i])) > 0){
        nec.c<-nec.cond[i][grep("~",nec.cond[i])]
        nec.c<-gsub('\\~', '', nec.c)
        nec.c<-unlist(nec.c)
        nec.c <- toupper(nec.c)
        NC[,i] <- 1-results$tt$recoded.data[, nec.c, drop=FALSE]}
      else{NC[,i] <- results$tt$recoded.data[, nec.cond[i], drop=FALSE]}}
    
    if (!is.null(suin)){
      nec.cond <- c(nec.cond,"MYSUIN")
      NC$MYSUIN <- apply(NC[, suin], 1, max)}
    if (!is.null(conjunc)){
      nec.cond <- c(nec.cond,"MYCONJUNC")
      NC$MYCONJUNC <- apply(NC[, conjunc], 1, min)
      #nec.cond <- setdiff(nec.cond,conjunc)
      #NC <- NC[,nec.cond,drop=FALSE]
    }
    
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    
    M <- list()
    for (i in 1:length(nec.cond)){
      
      if (nec.cond[i] == "MYSUIN"){titl = nec.cond_or[grepl("\\+",nec.cond_or)]}
      else{
        if (nec.cond[i] == "MYCONJUNC"){titl = nec.cond_or[grepl("\\*",nec.cond_or)]}
        else{titl = nec.cond[i]}
      }
      
      if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
        TYP1 <-  (NC[,"MYCONJUNC",drop=FALSE] > 0.5) & (y > 0.5) & (NC[,"MYCONJUNC",drop=FALSE]>=y)
      }
      else{ 
        TYP1 <- (NC[,i,drop=FALSE] > 0.5) & (y > 0.5) & (NC[,i,drop=FALSE]>=y)
      }
      
      iir <- (NC[,i,drop=FALSE] < 0.5) & (y < 0.5) & (NC[,i,drop=FALSE]>=y)
      rnt <- rownames(NC)[TYP1]
      rnd <- rownames(NC)[iir]
      K <- expand.grid(rnt, rnd)
      DT <- results$tt$recoded.data
      
      if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
        compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
        NC_CC <-  NC[,compconj,drop=FALSE]
        NC_CC$sum <- pmin(NC_CC[,compconj,drop=FALSE])
        names(NC_CC[,ncol(NC_CC)])<-"sum"
      }
      
      if (nrow(K)>0){
        
        if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
          aux.f <-
            function(p)
            {
              t <- which(rownames(X)==p[1])
              d <- which(rownames(X)==p[2])
              s <- (1-(NC[t,i]-NC[d,i])) + (1-(y[t,]-y[d,])) + + abs(NC_CC[t,"sum"]-NC_CC[d,"sum"]) + 2*abs(NC[t,i]-y[t,]) + abs(NC[d,i]-y[d,])
              return(s)
            }
        }
        else{
        aux.f <-
          function(p)
          {
            t <- which(rownames(X)==p[1])
            d <- which(rownames(X)==p[2])
            s <- (1-(NC[t,i]-NC[d,i])) + (1-(y[t,]-y[d,])) + 2*abs(NC[t,i]-y[t,]) + abs(NC[d,i]-y[d,])
            return(s)
          }
        }
      
        s <- apply(K, 1, aux.f)
        R <- data.frame(TYP=K[,1],
                        IIR=K[,2],
                        Best=s,
                        Best_matching_pair=rep(FALSE, length(s)))	  
        R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-c(4)]
        
        R$MostTyp <- FALSE
        mtt <- cases.nec.typ(nec.cond = nec.cond_or, results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[i]]$results
        mttc <- mtt$Case[(mtt$MostTyp==TRUE)]
        for (h in 1:nrow(R)){
          if (R$TYP[h] %in% mttc){R$MostTyp[h] <- TRUE}
        }
        
        if (length(suin)>0 & names(NC[,i,drop=FALSE]) %in% suin){
          R$UniqCov <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov[m] <- gufill[as.character(R$TYP[m]),"sum"]}
          }
          
          R$GlobUncov <- FALSE
          if (names(NC[,i,drop=FALSE]) %in% suin){
            compsuin <- setdiff(suin, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$GlobUncov[m] <- gufill[as.character(R$IIR[m]),1]}
          }
          R <- R[order(-R$UniqCov, -R$GlobUncov, R$Best, -R$MostTyp),]
          R <- R[,c(1,2,5,6,3,4)]
          
        }
        else{
          if (length(conjunc)>0 & names(NC[,i,drop=FALSE]) %in% conjunc){
            
            R$maxFCTyp <- 0
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE])
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
            for (m in  1:nrow(R))
            {R$maxFCTyp[m] <- gufill[as.character(R$TYP[m]),"sum"]}
            
            
            R$maxFCIIR <- 0
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] <= NC[,i,drop=FALSE])
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
            for (m in  1:nrow(R))
            {R$maxFCIIR[m] <- gufill[as.character(R$IIR[m]),"sum"]}
            
            R$FCIIRleq <- 0
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,i,drop=FALSE]<0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
            for (m in  1:nrow(R))
            {R$FCIIRleq[m] <- gufill[as.character(R$IIR[m]),"sum"]}
            
            R$CCIIRheq <- 0
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] >= 0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, 1,2)
            for (m in  1:nrow(R))
            {R$CCIIRheq[m] <- gufill[as.character(R$IIR[m]),"sum"]}
          
            
            R$PairRank <- 0
            for (h in 1:nrow(R)){
              if (R$maxFCTyp[h]==1 & R$maxFCIIR[h]==2 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==1){R$PairRank[h] <- 1}
              if (R$maxFCTyp[h]==2 & R$maxFCIIR[h]==2 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==1){R$PairRank[h] <- 2}
              if (R$maxFCTyp[h]==1 &  R$maxFCIIR[h]==1 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==2){R$PairRank[h] <- 3}
              if (R$maxFCTyp[h]==2 &  R$maxFCIIR[h]==1 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==2){R$PairRank[h] <- 4}
              if (R$maxFCTyp[h]==1 &  R$maxFCIIR[h]==2 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==2){R$PairRank[h] <- 5}
              if (R$maxFCTyp[h]==2 &  R$maxFCIIR[h]==2 & R$FCIIRleq[h]==1 & R$CCIIRheq[h]==2){R$PairRank[h] <- 6}
              if (R$maxFCTyp[h]==1 & R$FCIIRleq[h]==2){R$PairRank[h] <- 7}
              if (R$maxFCTyp[h]==2 & R$FCIIRleq[h]==2){R$PairRank[h] <- 8}
            }
            
            R$CleanCorr1 <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(((NC[,compconj,drop=FALSE] > NC[,i,drop=FALSE]) & (NC[,i,drop=FALSE] >= y[,1,drop=FALSE])) | ((NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE]) & (NC[,compconj,drop=FALSE] < y[,1,drop=FALSE])))
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE , FALSE)
            for (m in  1:nrow(R))
            {
              R$CleanCorr1[m] <- gufill[as.character(R$TYP[m]),"sum"]
            }
            
            R$CleanCorr2 <- FALSE
            
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(((NC[,compconj,drop=FALSE] > NC[,i,drop=FALSE]) & (NC[,i,drop=FALSE] >= y[,1,drop=FALSE])) | ((NC[,compconj,drop=FALSE] < NC[,i,drop=FALSE]) & (NC[,compconj,drop=FALSE] < y[,1,drop=FALSE])))
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE , FALSE)
            for (m in  1:nrow(R))
            {
              R$CleanCorr2[m] <- gufill[as.character(R$IIR[m]),"sum"]
              
            }
            
            R$CleanCorr <- 0
            for (h in 1:nrow(R)){
              if (R$CleanCorr1[h] & R$CleanCorr2[h]){R$CleanCorr[h] <- "both"}
              if (R$CleanCorr1[h] & !R$CleanCorr2[h]){R$CleanCorr[h] <- "typ"}
              if (!R$CleanCorr1[h] & R$CleanCorr2[h]){R$CleanCorr[h] <- "iir"}
              if (!R$CleanCorr1[h] & !R$CleanCorr2[h]){R$CleanCorr[h] <- "none"}
            }
            
            R$UniqCov <- FALSE
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] <0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$UniqCov[m] <- gufill[as.character(R$TYP[m]),"sum"]}
            
            R$GlobUncov <- FALSE
            compconj <- setdiff(conjunc, names(NC[,i,drop=FALSE]))
            gufill <- as.data.frame(NC[,compconj,drop=FALSE] <0.5)
            colnms=names(gufill)
            gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
            gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
            for (m in  1:nrow(R))
            {R$GlobUncov[m] <- gufill[as.character(R$IIR[m]),"sum"]}
            
            R <- R[order(R$PairRank,  1- R$CleanCorr1, 1- R$CleanCorr2, R$Best, -R$MostTyp, 1-R$UniqCov, 1-R$GlobUncov),]
            R <- R[,c(1,2,9,12,3,4,13,14)]
          }
        else{
        
          R$UniqCov <- FALSE
          if (nec.cond[i] == "MYSUIN"){
            compsuin <- setdiff(names(NC), c(suin, names(NC[,i,drop=FALSE])))
          }
          else{
            if (nec.cond[i] == "MYCONJUNC"){
              compsuin <- setdiff(names(NC), c(conjunc, names(NC[,i,drop=FALSE])))
            }
            else{
              compsuin <- setdiff(names(NC), names(NC[,i,drop=FALSE]))
            }
          }
          gufill <- as.data.frame(NC[,compsuin,drop=FALSE] < 0.5)
          colnms=names(gufill)
          gufill$sum<-rowSums(gufill[,colnms, drop = FALSE])
          gufill$sum <- ifelse(gufill$sum==ncol(gufill)-1, TRUE,FALSE)
          
          for (m in  1:nrow(R))
          {R$UniqCov[m] <- gufill[as.character(R$TYP[m]),"sum"]}
          
          for (m in  1:nrow(R))
          {R$GlobUncov[m] <- gufill[as.character(R$IIR[m]),"sum"]}
          
          
          
        R <- R[order(1-R$UniqCov,1-R$GlobUncov, R$Best, -R$MostTyp),]
        R <- R[,c(1,2,5,6,3,4)]
        }
        }
        M[[i]] <- list(title=titl, results=R[1:(min(c(nrow(R), max_pairs))), ])
      }
      else{
        R <- data.frame(TYP=NULL,
                        IIR=NULL,
                        Best=NULL,
                        MostTyp=NULL)
        M[[i]] <- list(title=titl, results=R)
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }



