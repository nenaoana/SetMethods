# SMMR Matching Helper:

# DCV - IIR
matches.suf.dcviir <-
  function(results,
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
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    n <- rownames(X)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    FS <- results$tt$recoded.data
    FS <- FS[, -which(colnames(FS)==outcome)]
    #	get tt row membership W
    FA <- FS
    FA[FA<=0.5] <- 1 - FA[FA<=0.5]
    w <- apply(FA, 1, min)
    #
    CS <- round(results$tt$recoded.data)
    CS <- CS[, -which(colnames(CS)==outcome)]
    tt_row <- apply(CS, 1,	function(i) paste(i, collapse=''))
    x <- X[, 'solution_formula']	
    y <- X[, 'out']
    devcove <- ((x<0.5) & (y>0.5)) #& (w<=y)
    indirre <- ((x<0.5) & (y<0.5)) 
    rnt <- n[devcove]
    rnd <- n[indirre]                     
    K <- expand.grid(rnt, rnd) 
    if (nrow(K)==0) {
      warning('No pairs')
      return(NULL)	
    }
    #	keep only same TT row pairs
    fil <- apply(K, 1, function(p) tt_row[p[1]]==tt_row[p[2]] )            
    K_fil <- K[fil, ]
    #
    aux.f <-
      function(p)
      {
        i <- which(n==p[1])
        j <- which(n==p[2])
        s <- ((abs(w[i]-w[j])) + (1-(y[i]-y[j])) + (1-w[i])+(1-w[j])) 
        #	2 is the maximum value of this formula. the W value must be
        #	membership in the truth table row all conditions that
        #	constitute the tt row to which the cases belong
        return(s)
      }
    s <- apply(K_fil, 1, aux.f)
    R <- data.frame(DevCov=K_fil[,1],
                    IIR=K_fil[,2],
                    Best=s,
                    Best_matching_pair=rep(FALSE, length(s)))	
    #	merge with a TT
    CS$ids <- rownames(CS)
    R <- merge(R, CS, by.x='DevCov', by.y='ids')
    colnames(R)[5:ncol(R)] <- paste('TT_', colnames(R)[5:ncol(R)], sep='') 
    tt_row_fil <- apply(R[, grep('TT_', colnames(R))], 1, 
                        function(r) paste(r, collapse=''))
    R <- R[order(tt_row_fil), ]
    tt_row_fil <- tt_row_fil[order(tt_row_fil)]
    sortnames<-names(R)[5:(ncol(R))]
    R$ConsTT_DCV <- FALSE
    constt <- w<=y
    for (h in 1:nrow(R)){
      if(as.logical(constt[as.character(R$DevCov[h])]==TRUE)){R[h,"ConsTT_DCV"] <- TRUE}
    }
    
    #	find the best match for each TT row	
    aux.list <-
      function(x)
      {
        x <- x[order(-x$ConsTT_DCV, x$Best), ]
        x[x$Best==min(x$Best), 4] <- TRUE
        return(x[1:min(c(nrow(x), max_pairs)), ])
      }
    
    R_list <- lapply(split(R, tt_row_fil), aux.list)
    R <- do.call(rbind, R_list)
    R$Best <- round(R$Best, digits = 3)
    rownames(R) <- NULL
    R <- R[,-4]
    R <- cbind(R[,1:2],R[sortnames],R["Best"],R["ConsTT_DCV"])
    # R$GlobUncov< FALSE
    # for (h in 1:nrow(R)){
    #   if(X[as.character(R$IIR[h]),"solution_formula"]<0.5){R$GlobUncovIRR[h] <- TRUE}
    # }
    # R$ConsTT_DCV <- FALSE
    # for (h in 1:nrow(R)){
    #   if(X[as.character(R$DevCov[h]),"solution_formula"]<0.5){R$GlobUncovIRR[h] <- TRUE}
    # }
    
    M <- list()
    M[[1]] <- list(title="Matching Deviant Coverage-IIR Cases", results=R)
    class(M) <- 'matchessuf'
    return(M)
  }

# TYP DCN
matches.suf.typdcn <-
  function(results,
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
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    nt <- ncol(X)-2
    tn <- colnames(X)[1:nt]
    L <- list()
    M <- list()
    for (i in 1:nt){
      term <- tn[i]
      termp <- paste("Term", tn[i], sep = " ")
      x <- X[, term]
      y <- X[, 'out']
      typical <- (x>0.5) & (y>0.5) & (x<=y) 
      devcons <- (x>0.5) & (y<0.5) 
      rnt <- rownames(X)[typical]
      rnd <- rownames(X)[devcons]
      K <- expand.grid(rnt, rnd) 
      if (nrow(K)>0) {
        aux.f <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            s <- ((abs(x[i]-x[j]))+(1-(y[i]-y[j])) +(1-x[i])+(1-x[j]))
            return(s)
          }
        s <- apply(K, 1, aux.f)
        R <- data.frame(Typical=K[,1],
                        DevCons=K[,2],
                        Best=s,
                        Term=rep(term, length(s)),
                        Best_matching_pair=rep(FALSE, length(s)))	
        R <- R[order(s), ]
        R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-c(4,5)]
        
        R$MostTypTerm <- FALSE
        mtt <- cases.suf.typ.most(results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[1]]$results
        mttc <- mtt[mtt$term==colnames(X)[i],"case"]
        for (h in 1:nrow(R)){
          if (R$Typical[h] %in% mttc){R$MostTypTerm[h] <- TRUE}
        }
        
        
        R$MostDevCons <- FALSE
        mtt <- cases.suf.dcn(results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[1]]$results
        mtt <- mtt[mtt$Term==colnames(X)[i],]
        mttc <- mtt$Cases[(mtt$MostDevCons==TRUE)]
        for (h in 1:nrow(R)){
          if (R$DevCons[h] %in% mttc){R$MostDevCons[h] <- TRUE}
        }
        R <- R[order(R$Best, -R$MostTypTerm, -R$MostDevCons),]
        L[[i]]<-R[1:(min(c(nrow(R), max_pairs))), ]
        M[[i]] <- list(title=termp, results=R[1:(min(c(nrow(R), max_pairs))), ])
        class(M) <- 'matchessuf'
      } else {
        R <- data.frame(Typical=NULL,
                        DevCons=NULL,
                        Best=NULL,
                        Term=NULL,
                        Best_matching_pair=NULL,
                        MostDevCons=NULL,
                        MostTypTerm= NULL)
        R <- R[,-c(4,5)]
        L[[i]]<-R
        M[[i]] <- list(title=termp, results=R)
        class(M) <- 'matchessuf'
      }
    }
    return(M)
  }

# TYP IIR
matches.suf.typiir <-
  function(results,
           outcome,
           term=1,
           sol=1,
           max_pairs=5,
           ...)
    
  { termnr = term
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
    pdata <- pimdata(results=results, outcome=outcome, sol=sol)
    if (term>(ncol(pdata)-2)){stop("The term selected does not exist for the chosen model of the solution. Check the solution again and pick another term or change the model using the argument sol.")}
    nterm <- colnames(pdata[term])
    data <- results$tt$initial.data
    data1 <- data.frame(matrix(NA,ncol=0,nrow=nrow(data)))
    row.names(data1)<-row.names(data)
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
      data1[t_pre] <- data[t_pre]
      colnames(data1[t_pre])<-toupper(colnames(data1[t_pre]))      
    }
    if (length(t_neg) > 0) {
      data1[t_neg] <- 1 - data[t_neg] 
      colnames(data1[t_neg])<-tolower(colnames(data1[t_neg]))
    }
    
    Y <- pdata[,"out", drop=FALSE]
    names(Y) <- outcome
    
    # Formulas and stuff
    M <- list()
    for (i in (1:length(tn)))
    { 
      focconj <- paste("Focal Conjunct", tn[i], sep = " ")
      if(length(grep("~",tn[i])) > 0){tn[i]<-unlist(gsub('\\~', '', tn[i]))}
      X <- data1[toupper(tn[i])]
      if (length(tn)==1) {
        codata <- X
        codata1<-X
        names(codata1)[1]<-"a"
        codata$term<- X[,1]
      }
      else{
        # dataframe of the complementary conjuncts
        co<- tn[-grep(tn[i], tn)]
        co<- toupper(co)
        co <- unlist(gsub('\\~', '', co))
        codata<-data1[co]
        if(ncol(codata)>1){
          a<-do.call(pmin, codata[,])
          codata1<-data.frame(a) # the minimum of the complementary conjuncts
          row.names(codata1)<-row.names(codata)}
        else{
          codata1<-codata
          names(codata1)[1]<-"a"}
        codata$term<-pmin(codata1$a,X[,])
      }
      typical <-((codata$term>0.5) & (Y>0.5) & (codata$term<=Y))
      indirre <- ((codata$term<0.5) & (Y<0.5))
      
      fc <- X[,toupper(tn[i]), drop=FALSE]
      consfc <-(fc<=Y)
      #typ1 <- (X <= codata1$a)
      #typ2 <- (X > codata1$a)
      cfc <- rownames(data1)[consfc]
      # typ1 <- (X < codata1$a)
      # typ2 <- (X >= codata1$a)
      # iir3 <- (X < 0.5) & (codata1$a>0.5)
      # iir4 <- ((X < 0.5) &  (codata1$a < 0.5) & (X <= codata1$a))
      # iir5 <- ((X < 0.5) &  (codata1$a < 0.5) & (codata1$a < X))
      # iir6 <- (codata1$a < 0.5) & (X>0.5)
      
      ty <- rownames(data1)[typical]
      ir <- rownames(data1)[indirre]
      
      if (identical(ir, character(0))) {M[[i]] <-list(title=focconj, results="no individually irrelevant cases")}
      else { 
        if (identical(ty, character(0))) {M[[i]] <-list(title=focconj, results="no typical cases")}
        else { 
          K <- expand.grid(ty, ir)
          x <- X[,toupper(tn[i])]
          y <- Y[,outcome]
          if(length(tn)==1){mincc <- rep(0,length(codata1[,"a"]))}
          else{mincc <- codata1[,"a"]}
          term <- codata[,"term"]
          aux.f <-
            function(p)
            {
              i <- which(rownames(X)==p[1])
              j <- which(rownames(X)==p[2])
              s <- ((1-(x[i]-x[j]))+ #big diff. in FC
                      (1-(y[i]-y[j]))+ #big diff in Y
                      abs(mincc[i]-mincc[j])+ #small diff in complementary conj.
                      2*abs(y[i]-x[i])+ 
                      abs(y[j]-x[j])) # for IIR not multiplied
              return(s)
            }
          aux.ff <-
            function(p)
            {
              i <- which(rownames(X)==p[1])
              j <- which(rownames(X)==p[2])
              if ((x[i] < mincc[i]) & (x[j]<0.5) & (mincc[j]>=0.5)) {order<-c(1)}
              else {if ((x[i] >= mincc[i]) & (x[j]<0.5) & (mincc[j]>=0.5)) {order<-c(2)}
                else {if ((x[i] < mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (mincc[j]> x[j])) {order<-c(3)}
                  else {if ((x[i] < mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (mincc[j]<= x[j])) {order<-c(4)}
                    else {if ((x[i] >= mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (x[j] < mincc[j])) {order<-c(5)}
                      else {if ((x[i] >= mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (mincc[j]<= x[j])) {order<-c(6)}
                        else {if ((x[i] < mincc[i]) & (mincc[j]<0.5) & (x[j]>=0.5)) {order<-c(7)}
                          else {if ((x[i] >= mincc[i]) & (mincc[j]<0.5) & (x[j]>=0.5)) {order<-c(8)}
                          }}}}}}}
              return(order)
            }
          
          s <- apply(K, 1, aux.f)
          order <- apply(K, 1, aux.ff)
          
          matcres <- data.frame(Typical=K[,1],
                                IIR=K[,2],
                                Distance=round(s, digits=3),
                                PairRank=order)
          
          matcres[,5] <- NA
          matcres[,6] <- NA
          colnames(matcres)<-c("Typical","IIR","Best","PairRank", "UniqCov","GlobUncov")
          R<-cases.suf.typ(results=results, outcome=outcome, sol=sol)
          R <- R[[1]]$results
          for(u in 1:nrow(matcres)) { 
            for (uu in 1:nrow(R)){
              if (as.character(matcres[u,1])==as.character(R[uu,1])) 
              {matcres[u,5]<-R[uu,5]}
            }}
          for(u in 1:nrow(matcres)) { 
            for (uu in 1:nrow(pdata)){
              if (as.character(matcres[u,2])==rownames(pdata[uu,])) { 
                if (pdata[uu,"solution_formula"]< 0.5) {matcres[u,6]<-TRUE}
                else {matcres[u,6]<-FALSE}
              }
            }}
          maxl<-min(max_pairs,nrow(matcres))
          
          matcres$ConsFC_Typ <- FALSE
          for (h in 1:nrow(matcres)){
            if (matcres$Typical[h] %in% cfc){matcres$ConsFC_Typ[h] <- TRUE}
          }
          
          matcres$ConsFC_IIR <- FALSE
          for (h in 1:nrow(matcres)){
            if (matcres$IIR[h] %in% cfc){matcres$ConsFC_IIR[h] <- TRUE}
          }
          
          matcres<-matcres[order(matcres$PairRank,-matcres$ConsFC_Typ, -matcres$UniqCov, -matcres$ConsFC_IIR, -matcres$GlobUncov, matcres$Best),]
          if (length(tn)==1){matcres$PairRank <- "-"}
          
          matcres$MostTypTerm <- FALSE
          mtt <- cases.suf.typ.most(results = results, outcome = outcome, sol = sol)
          mtt <- mtt[[1]]$results
          mttc <- mtt[mtt$term==colnames(pdata)[termnr],"case"]
          for (h in 1:nrow(matcres)){
            if (matcres$Typical[h] %in% mttc){matcres$MostTypTerm[h] <- TRUE}
          }
         
          
          matcres$MostTypFC <- FALSE
          mtfc <- cases.suf.typ.fct(results = results, outcome = outcome, sol = sol, term = termnr, max_pairs = 100^100)
          mtfc <- mtfc[[i]]$results
          if(length(tn)==1){mtfcc <- rownames(mtfc)[(mtfc$MostTyp==TRUE)]}
          else{ mtfcc <- rownames(mtfc)[(mtfc$MostTypFC==TRUE)]}          
          for (h in 1:nrow(matcres)){
            if (matcres$Typical[h] %in% mtfcc){matcres$MostTypFC[h] <- TRUE}
          }
          
          matcres<-matcres[order(matcres$PairRank,-matcres$ConsFC_Typ, -matcres$UniqCov, -matcres$ConsFC_IIR, -matcres$GlobUncov, matcres$Best, -matcres$MostTypFC, -matcres$MostTypTerm),]
          
          
          matcres <- matcres[,c(1,2,5,6,3,4,7,9,10,8)]
          M[[i]] <- list(title=focconj, results=(head(matcres, maxl)))         
        }  
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }


# TYP TYP
matches.suf.typtyp <-
  function(results,
           outcome,
           term=1,
           sol=1,
           max_pairs=5,
           ...)
    
  { termnr <- term
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
    pdata <- pimdata(results=results, outcome=outcome, sol=sol)
    if (term>(ncol(pdata)-2)){stop("The term selected does not exist for the chosen model of the solution. Check the solution again and pick another term or change the model using the argument sol.")}
    nterm <- colnames(pdata[term])
    data <- results$tt$initial.data
    data1 <- data.frame(matrix(NA,ncol=0,nrow=nrow(data)))
    row.names(data1)<-row.names(data)
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
    # }
    # #Code for lower case:
    # else{
    #   t_pre <- toupper(tn)[toupper(tn)==tn]
    #   t_neg <- toupper(tn)[tolower(tn)==tn]}
    if (length(t_pre) > 0) {
      data1[t_pre] <- data[t_pre]
      colnames(data1[t_pre])<-toupper(colnames(data1[t_pre]))
    }
    if (length(t_neg) > 0) {
      data1[t_neg] <- 1 - data[t_neg]
      colnames(data1[t_neg])<-tolower(colnames(data1[t_neg]))
    }
    
    Y <- pdata[,"out", drop=FALSE]
    names(Y) <- outcome
    
    
    # Formulas and stuff
    M <- list()
    
    for (i in (1:length(tn)))
    { 
      focconj <- paste("Focal Conjunct", tn[i], sep = " ")
      if(length(grep("~",tn[i])) > 0){tn[i]<-unlist(gsub('\\~', '', tn[i]))}
      X <- data1[toupper(tn[i])]
      if (length(tn)==1) {
        codata <- X
        codata1<-X
        names(codata1)[1]<-"a"
        codata$term<- X[,1]
      }
      else{
        # dataframe of the complementary conjuncts
        co<- tn[-grep(tn[i], tn)]
        co<- toupper(co)
        co <- unlist(gsub('\\~', '', co))
        codata<-data1[co]
        if(ncol(codata)>1){
          a<-do.call(pmin, codata[,])
          codata1<-data.frame(a)
          row.names(codata1)<-row.names(codata)}
        else{
          codata1<-codata
          names(codata1)[1]<-"a"}
        codata$term<-pmin(codata1$a,X[,]) 
      }
      
      typical <-(codata$term>0.5) & (Y>0.5) & (codata$term<=Y)
      fc <- X[,toupper(tn[i]), drop=FALSE]
      consfc <-(fc<=Y)
      #typ1 <- (X <= codata1$a)
      #typ2 <- (X > codata1$a)
      
      ty <- rownames(data1)[typical]
      cfc <- rownames(data1)[consfc]
      if (identical(ty, character(0))) {M[[i]] <-list(title=focconj, results="no typical cases")}
      else {
        K <- expand.grid(ty, ty)
        x <- X[,toupper(tn[i])]
        y <- Y[,outcome]
        if(length(tn)==1){mincc <- rep(0,length(codata1[,"a"]))}
        else{mincc <- codata1[,"a"]}
        term <- codata[,"term"]
        aux.f <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            s <- ((0.5-(x[i]-x[j]))+ #big diff. in FC
                    (0.5-(y[i]-y[j]))+ #big diff in Y
                    abs(mincc[i]-mincc[j])+ #small diff in complementary conj.
                    2*abs(y[i]-x[i])+ 
                    2*abs(y[j]-x[j]))
            return(s)
          }
        aux.f2 <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            sm<-(2*(y[i]-term[i]) + (1-term[i]))
            sn<-(2*(y[j]-term[j]) + (1-term[j]))     
            return(sm<=sn)
          }
        aux.f3 <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            if ((x[i] < mincc[i]) & (x[j] < mincc[j])) {order<-c(1)}
            else { if ((x[i] < mincc[i]) & (x[j] >= mincc[j])) {order<-c(2)}
              else { if ((x[i] >= mincc[i]) & (x[j] < mincc[j])) {order<-c(3)}
                else { if ((x[i] >= mincc[i]) & (x[j] >= mincc[j])) {order<-c(4)}
                }
              }
            }
            return(order)
          }  
        
        s <- apply(K, 1, aux.f)
        order <- apply(K, 1, aux.f3)
        mt <- apply(K, 1, aux.f2)
        
        matcres <- data.frame(Typical=K[,1],
                              IIR=K[,2],
                              Distance=round(s, digits=3),
                              PairRank=order,
                              Typ1moreTypical=mt)  
        
        matcres[,6] <- NA
        matcres[,7] <- NA
        colnames(matcres)<-c("Typical1","Typical2","Best","PairRank","Typ1MoreTypical","UniqCov1","UniqCov2")
        R <-cases.suf.typ(results=results, outcome=outcome, sol=sol)
        R <- R[[1]]$results
        for(u in 1:nrow(matcres)) { for (uu in 1:nrow(R)){
          if (as.character(matcres[u,1])==as.character(R[uu,1])) {matcres[u,6]<-R[uu,5]}
          if (as.character(matcres[u,2])==as.character(R[uu,1])) {matcres[u,7]<-R[uu,5]}
        }}
        maxl<-min(max_pairs,nrow(matcres))
        matcres<-matcres[order(-matcres$Typ1MoreTypical, matcres$PairRank,-matcres$UniqCov1, -matcres$UniqCov2, matcres$Best),]
        matcres <- matcres[matcres$Typ1MoreTypical==TRUE,]
        matcres <- matcres[, -c(5)]
        matcres <- matcres[matcres$Typical1!=matcres$Typical2,]
        if (length(tn)==1){matcres$PairRank <- "-"}
        matcres$ConsFC1 <- FALSE
        matcres$ConsFC2 <- FALSE
        for (h in 1:nrow(matcres)){
          if (matcres$Typical1[h] %in% cfc){matcres$ConsFC1[h] <- TRUE}
        }
        for (m in 1:nrow(matcres)){
          if (matcres$Typical2[m] %in% cfc){matcres$ConsFC2[m] <- TRUE}
        }
        matcres<-matcres[order(matcres$PairRank,-matcres$ConsFC1, -matcres$ConsFC2,-matcres$UniqCov1, -matcres$UniqCov2, matcres$Best),]
        matcres$MostTypTerm1 <- FALSE
        matcres$MostTypTerm2 <- FALSE
        mtt <- cases.suf.typ.most(results = results, outcome = outcome, sol = sol)
        mtt <- mtt[[1]]$results
        mttc <- mtt[mtt$term==colnames(pdata)[termnr],"case"]
        for (h in 1:nrow(matcres)){
          if (matcres$Typical1[h] %in% mttc){matcres$MostTypTerm1[h] <- TRUE}
        }
        for (h in 1:nrow(matcres)){
          if (matcres$Typical2[h] %in% mttc){matcres$MostTypTerm2[h] <- TRUE}
        }
        matcres$MostTypFC1 <- FALSE
        matcres$MostTypFC2 <- FALSE
        mtfc <- cases.suf.typ.fct(results = results, outcome = outcome, sol = sol, term = termnr, max_pairs = 100^100)
        mtfc <- mtfc[[i]]$results
        if(length(tn)==1){mtfcc <- rownames(mtfc)[mtfc$MostTyp==TRUE]}
        else{mtfcc <- rownames(mtfc)[mtfc$MostTypFC==TRUE]}
        for (h in 1:nrow(matcres)){
          if (matcres$Typical1[h] %in% mtfcc){matcres$MostTypFC1[h] <- TRUE}
        }
        for (h in 1:nrow(matcres)){
          if (matcres$Typical2[h] %in% mtfcc){matcres$MostTypFC2[h] <- TRUE}
        }
        matcres <- matcres[, c(1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12)]
        matcres <- matcres[1:(min(c(nrow(matcres), max_pairs))), ]
        M[[i]] <- list(title=focconj, results=(head(matcres, maxl))) 
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }


# TYP IIR - no FC
matches.suf.typiirnfc <-
  function(results,
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
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    nt <- ncol(X)-2
    tn <- colnames(X)[1:nt]
    L <- list()
    M <- list()
    for (i in 1:nt){
      term <- tn[i]
      termp <- paste("Term", tn[i], sep = " ")
      x <- X[, term]
      y <- X[, 'out']
      typical <- (x>0.5) & (y>0.5) & (x<=y) 
      iir <- (x<0.5) & (y<0.5)
      ciir <- (x<0.5) & (y<0.5) & (x<=y)
      rnt <- rownames(X)[typical]
      rni <- rownames(X)[iir]
      consiir <- rownames(X)[ciir]
      K <- expand.grid(rnt, rni) 
      if (nrow(K)>0) {
        aux.f <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            s <- ((1-(x[i]-x[j]))+ #big diff. in Term
                    (1-(y[i]-y[j]))+ #big diff in Y
                    2*abs(y[i]-x[i])+ #corridor
                    2*abs(y[j]-x[j])) #corridor
            return(s)
          }
        s <- apply(K, 1, aux.f)
        R <- data.frame(Typical=K[,1],
                        IIR=K[,2],
                        Best=s,
                        Term=rep(term, length(s))
                        )	
        R <- R[order(s), ]

        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-4]
        # Most typical:
        R$MostTyp <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$MostTyp==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical[h] %in% mtfcc){R$MostTyp[h] <- TRUE}
        }
        
        # Uniquely cov:
        
        R$UniqCov <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$UniqCov==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical[h] %in% mtfcc){R$UniqCov[h] <- TRUE}
        }
        
        # Globally irrelevant:
        R$GlobUncov<- FALSE
        for (m in 1:nrow(R)){
          if (X[R$IIR[m],"solution_formula"]<0.5){R$GlobUncov[m] <- TRUE}
        }
        colnames(R) <- c("Typical","IIR","Best","MostTyp","UniqCov","GlobUncov")
        
        R$ConsIIR <- FALSE
        for (h in 1:nrow(R)){
          if (R$IIR[h] %in% consiir){R$ConsIIR[h] <- TRUE}
        }
        
        R <- R[order(1-R$UniqCov, 1-R$ConsIIR, 1-R$GlobUncov, R$Best, 1-R$MostTyp),]
        R <- R[,c(1,2,5,6,3,4,7)]
        L[[i]]<-R[1:(min(c(nrow(R), max_pairs))), ]
        M[[i]] <- list(title=termp, results=R[1:(min(c(nrow(R), max_pairs))), ])
        class(M) <- 'matchessuf'
      } else {
        R <- data.frame(Typical=NULL,
                        IIR=NULL,
                        Best=NULL,
                        MostTyp=NULL,
                        UniqCov=NULL,
                        GlobUncov=NULL)
        R <- R[,c(1,2,5,6,3,4)]
        L[[i]]<-R
        M[[i]] <- list(title=termp, results=R)
        class(M) <- 'matchessuf'
      }
    }
    return(M)
  }

# TYP TYP - no FC
matches.suf.typtypnfc <-
  function(results,
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
    if(length(grep("~",outcome)) > 0){
      outcome<-outcome[grep("~",outcome)]
      outcome<-gsub('\\~', '', outcome)
      outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, sol=sol)
    y <- X[,"out", drop=FALSE]
    names(y) <- outcome
    nt <- ncol(X)-2
    tn <- colnames(X)[1:nt]
    L <- list()
    M <- list()
    for (i in 1:nt){
      term <- tn[i]
      termp <- paste("Term", tn[i], sep = " ")
      x <- X[, term]
      y <- X[, 'out']
      typical1 <- (x>0.5) & (y>0.5) & (x<=y) 
      typical2 <- (x>0.5) & (y>0.5) & (x<=y)
      rnt1 <- rownames(X)[typical1]
      rnt2 <- rownames(X)[typical2]
      K <- expand.grid(rnt1, rnt2) 
      if (nrow(K)>0) {
        aux.f <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            s <- ((0.5-(x[i]-x[j]))+ #big diff. in Term
                    (0.5-(y[i]-y[j]))+ #big diff in Y
                    2*abs(y[i]-x[i])+ #corridor
                    2*abs(y[j]-x[j])) #corridor
            return(s)
          }
        aux.f2 <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            sm<-(2*(y[i]-x[i]) + (1-x[i]))
            sn<-(2*(y[j]-x[j]) + (1-x[j]))     
            return(sm<=sn)
          }
        s <- apply(K, 1, aux.f)
        mt <- apply(K, 1, aux.f2)
        R <- data.frame(Typical1=K[,1],
                        Typical2=K[,2],
                        Best=s,
                        Term=rep(term, length(s)),
                        Typ1MoreTypical=mt)
        
        R <- R[R$Typ1MoreTypical==TRUE,]
        R <- R[, -c(5)]
        R <- R[R$Typical1!=R$Typical2,]
        R <- R[order(s), ]
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        R <- R[,-4]
        # UniqCov1:
        R$UniqCov1 <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$UniqCov==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical1[h] %in% mtfcc){R$UniqCov1[h] <- TRUE}
        }
        # UniqCov2:
        R$UniqCov2 <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$UniqCov==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical2[h] %in% mtfcc){R$UniqCov2[h] <- TRUE}
        }
        # Most typical1:
        R$MostTyp1 <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$MostTyp==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical1[h] %in% mtfcc){R$MostTyp1[h] <- TRUE}
        }
        # Most typical2:
        R$MostTyp2 <- FALSE
        mtfc <- cases.suf.typ(results = results, outcome = outcome, sol = sol)
        mtfc <- mtfc[[1]]$results
        mtfcc <- mtfc$case[(mtfc$MostTyp==TRUE)]       
        for (h in 1:nrow(R)){
          if (R$Typical2[h] %in% mtfcc){R$MostTyp2[h] <- TRUE}
        }
        colnames(R) <- c("Typical1","Typical2","Best","UniqCov1","UniqCov2","MostTyp1","MostTyp2")
        R <- R[order(1-R$UniqCov1,1-R$UniqCov2,R$Best,1-R$MostTyp1,1-R$MostTyp2),]
        R <- R[,c(1,2,4,5,3,6,7)]
        L[[i]]<-R[1:(min(c(nrow(R), max_pairs))), ]
        M[[i]] <- list(title=termp, results=R[1:(min(c(nrow(R), max_pairs))), ])
        class(M) <- 'matchessuf'
      } else {
        R <- data.frame(Typical1=NULL,
                        Typical2=NULL,
                        Best=NULL,
                        UniqCov1=NULL,
                        UniqCov2=NULL,
                        MostTyp1=NULL,
                        MostTyp2=NULL)
        R <- R[,c(1,2,4,5,3,6,7)]
        L[[i]]<-R
        M[[i]] <- list(title=termp, results=R)
        class(M) <- 'matchessuf'
      }
    }
    return(M)
  }
