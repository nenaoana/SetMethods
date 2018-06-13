matches.suf.typtyp <-
function(results,
           outcome,
           term=1,
           neg.out=FALSE,
           sol=1,
           max_pairs=5)

  {if(length(grep("~",outcome)) > 0){
    outcome<-outcome[grep("~",outcome)]
    outcome<-gsub('\\~', '', outcome)
    outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    pdata <- pimdata(results=results, outcome=outcome, sol=sol)
    nterm <- colnames(pdata[term])
    data <- results$tt$initial.data
    data1 <- data.frame(matrix(NA,ncol=0,nrow=nrow(data)))
    row.names(data1)<-row.names(data)
    tl <- gsub('\\s', '', nterm)
    tl <- strsplit(tl, '\\*')
    tn <- unique(unlist(tl))
    t_pre <- toupper(tn)[toupper(tn)==tn]
    t_neg <- toupper(tn)[tolower(tn)==tn]
    if (length(t_pre) > 0) {
      data1[t_pre] <- data[t_pre]
      colnames(data1[t_pre])<-toupper(colnames(data1[t_pre]))
    }
    if (length(t_neg) > 0) {
      data1[t_neg] <- 1 - data[t_neg]
      colnames(data1[t_neg])<-tolower(colnames(data1[t_neg]))
    }
    
    if (!neg.out){
      Y <- data[outcome]}
    else{
      Y <- 1-data[outcome]}
    
    if (length(tn)==1) {return("This term has a single condition!")}
    
    else {
      M <- list()
      
    for (i in (1:length(tn)))
    { focconj <- paste("Focal Conjunct", tn[i], sep = " ")
      X <- data1[toupper(tn[i])]
      co<- tn[-grep(tn[i], tn)]
      co<- toupper(co)
      codata<-data1[co]
      if(ncol(codata)>1){
        a<-do.call(pmin, codata[,])
        codata1<-data.frame(a)
        row.names(codata1)<-row.names(codata)}
      else{
        codata1<-codata
        names(codata1)[1]<-"a"}
      
      codata$term<-pmin(codata1$a,X[,])

      typical <-(codata$term>0.5) & (Y>0.5) & (codata$term<=Y)
      typ1 <- (X <= codata1$a)
      typ2 <- (X > codata1$a)

      ty <- rownames(data1)[typical]
      
      if (identical(ty, character(0))) {M[[i]] <-list(title=focconj, results="no typical cases")}
      else {
        K <- expand.grid(ty, ty)
        x <- X[,toupper(tn[i])]
        y <- Y[,outcome]
        mincc <- codata1[,"a"]
        term <- codata[,"term"]
        aux.f <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            s <- ((0.5-(x[i]-x[j]))+ #big diff. in FC
                    (0.5-(y[i]-y[j]))+ #big diff in Y
                    abs(mincc[i]-mincc[j])+ #small diff in complementary conj.
                    2*abs(y[i]-term[i])+ 
                    2*abs(y[j]-term[j]))
            return(s)
          }
        aux.f2 <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            sm<-((y[i]-term[i])/term[i])
            sn<-((y[j]-term[j])/term[j])     
            return(sm>=sn)
          }
        aux.f3 <-
          function(p)
          {
            i <- which(rownames(X)==p[1])
            j <- which(rownames(X)==p[2])
            if ((x[i] <= mincc[i]) & (x[j] <= mincc[j])) {order<-c(1)}
            else { if ((x[i] <= mincc[i]) & (x[j] > mincc[j])) {order<-c(2)}
              else { if ((x[i] > mincc[i]) & (x[j] <= mincc[j])) {order<-c(3)}
                else { if ((x[i] > mincc[i]) & (x[j] > mincc[j])) {order<-c(4)}
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
      colnames(matcres)<-c("Typical1","Typical2","Distance","PairRank","Typ1MoreTypical","UniqCov1","UniqCov2")
      R<-cases.suf.typ (results=results, outcome=outcome, neg.out=neg.out, sol=sol)
      R <- R[[1]]$results
      for(u in 1:nrow(matcres)) { for (uu in 1:nrow(R)){
        if (as.character(matcres[u,1])==as.character(R[uu,1])) {matcres[u,6]<-R[uu,7]}
        if (as.character(matcres[u,2])==as.character(R[uu,1])) {matcres[u,7]<-R[uu,7]}
      }}
      maxl<-min(max_pairs,nrow(matcres))
      matcres<-matcres[order(-matcres$Typ1MoreTypical, matcres$PairRank,-matcres$UniqCov1, -matcres$UniqCov2, matcres$Distance),]
      matcres <- matcres[matcres$Typ1MoreTypical==TRUE,]
      matcres <- matcres[, -c(5)]
      matcres <- matcres[matcres$Typical1!=matcres$Typical2,]
      M[[i]] <- list(title=focconj, results=(head(matcres, maxl))) 
      }
    }
      class(M) <- 'matchessuf'
      return(M)
    }
}
