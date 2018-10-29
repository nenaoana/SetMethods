matches.suf.typiir <-
function(results,
           outcome,
           term=1,
           neg.out=FALSE,
           sol=1,
           max_pairs=5, use.tilde = TRUE)
    
  { if(length(grep("~",outcome)) > 0){
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
      Y <- 1-data[outcome]
    } 
    if (length(tn)==1) {return("This term has a single condition!")}
    else {
      M <- list()
    for (i in (1:length(tn)))
    { 
      focconj <- paste("Focal Conjunct", tn[i], sep = " ")
      X <- data1[toupper(tn[i])] 
      co<- tn[-grep(tn[i], tn)]
      co<- toupper(co)
      codata<-data1[co] # dataframe of the complementary conjuncts
      if(ncol(codata)>1){
        a<-do.call(pmin, codata[,])
        codata1<-data.frame(a) # the minimum of the complementary conjuncts
        row.names(codata1)<-row.names(codata)}
      else            {
        codata1<-codata
        names(codata1)[1]<-"a"}
      
      codata$term<-pmin(codata1$a,X[,])
      
      typical <-((codata$term>0.5) & (Y>0.5) & (codata$term<=Y))
      indirre <- ((codata$term<0.5) & (Y<0.5))
      typ1 <- (X <= codata1$a)
      typ2 <- (X > codata1$a)
      iir3 <- (X < 0.5) & (codata1$a>0.5)
      iir4 <- ((X < 0.5) &  (codata1$a < 0.5) & (X <= codata1$a))
      iir5 <- ((X < 0.5) &  (codata1$a < 0.5) & (codata1$a < X))
      iir6 <- (codata1$a < 0.5) & (X>0.5)
      
      ty <- rownames(data1)[typical]
      ir <- rownames(data1)[indirre]
      
      if (identical(ir, character(0))) {M[[i]] <-list(title=focconj, results="no individually irrelevant cases")}
      else { 
        if (identical(ty, character(0))) {M[[i]] <-list(title=focconj, results="no typical cases")}
        else { 
          K <- expand.grid(ty, ir)
          x <- X[,toupper(tn[i])]
          y <- Y[,outcome]
          mincc <- codata1[,"a"]
          term <- codata[,"term"]
            aux.f <-
              function(p)
              {
                i <- which(rownames(X)==p[1])
                j <- which(rownames(X)==p[2])
                s <- ((1-(x[i]-x[j]))+ #big diff. in FC
                    (1-(y[i]-y[j]))+ #big diff in Y
                    abs(mincc[i]-mincc[j])+ #small diff in complementary conj.
                    2*abs(y[i]-term[i])+ 
                    2*abs(y[j]-term[j]))
                return(s)
              }
            aux.ff <-
              function(p)
              {
                i <- which(rownames(X)==p[1])
                j <- which(rownames(X)==p[2])
                if ((x[i] <= mincc[i]) & (x[j]<0.5) & (mincc[j]>0.5)) {order<-c(1)}
                else {if ((x[i] <= mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (x[j] <= mincc[j])) {order<-c(2)}
                  else {if ((x[i] <= mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (mincc[j]< x[j])) {order<-c(3)}
                    else {if ((x[i] > mincc[i]) & (x[j]<0.5) & (mincc[j]>0.5)) {order<-c(4)}
                      else {if ((x[i] > mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (x[j] <= mincc[j])) {order<-c(5)}
                        else {if ((x[i] > mincc[i]) & (x[j]<0.5) & (mincc[j]<0.5) & (mincc[j]< x[j])) {order<-c(6)}
                          else {if ((x[i] <= mincc[i]) & (mincc[j]<0.5) & (x[j]>0.5)) {order<-c(7)}
                            else {if ((x[i] > mincc[i]) & (mincc[j]<0.5) & (x[j]>0.5)) {order<-c(8)}
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
          colnames(matcres)<-c("Typical","IIR","Distance","PairRank", "UniqCovTyp","GlobUncovIIR")
          R<-cases.suf.typ (results=results, outcome=outcome, neg.out=neg.out, sol=sol)
          R <- R[[1]]$results
          for(u in 1:nrow(matcres)) { 
            for (uu in 1:nrow(R)){
              if (as.character(matcres[u,1])==as.character(R[uu,1])) 
              {matcres[u,5]<-R[uu,7]}
            }}
          for(u in 1:nrow(matcres)) { 
            for (uu in 1:nrow(pdata)){
              if (as.character(matcres[u,2])==rownames(pdata[uu,])) { 
                if (pdata[uu,"solution_formula"]< 0.5) {matcres[u,6]<-TRUE}
                else {matcres[u,6]<-FALSE}
              }
            }}
          maxl<-min(max_pairs,nrow(matcres))
          matcres<-matcres[order(matcres$PairRank,-matcres$UniqCovTyp, -matcres$GlobUncovIIR, matcres$Distance),]
          M[[i]] <- list(title=focconj, results=(head(matcres, maxl)))         
        }  
      }
    }
    class(M) <- 'matchessuf'
    return(M)
  }
}
