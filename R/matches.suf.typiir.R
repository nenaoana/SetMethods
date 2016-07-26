matches.suf.typiir <-
function(results,
           outcome,
           term=1,
           neg.out=FALSE,
           intermed=FALSE,
           sol=1,
           max_pairs=5)
    
  {outcome <- toupper(outcome)
    pdata <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
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
    
    for (i in (1:length(tn)))
    { print(paste("Focal Conjunct", tn[i], sep = " "))                                          
      x <- data1[toupper(tn[i])] 
      if (!neg.out){
        y <- data[outcome]}  
      else{
        y <- 1-data[outcome]
      } 
    if (length(tn)==1) {print(paste("This term has a single condition!"))}
    else{  
      co<- tn[-grep(tn[i], tn)]
      co<- toupper(co)
      codata<-data1[co]
      if(ncol(codata)>1){
        a<-do.call(pmin, codata[,])
        codata1<-data.frame(a)
        row.names(codata1)<-row.names(codata)}
      else            {
        codata1<-codata
        names(codata1)[1]<-"a"}
      codata$term<-pmin(codata1$a,x[,])
      
      typical <-((codata$term>0.5) & (y>0.5) & (codata$term<=y))
      indirre <- ((codata$term<0.5) & (y<0.5))
      typ1 <- (x <= codata1$a)
      typ2 <- (x > codata1$a)
      iir3 <- (x < 0.5) & (codata1$a>0.5)
      iir4 <- ((x < 0.5) &  (codata1$a < 0.5) & (x <= codata1$a))
      iir5 <- ((x < 0.5) &  (codata1$a < 0.5) & (codata1$a < x))
      iir6 <- (codata1$a < 0.5) & (x>0.5)
      
      d <- rownames(data1)[typical]
      e <- rownames(data1)[indirre]
      
      if (identical(e, character(0))) {print("no individually irrelevant cases")}
      else { 
        if (identical(d, character(0))) {print("no typical cases")}
        else { 
          casesm<-c()
          casesn<-c()
          val<-c()
          order<-c()
          for (m in d) { 
            for (n in e) {
              s <- (((y[m,]-x[m,])+(y[n,]-x[n,])+(1.5-(x[m,]+x[n,])))/(x[m,]+x[n,]))
              casesm<-c(casesm,m)
              casesn<-c(casesn,n)
              val<-c(val,s)
              if ((x[m,] <= codata1[m,]) & (x[n,]<0.5) & (codata1[n,]>0.5)) {order<-c(order,1)}
              else {if ((x[m,] <= codata1[m,]) & (x[n,]<0.5) & (codata1[n,]<0.5) & (x[n,] <= codata1[n,])) {order<-c(order,2)}
                else {if ((x[m,] <= codata1[m,]) & (x[n,]<0.5) & (codata1[n,]<0.5) & (codata1[n,]< x[n,])) {order<-c(order,3)}
                  else {if ((x[m,] > codata1[m,]) & (x[n,]<0.5) & (codata1[n,]>0.5)) {order<-c(order,4)}
                    else {if ((x[m,] > codata1[m,]) & (x[n,]<0.5) & (codata1[n,]<0.5) & (x[n,] <= codata1[n,])) {order<-c(order,5)}
                      else {if ((x[m,] > codata1[m,]) & (x[n,]<0.5) & (codata1[n,]<0.5) & (codata1[n,]< x[n,])) {order<-c(order,6)}
                        else {if ((x[m,] <= codata1[m,]) & (codata1[n,]<0.5) & (x[n,]>0.5)) {order<-c(order,7)}
                          else {if ((x[m,] > codata1[m,]) & (codata1[n,]<0.5) & (x[n,]>0.5)) {order<-c(order,8)}
                          }
                        }
                      }
                    }
                  }
                }
              }
            }} 
          matcres <- data.frame(casesm, casesn, val, order)
            
          matcres[,5] <- NA
          matcres[,6] <- NA
          colnames(matcres)<-c("Typical","IIR","Distance","PairRank", "UniqCovTyp","GlobUncovIIR")
          R<-cases.suf.typ (results=results, outcome=outcome, neg.out=neg.out, intermed=intermed, sol=sol)
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
          print(head(matcres, maxl))         
        }  
      }
    }
  }
}
    
