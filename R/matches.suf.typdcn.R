matches.suf.typdcn <-
function(results,
           outcome,
           neg.out=FALSE,
           intermed=FALSE,
           sol=1,
           max_pairs=5)
  {outcome <- toupper(outcome)
    X <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
    if (!neg.out){
      y <- results$tt$initial.data[, outcome]}
    else{
      y <- 1-results$tt$initial.data[, outcome]}
    
    nt <- ncol(X)-2
    tn <- colnames(X)[1:nt]
    L <- list()
    for (i in 1:nt){
      term <- tn[i]
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
            s <- ((2-(x[i]+x[j]))+(1-(y[i]-y[j]))) / (x[i]+x[j])
            return(s)
          }
        s <- apply(K, 1, aux.f)
        R <- data.frame(typical=K[,1],
                        deviant_consistency=K[,2],
                        distance=s,
                        term=rep(term, length(s)),
                        best_matching_pair=rep(FALSE, length(s)))	
        R <- R[order(s), ]
        R[R$distance==min(R$distance), 'best_matching_pair'] <- TRUE
        rownames(R) <- NULL
        L[[i]]<-R[1:(min(c(nrow(R), max_pairs))), ]
      } else {
        R <- data.frame(typical=NULL,
                        deviant_consistency=NULL,
                        distance=NULL,
                        term=NULL,
                        best_matching_pair=NULL)	
        L[[i]]<-R
      }
    }
    return(L)
  }
