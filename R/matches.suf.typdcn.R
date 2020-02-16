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
                        Deviant_consistency=K[,2],
                        Best=s,
                        Term=rep(term, length(s)),
                        Best_matching_pair=rep(FALSE, length(s)))	
        R <- R[order(s), ]
        R[R$Best==min(R$Best), 'Best_matching_pair'] <- TRUE
        R$Best <- round(R$Best, digits = 3)
        rownames(R) <- NULL
        L[[i]]<-R[1:(min(c(nrow(R), max_pairs))), ]
        M[[i]] <- list(title=termp, results=R[1:(min(c(nrow(R), max_pairs))), ])
        class(M) <- 'matchessuf'
      } else {
        R <- data.frame(Typical=NULL,
                        Deviant_consistency=NULL,
                        Best=NULL,
                        Term=NULL,
                        Best_matching_pair=NULL)	
        L[[i]]<-R
        M[[i]] <- list(title=termp, results=R)
        class(M) <- 'matchessuf'
      }
    }
    return(M)
  }

