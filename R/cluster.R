cluster <-
  function(data=NULL,
           results,
           outcome,
           unit_id,
           cluster_id,
           sol=1,
           necessity=FALSE)
  { if (is(results,'qca')) 
    {
    if (is.null(data)) stop('You have not provided a dataframe. Please provide the dataframe in the long format to the argument data.')
    if(length(grep("~",outcome)) > 0){
    outcome<-outcome[grep("~",outcome)]
    outcome<-gsub('\\~', '', outcome)
    outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    if (is.null(results$i.sol)){
      if (is.character(sol)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
      s <- results$solution[[sol]]
      P <- results$pims[colnames(results$pims)%in%s]}
      else{
        if (is.numeric(sol)){
          s <- results$i.sol$C1P1$solution[[sol]]
          P <- results$i.sol$C1P1$pims[colnames(results$i.sol$C1P1$pims)%in%s]}
        else {
          if (is.character(sol)){
            if (!nchar(sol)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
            sol <- toupper(sol)  
            int <- as.numeric(unlist(strsplit(sol, "I"))[2])
            mod <- toupper(unlist(strsplit(sol, "I"))[1])
            if (int > length(get(mod, pos = results$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
            s <- get(mod, pos = results$i.sol)$solution[[int]]
            P <- get(mod, pos = results$i.sol)$pims[colnames(get(mod, pos = results$i.sol)$pims)%in%s]  
          }
          else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
        }
      }
      if (results$options$neg.out) {
        P$outcome <- 1 - data[,outcome]
      } else {
        P$outcome <- data[,outcome]		
      }
      if (is.character(unit_id) & is.character(unit_id)) {
        P$unit_id <- data[, unit_id]
        P$cluster_id <- data[, cluster_id]
      }
      else {
        P$unit_id <- as.character(unit_id)
        P$cluster_id <- as.character(cluster_id)
      }
      
      n_c <- ncol(P)-3
      
      old_names <- names(P)[1:n_c]
      
      names(P) <- gsub('\\*', '', names(P))  
      
      aux <-
        function(i)
        {
          return(cluster.diagnostics(P[,i], P$outcome, P$unit_id, P$cluster_id))
        }
      
      O <- lapply(1:n_c, aux)
      names(O) <- old_names
      
      E <- list()
      E$output <- O
      E$unit_ids <- P$unit_id
      E$cluster_ids <- P$cluster_id	
      
      class(E) <- 'clusterminimize'
      return(E)
  }
    else { 
          if (is.vector(results, mode="numeric")) { # when results is a vector
          if (is.character(outcome)) {
            if (is.null(data)) stop('You have not provided a dataframe. Please provide the dataframe in the argument data.')
            if(length(grep("~",outcome)) > 0){
              outcome<-outcome[grep("~",outcome)]
              outcome<-gsub('\\~', '', outcome)
              outcome<-unlist(outcome)
              outcome <- 1-data[,outcome]}
            else {outcome <- data[,outcome]}
            } # if outcome is character
           # when both are vectors;
          if (is.character(unit_id) & is.character(unit_id)) {
              if (is.null(data)) stop('You have not provided a dataframe. Please provide the dataframe in the argument data.')
            unit <- as.character(data[, unit_id])
            cluster <- as.character(data[, cluster_id])
          }
            else {
            unit <- as.character(unit_id)
            cluster <- as.character(cluster_id)
            }
           x <- as.numeric(results)
           y <- as.numeric(outcome)
           X <- xtabs( x ~ unit + cluster)
           Y <- xtabs( y ~ unit + cluster)
           p<-2
           # Consistency suff.
           con.ragin <-
             function(X, Y)
             {
               sum(apply(cbind(X, Y), 1, min) )/sum(X)
             }
           #	Pooled consistency suff.
           con.pool <-
             function(x, y)
             {
               z <- cbind(as.vector(x), as.vector(y))
               sum(apply(z, 1, min))/sum(x)
             }
           #	Between consistency suff.
           con.betw <-
             function(X, Y)
             {
               unlist(lapply(1:ncol(X), function(j) con.ragin(X[,j], Y[,j])))
             }
           #	Within consistency suff. 
           con.with <-
             function(X, Y)
             {
               unlist(lapply(1:nrow(X), function(i) con.ragin(X[i,], Y[i,])))
             }
           #	Coverage suff.
           cvr.ragin <-
             function(X, Y)
             {
               sum(apply(cbind(X, Y), 1, min))/sum(Y)
             }
           #	Pooled coverage suff.
           cvr.pool <-
             function(x, y)
             {
               z <- cbind(as.vector(x), as.vector(y))
               sum(apply(z, 1, min))/sum(y)
             }
           #	Between coverage suff.
           cvr.betw <-
             function(X, Y)
             {
               J <- ncol(X)
               unlist(lapply(1:J, function(j) cvr.ragin(X[,j], Y[,j])))
             }
           #	Within coverage suff.
           cvr.with <-
             function(X, Y)
             {
               N <- nrow(X)
               unlist(lapply(1:N, function(i) cvr.ragin(X[i,], Y[i,])))
             }
           if (!necessity){
             #	d(Between, Pooled), Euclidean for p=2
             dBP <-
               function(X, Y)
               {
                 J <- ncol(X)
                 bc <- con.betw(X, Y)
                 sqrt(sum(((bc/sum(bc)) - (1/J))^p))
               }
             #	d(Within, Pooled), Euclidean for p=2
             dWP <-
               function(X, Y)
               {
                 N <- nrow(X)
                 wc <- con.with(X, Y)
                 sqrt(sum(((wc/sum(wc)) - (1/N))^p))
               }}
           else{
             #	d(Between, Pooled), Euclidean for p=2
             dBP <-
               function(X, Y)
               {
                 J <- ncol(X)
                 bc <- cvr.betw(X, Y)
                 sqrt(sum(((bc/sum(bc)) - (1/J))^p))
               }
             #	d(Within, Pooled), Euclidean for p=2
             dWP <-
               function(X, Y)
               {
                 N <- nrow(X)
                 wc <- cvr.with(X, Y)
                 sqrt(sum(((wc/sum(wc)) - (1/N))^p))
               }
           }
           # Together:
           if (!necessity){
             r1 <- con.pool(x, y)
             r2 <- con.betw(X, Y)
             r3 <- dBP(X, Y)
             r4 <- con.with(X, Y)
             r5 <- dWP(X, Y)
             r6 <- list('pooled'  = cvr.pool(x, y),
                        'between' = cvr.betw(X, Y),
                        'within'  = cvr.with(X, Y))}
           else{
             r1 <- cvr.pool(x, y)
             r2 <- cvr.betw(X, Y)
             r3 <- dBP(X, Y)
             r4 <- cvr.with(X, Y)
             r5 <- dWP(X, Y)
             r6 <- list('pooled'  = con.pool(x, y),
                        'between' = con.betw(X, Y),
                        'within'  = con.with(X, Y))  
           }
           
           r <- list('POCOS'=r1,
                     'BECOS'=r2,
                     'dBP'=r3,
                     'WICOS'=r4,
                     'dWP'=r5,
                     'Coverages'=r6)
           
           class(r) <- 'clusterdiagnostics'
           return(r)
          }
          else { if (is.character(results)){ # when results in a boolean expression
            if (is.null(data)) stop('You have not provided a dataframe. Please provide the dataframe in the argument data.')
            tl <- gsub('\\s', '', results)
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
              PRE <- data[t_pre] ; names(PRE) <- t_pre      
            }
            if (length(t_neg) > 0) {
              NEG <- 1 - data[t_neg] ; names(NEG) <- paste("~", t_neg, sep="") 
            }
            
            if ((length(t_pre)>0)&(length(t_neg)>0)){
              ALL <- cbind(PRE, NEG)	
            } else if ((length(t_pre)>0)&(length(t_neg)==0)){
              ALL <- PRE
            } else if ((length(t_pre)==0)&(length(t_neg)>0)){
              ALL <- NEG
            } else if ((length(t_pre)==0)&(length(t_neg)==0)){
              stop('Missing results.\n')	
            }
            
            EXP <- as.data.frame(matrix(nrow=nrow(data), ncol=length(tl)))
            
            for (j in 1:length(tl)) {
              if (length(tl[[j]])>1){
                EXP[, j] <- apply(ALL[, tl[[j]]], 1, min)
              }
              else {EXP[, j] <- ALL[, tl[[j]]] }
            }
            exp <- apply(EXP, 1, max)
            
            if (is.character(outcome)) {
              if(length(grep("~",outcome)) > 0){
                outcome<-outcome[grep("~",outcome)]
                outcome<-gsub('\\~', '', outcome)
                outcome<-unlist(outcome)
                outcome <- 1-data[,outcome]}
              else {outcome <- data[,outcome]}
            } # if outcome is character
            # when both are vectors;
            if (is.character(unit_id) & is.character(unit_id)) {
              unit <- as.character(data[, unit_id])
              cluster <- as.character(data[, cluster_id])
            }
            else {
              unit <- as.character(unit_id)
              cluster <- as.character(cluster_id)
            } # if unit and cluster are vectors
            
              x <- as.numeric(exp)
              y <- as.numeric(outcome)
              X <- xtabs( x ~ unit + cluster)
              Y <- xtabs( y ~ unit + cluster)
              p<-2
              # Consistency suff.
              con.ragin <-
                function(X, Y)
                {
                  sum(apply(cbind(X, Y), 1, min))/sum(X)
                }
              #	Pooled consistency suff.
              con.pool <-
                function(x, y)
                {
                  z <- cbind(as.vector(x), as.vector(y))
                  sum(apply(z, 1, min))/sum(x)
                }
              #	Between consistency suff.
              con.betw <-
                function(X, Y)
                {
                  unlist(lapply(1:ncol(X), function(j) con.ragin(X[,j], Y[,j])))
                }
              #	Within consistency suff. 
              con.with <-
                function(X, Y)
                {
                  unlist(lapply(1:nrow(X), function(i) con.ragin(X[i,], Y[i,])))
                }
              #	Coverage suff.
              cvr.ragin <-
                function(X, Y)
                {
                  sum(apply(cbind(X, Y), 1, min))/sum(Y)
                }
              #	Pooled coverage suff.
              cvr.pool <-
                function(x, y)
                {
                  z <- cbind(as.vector(x), as.vector(y))
                  sum(apply(z, 1, min))/sum(y)
                }
              #	Between coverage suff.
              cvr.betw <-
                function(X, Y)
                {
                  J <- ncol(X)
                  unlist(lapply(1:J, function(j) cvr.ragin(X[,j], Y[,j])))
                }
              #	Within coverage suff.
              cvr.with <-
                function(X, Y)
                {
                  N <- nrow(X)
                  unlist(lapply(1:N, function(i) cvr.ragin(X[i,], Y[i,])))
                }
              if (!necessity){
                #	d(Between, Pooled), Euclidean for p=2
                dBP <-
                  function(X, Y)
                  {
                    J <- ncol(X)
                    bc <- con.betw(X, Y)
                    sqrt(sum(((bc/sum(bc)) - (1/J))^p))
                  }
                #	d(Within, Pooled), Euclidean for p=2
                dWP <-
                  function(X, Y)
                  {
                    N <- nrow(X)
                    wc <- con.with(X, Y)
                    sqrt(sum(((wc/sum(wc)) - (1/N))^p))
                  }}
              else{
                #	d(Between, Pooled), Euclidean for p=2
                dBP <-
                  function(X, Y)
                  {
                    J <- ncol(X)
                    bc <- cvr.betw(X, Y)
                    sqrt(sum(((bc/sum(bc)) - (1/J))^p))
                  }
                #	d(Within, Pooled), Euclidean for p=2
                dWP <-
                  function(X, Y)
                  {
                    N <- nrow(X)
                    wc <- cvr.with(X, Y)
                    sqrt(sum(((wc/sum(wc)) - (1/N))^p))
                  }
              }
              # Together:
              if (!necessity){
                r1 <- con.pool(x, y)
                r2 <- con.betw(X, Y)
                r3 <- dBP(X, Y)
                r4 <- con.with(X, Y)
                r5 <- dWP(X, Y)
                r6 <- list('pooled'  = cvr.pool(x, y),
                           'between' = cvr.betw(X, Y),
                           'within'  = cvr.with(X, Y))}
              else{
                r1 <- cvr.pool(x, y)
                r2 <- cvr.betw(X, Y)
                r3 <- dBP(X, Y)
                r4 <- cvr.with(X, Y)
                r5 <- dWP(X, Y)
                r6 <- list('pooled'  = con.pool(x, y),
                           'between' = con.betw(X, Y),
                           'within'  = con.with(X, Y))  
              }
              
              r <- list('POCOS'=r1,
                        'BECOS'=r2,
                        'dBP'=r3,
                        'WICOS'=r4,
                        'dWP'=r5,
                        'Coverages'=r6)
              
              class(r) <- 'clusterdiagnostics'
              return(r)
          }}
      }
}

