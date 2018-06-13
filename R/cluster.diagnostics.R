cluster.diagnostics <-
function( x, y, unit, cluster, necessity=FALSE)
  {X <- xtabs( x ~ unit + cluster)
   Y <- xtabs( y ~ unit + cluster)
   p<-2
    # Consistency suff.
    con.ragin <-
      function(X, Y)
      {
        sum( apply(cbind(X, Y), 1, min) )/sum(X)
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
