# Cluster helpers:

cluster.diagnostics <-
  function( x, y, unit, cluster, necessity=FALSE, wicons = FALSE)
  {X <- xtabs( x ~ unit + cluster)
  Y <- xtabs( y ~ unit + cluster)
  p<-2
  # Consistency suff.
  con.ragin <-
    function(X, Y)
    {
      S <- (sum( apply(cbind(X, Y), 1, min) )+1e-10)/(sum(X)+1e-10)
    }
  #	Pooled consistency suff.
  con.pool <-
    function(x, y)
    {
      z <- cbind(as.vector(x), as.vector(y))
      (sum(apply(z, 1, min))+1e-10)/(sum(x)+1e-10)
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
      (sum(apply(cbind(X, Y), 1, min))+1e-10)/(sum(Y)+1e-10)
    }
  #	Pooled coverage suff.
  cvr.pool <-
    function(x, y)
    {
      z <- cbind(as.vector(x), as.vector(y))
      (sum(apply(z, 1, min))+1e-10)/(sum(y)+1e-10)
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
        sqrt(sum((((bc+1e-10)/(sum(bc)+1e-10)) - (1/J))^p))
      }
    #	d(Within, Pooled), Euclidean for p=2
    dWP <-
      function(X, Y)
      {
        N <- nrow(X)
        wc <- con.with(X, Y)
        sqrt(sum((((wc+1e-10)/(sum(wc)+1e-10)) - (1/N))^p))
      }}
  else{
    #	d(Between, Pooled), Euclidean for p=2
    dBP <-
      function(X, Y)
      {
        J <- ncol(X)
        bc <- cvr.betw(X, Y)
        sqrt(sum((((bc+1e-10)/(sum(bc)+1e-10)) - (1/J))^p))
      }
    #	d(Within, Pooled), Euclidean for p=2
    dWP <-
      function(X, Y)
      {
        N <- nrow(X)
        wc <- cvr.with(X, Y)
        sqrt(sum((((wc+1e-10)/(sum(wc)+1e-10)) - (1/N))^p))
      }
  }
  
  unit <- as.character(unit)
  cluster <- as.character(cluster)
  # Nr of cases in clusters and units:
  CNRC <- data.frame(table(cluster))
  cnrc <- paste(as.character(CNRC[,1])," (",as.character(CNRC[,2]),") ",sep = "")
  CNRU <- data.frame(table(unit))
  cnru <- paste(as.character(CNRU[,1])," (",as.character(CNRU[,2]),") ",sep = "")
  # Together:
  if (!necessity){
    r1 <- con.pool(x, y)
    r2 <- con.betw(X, Y)
    r3 <- dBP(X, Y)
    r4 <- con.with(X, Y)
    r5 <- dWP(X, Y)
    r6 <- list('pooled'  = cvr.pool(x, y),
               'between' = cvr.betw(X, Y),
               'within'  = cvr.with(X, Y))
    r7 <- cnrc
    r8 <- cnru}
  else{
    r1 <- cvr.pool(x, y)
    r2 <- cvr.betw(X, Y)
    r3 <- dBP(X, Y)
    r4 <- cvr.with(X, Y)
    r5 <- dWP(X, Y)
    r6 <- list('pooled'  = con.pool(x, y),
               'between' = con.betw(X, Y),
               'within'  = con.with(X, Y))
    r7 <- cnrc
    r8 <- cnru
  }
  r <- list('POCOS'=r1,
            'BECOS'=r2,
            'dBP'=r3,
            'WICONS'=r4,
            'dWP'=r5,
            'Coverages'=r6,
            'wiconsprint'=wicons,
            'cluster_ids' = r7,
            'unit_ids' = r8)
  
  class(r) <- 'clusterdiagnostics'
  return(r)
  }

cluster.minimize <-
  function(results,
           data,
           outcome,
           unit_id,
           cluster_id,
           sol=1,
           wicons = FALSE)
  { if(length(grep("~",outcome)) > 0){
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
        else (return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format."))
      }
      
      if (length(P)<1){P <- P[,s]}
      else {P <- P}
    }
    
    if (results$options$neg.out) {
      P$outcome <- 1 - data[, outcome]
    } else {
      P$outcome <- data[, outcome]		
    }
    
    P$unit_id <- data[, unit_id]
    P$cluster_id <- data[, cluster_id]
    
    n_c <- ncol(P)-3
    
    old_names <- names(P)[1:n_c]
    
    names(P) <- gsub('\\*', '', names(P))  
    
    aux <-
      function(i)
      {
        return(cluster.diagnostics(P[,i], P$outcome, P$unit_id, P$cluster_id, wicons = wicons))
      }
    
    O <- lapply(1:n_c, aux)
    names(O) <- old_names
    
    E <- list()
    E$output <- O
    E$unit_ids <- P$unit_id
    E$cluster_ids <- P$cluster_id	
    E$wiconsprint <- wicons
    class(E) <- 'clusterminimize'
    return(E)
  }
