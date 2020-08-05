# Printing functions:

# Cases Theory Evaluation:

print.casestheoryeval <-
  function(x, ...)
  {
    for (b in 1:length(x)) {
      cat(x[[b]]$Intersection, ':\n-------------------\n\n')
      cat(x[[b]]$Boolean, '\n\n')
      cat(x[[b]]$CaseNo, '\n')
      cat(x[[b]]$CaseNo2, '\n\n')
      cat('Case Names:\n')
      cat(x[[b]]$CaseNames, '\n-------------------\n')
      cat('\n') }}  

# Theory Evaluation:

print.theoryeval <-
  function(x,...)
  { if (x$printd==TRUE){
    cat("\nDATA:\n**********************\n\n")
    print(x$data)}
    if (x$printf==TRUE){
      cat("\nFIT:\n**********************\n\n")
      print(x$fit)}
    cat("\nCASES:\n**********************\n\n")
    print(x$cases)
    cat("\n")
  }  

# Theory Intersections:

print.thintersect <-
  function(x, ...)
  {
    cat("\nT*E:\n--------------------\n\n")
    print(x$TE)
    cat("\n~T*E:\n--------------------\n\n")
    print(x$tE)
    cat("\nT*~E:\n--------------------\n\n")
    print(x$Te)
    cat("\n~T*~E:\n--------------------\n\n")
    print(x$te)
    cat('\n') }  

# Cluster Diagnostics:

print.clusterdiagnostics <-
  function(x, ...)
  { digits <- 3
  cat('Consistencies:\n--------------\n')
  cat('Pooled:\t\t', round(x$POCOS, digits=3), '\n')
  for (b in 1:length(x$BECOS)) {
    cat('Between', x$cluster_ids[b] ,':\t', round(x$BECOS[b], digits=3), '\n')	
  }
  if (x$wiconsprint == TRUE) {  
    for (w in 1:length(x$WICONS)) {
      cat('Within', x$unit_ids[w] ,':\t', round(x$WICONS[w], digits=3), '\n')	
    }}
  cat('\n')
  
  cat('Distances:\n----------\n')
  cat('Between to Pooled:\t',  round(x$dBP, digits=3), '\n')
  if (x$wiconsprint == TRUE) {  
    cat('Within to Pooled:\t',  round(x$dWP, digits=3), '\n\n')}
  
  cat('Coverages:\n----------\n')
  cat('Pooled:\t\t', round(x$Coverages$pooled, digits=3), '\n')
  for (b in 1:length(x$Coverages$between)) {
    cat('Between', x$cluster_ids[b] ,':\t', round(x$Coverages$between[b], digits=3), '\n')	
  }
  if (x$wiconsprint == TRUE) {  
    for (w in 1:length(x$Coverages$within)) {
      cat('Within', x$unit_ids[w] ,':\t', round(x$Coverages$within[w], digits=3), '\n')	
    }}
  cat('\n')}

# Cluster minimize:

print.clusterminimize <-
  function(x, ...)
  {
    digits <- 3  
    aux.pocos <- function(y) return(y$POCOS)	
    aux.becos <- function(y) return(y$BECOS)	
    aux.wicons <- function(y) return(y$WICONS)	
    aux.dBP <- function(y) return(y$dBP)	
    aux.dWP <- function(y) return(y$dWP)	
    aux.pocvr <- function(y) return(y$Coverages$pooled)	
    aux.becvr <- function(y) return(y$Coverages$between)	
    aux.wicvr <- function(y) return(y$Coverages$within)	
    
    pocos <- do.call(cbind, lapply(x$output, aux.pocos))
    becos <- do.call(cbind, lapply(x$output, aux.becos))
    wicons <- do.call(cbind, lapply(x$output, aux.wicons))
    dBP <- do.call(cbind, lapply(x$output, aux.dBP))
    dWP <- do.call(cbind, lapply(x$output, aux.dWP))
    pocvr <- do.call(cbind, lapply(x$output, aux.pocvr))
    becvr <- do.call(cbind, lapply(x$output, aux.becvr))
    wicvr <- do.call(cbind, lapply(x$output, aux.wicvr))
    
    te <- names(x$output)
    
    colnames(pocos) <- colnames(becos) <- colnames(wicons) <- te
    colnames(dWP) <- colnames(dBP) <- te
    
    rownames(pocos) <- rownames(pocvr) <- 'Pooled'
    
    # Nr of cases in clusters and units:
    CNRC <- data.frame(table(x$cluster_ids))
    cnrc <- paste(as.character(CNRC[,1])," (",as.character(CNRC[,2]),") ",sep = "")
    CNRU <- data.frame(table(x$unit_ids))
    cnru <- paste(as.character(CNRU[,1])," (",as.character(CNRU[,2]),") ",sep = "")
    
    rownames(becos) <- rownames(becvr) <- paste('Between', cnrc) 
    
    rownames(wicons) <- rownames(wicvr) <- paste('Within', cnru)
    
    rownames(dBP) <- 'From Between to Pooled'
    rownames(dWP) <- 'From Within to Pooled'
    
    if (x$wiconsprint == TRUE) {
      coses <- rbind(pocos, becos, wicons)
      dists <- rbind(dBP, dWP)
      cvres <- rbind(pocvr, becvr, wicvr)
    }
    else{
      coses <- rbind(pocos, becos)
      dists <- rbind(dBP)
      cvres <- rbind(pocvr, becvr)
    }
    
    cat('Consistencies:\n---------------\n')
    print(round(coses, digits))
    
    cat('\n\nDistances:\n')
    cat('----------\n')
    print(round(dists, digits))
    
    cat('\n\nCoverages:\n')
    cat('----------\n')
    print(round(cvres, digits))
  }

# Matching functions (matchessuf):

print.matchessuf <-
  function(x, ...)
  {
    for (b in 1:length(x)) {
      cat(x[[b]]$title, ':\n----------\n')
      print(x[[b]]$results)
      cat('\n') }}  

# Robustness intersections:

print.robtersect <-
  function(x, ...)
  {
    cat("\nIS*TS:\n--------------------\n\n")
    print(x$S1S2)
    cat("\n~IS*TS:\n--------------------\n\n")
    print(x$s1S2)
    cat("\nIS*~TS:\n--------------------\n\n")
    print(x$S1s2)
    cat("\n~IS*~TS:\n--------------------\n\n")
    print(x$s1s2)
    cat('\n') }  


