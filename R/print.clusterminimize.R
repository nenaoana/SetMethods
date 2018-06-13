print.clusterminimize <-
function(x, ...)
  {
    digits <- 3  
    aux.pocos <- function(y) return(y$POCOS)	
    aux.becos <- function(y) return(y$BECOS)	
    aux.wicos <- function(y) return(y$WICOS)	
    aux.dBP <- function(y) return(y$dBP)	
    aux.dWP <- function(y) return(y$dWP)	
    aux.pocvr <- function(y) return(y$Coverages$pooled)	
    aux.becvr <- function(y) return(y$Coverages$between)	
    aux.wicvr <- function(y) return(y$Coverages$within)	
    
    pocos <- do.call(cbind, lapply(x$output, aux.pocos))
    becos <- do.call(cbind, lapply(x$output, aux.becos))
    wicos <- do.call(cbind, lapply(x$output, aux.wicos))
    dBP <- do.call(cbind, lapply(x$output, aux.dBP))
    dWP <- do.call(cbind, lapply(x$output, aux.dWP))
    pocvr <- do.call(cbind, lapply(x$output, aux.pocvr))
    becvr <- do.call(cbind, lapply(x$output, aux.becvr))
    wicvr <- do.call(cbind, lapply(x$output, aux.wicvr))
    
    te <- names(x$output)
    
    colnames(pocos) <- colnames(becos) <- colnames(wicos) <- te
    colnames(dWP) <- colnames(dBP) <- te
    
    rownames(pocos) <- rownames(pocvr) <- 'Pooled'
    
    rownames(becos) <- rownames(becvr) <- paste('Between', names(table(x$cluster_ids))) 
    
    
    rownames(wicos) <- rownames(wicvr) <- paste('Within', names(table(x$unit_ids)))
    
    rownames(dBP) <- 'From Between to Pooled'
    rownames(dWP) <- 'From Within to Pooled'
    
    coses <- rbind(pocos, becos, wicos)
    dists <- rbind(dBP, dWP)
    cvres <- rbind(pocvr, becvr, wicvr)
    
    cat('Consistencies:\n---------------\n')
    print(round(coses, digits))
    
    cat('\n\nDistances:\n')
    cat('----------\n')
    print(round(dists, digits))
    
    cat('\n\nCoverages:\n')
    cat('----------\n')
    print(round(cvres, digits))
  }
