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
