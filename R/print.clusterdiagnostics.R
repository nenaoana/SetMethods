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
  
