print.clusterdiagnostics <-
function(R, digits=3)
  {   cat('Consistencies:\n--------------\n')
      cat('Pooled:\t\t', round(R$POCOS, digits=3), '\n')
      for (b in 1:length(R$BECOS)) {
      cat('Between', b,':\t', round(R$BECOS[b], digits=3), '\n')	
      }
      for (w in 1:length(R$WICOS)) {
      cat('Within', w,':\t', round(R$WICOS[w], digits=3), '\n')	
      }
      cat('\n')
    
      cat('Distances:\n----------\n')
      cat('Between to Pooled:\t',  round(R$dBP, digits=3), '\n')		
      cat('Within to Pooled:\t',  round(R$dWP, digits=3), '\n\n')
    
      cat('Coverages:\n----------\n')
      cat('Pooled:\t\t', round(R$Coverages$pooled, digits=3), '\n')
      for (b in 1:length(R$Coverages$between)) {
      cat('Between', b,':\t', round(R$Coverages$between[b], digits=3), '\n')	
      }
      for (w in 1:length(R$Coverages$within)) {
      cat('Within', w,':\t', round(R$Coverages$within[w], digits=3), '\n')	
      }
      cat('\n')
  }
