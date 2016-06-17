cluster.eqmcc <-
function(results,
           data,
           outcome,
           unit_id,
           cluster_id,
           intermed=FALSE,
           sol=1)
  {
    if (!intermed){
      s <- results$solution[[sol]]
      P <- results$pims[colnames(results$pims)%in%s]}
    else{
      s <- results$i.sol$C1P1$solution[[sol]]
      P <- results$i.sol$C1P1$pims[colnames(results$i.sol$C1P1$pims)%in%s]}
    
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
        return(cluster.diagnostics(P[,i], P$outcome, P$unit_id, P$cluster_id))
      }
    
    O <- lapply(1:n_c, aux)
    names(O) <- old_names
    
    E <- list()
    E$output <- O
    E$unit_ids <- P$unit_id
    E$cluster_ids <- P$cluster_id	
    
    class(E) <- 'clustereqmcc'
    return(E)
  }
