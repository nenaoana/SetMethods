pimdata <-
function(results,
           outcome, 
           intermed=FALSE,
           sol=1)
  { outcome <- toupper(outcome)
    if (!intermed){
    s <- results$solution[[sol]]
    P <- results$pims[colnames(results$pims)%in%s]}
    else{
      s <- results$i.sol$C1P1$solution[[sol]]
      P <- results$i.sol$C1P1$pims[colnames(results$i.sol$C1P1$pims)%in%s]}
    P$solution_formula <- apply(P, 1, max)
    data <- results$tt$initial.data
    if (results$options$neg.out) {
      P$out <- 1-data[, outcome]
    } else {
      P$out <- data[, outcome]
    }
    return(P)
  }
