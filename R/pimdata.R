pimdata <-
function(results,
           outcome,
           sol=1)
  {if(length(grep("~",outcome)) > 0){
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
      
      if (length(P)<1){P <- P[,s]}
      else {P <- P}
      }
    P$solution_formula <- apply(P, 1, max)
    data <- results$tt$initial.data
    if (results$options$neg.out) {
      P$out <- 1-data[, outcome]
    } else {
      P$out <- data[, outcome]
    }
    return(P)
  }