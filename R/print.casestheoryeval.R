print.casestheoryeval <-
  function(x, ...)
  {
    for (b in 1:length(x)) {
      cat(x[[b]]$Intersection, ':\n----------\n\n')
      cat(x[[b]]$CaseNo, '\n\n')
      cat('Case Names:\n')
      cat(x[[b]]$CaseNames, '\n----------\n')
      cat('\n') }}  
