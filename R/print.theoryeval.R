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
