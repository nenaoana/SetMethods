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
