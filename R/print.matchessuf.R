print.matchessuf <-
  function(x, ...)
  {
    for (b in 1:length(x)) {
    cat(x[[b]]$title, ':\n----------\n')
    print(x[[b]]$results)
    cat('\n') }}  

