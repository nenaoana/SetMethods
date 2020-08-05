theory.evaluation <-
  function(theory, 
           empirics, 
           outcome,
           sol=1,
           print.fit=FALSE,
           print.data=FALSE,
           consH = FALSE,
           ...)
  { dots <- list(...)
    if(length(dots) != 0){
    if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    Tdata <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = TRUE)
    Tcases <- cases.theory.evaluation(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = TRUE)
    Tfit <- theory.fit(Tdata, consH = consH)
    Tint <- theory.intersections(theory=theory, empirics=empirics, sol=sol, use.tilde = TRUE)
    T <- list()
    T <- list('printd'= print.data,
              'printf'= print.fit,
              'data'= Tdata,
              'cases'= Tcases,
              'fit'= Tfit,
              'inters' = Tint)
    class(T) <- 'theoryeval'
    return(T)
  }
