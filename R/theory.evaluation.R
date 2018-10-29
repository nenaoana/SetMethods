theory.evaluation <-
  function(theory, 
           empirics, 
           outcome,
           sol=1,
           print.fit=FALSE,
           print.data=FALSE,
           use.tilde = TRUE)
  {
    Tdata <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = use.tilde)
    Tcases <- cases.theory.evaluation(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = use.tilde)
    Tfit <- theory.fit(Tdata)
    Tint <- theory.intersections(theory=theory, empirics=empirics, sol=sol, use.tilde = use.tilde)
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
