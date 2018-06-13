theory.evaluation <-
  function(theory, 
           empirics, 
           outcome,
           sol=1,
           print.intersections=TRUE,
           print.fit=FALSE,
           print.data=FALSE)
  {
    Tdata <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol)
    Tcases <- cases.theory.evaluation(Tdata)
    Tfit <- theory.fit(Tdata)
    Tint <- theory.intersections(theory=theory, empirics=empirics, sol=sol)
    T <- list()
    T <- list('printd'= print.data,
              'printf'= print.fit,
              'printi'= print.intersections,
              'data'= Tdata,
              'cases'= Tcases,
              'fit'= Tfit,
              'inters' = Tint)
    class(T) <- 'theoryeval'
    return(T)
  }
