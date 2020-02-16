cases.suf.typ.unique <-
function(results,
		 outcome,
		 sol=1,
		 ...)
	{
  dots <- list(...)
  if(length(dots) != 0){
    if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
    if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
  }	
  R <- cases.suf.typ(results, outcome, sol)
		R <- R[[1]]$results
		M<-list()
		M[[1]] <- list(title="Uniquely Covered Typical Cases", results=R[R$uniquely_cov, ])
		class(M) <- 'matchessuf'
		return(M)
	}
