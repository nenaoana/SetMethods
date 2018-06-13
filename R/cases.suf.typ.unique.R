cases.suf.typ.unique <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 sol=1)
	{
		R <- cases.suf.typ(results, outcome, neg.out, sol)
		R <- R[[1]]$results
		M<-list()
		M[[1]] <- list(title="Uniquely Covered Typical Cases", results=R[R$uniquely_cov, ])
		class(M) <- 'matchessuf'
		return(M)
	}
