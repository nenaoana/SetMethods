cases.suf.typ.most <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 sol=1)
	{
		R <- cases.suf.typ(results, outcome, neg.out, sol)
		R <- R[[1]]$results
		M <- list()
		M[[1]] <- list(title="Most Typical Cases", results=R[R$most_typical, ])
		class(M) <- 'matchessuf'
		return(M)
	}
