cases.suf.typ.most <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 intermed=FALSE,
		 sol=1)
	{
		R <- cases.suf.typ(results, outcome, neg.out, intermed, sol)
		return(R[R$most_typical, ])
	}
