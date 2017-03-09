cases.suf.dcn <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 intermed=FALSE,
		 sol=1)
	{outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
		if (!neg.out){
		  y <- results$tt$initial.data[, outcome]}
		else{
		  y <- 1-results$tt$initial.data[, outcome]}  
		aux <-
			function(i)
			{
				fil <- (X[,i] > 0.5) & (y < 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								case=rownames(X)[fil])
				s <- 1 - (Z$x-Z$y)/Z$x
				suppressWarnings(Z$s[s==min(s)] <- TRUE)
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
				Z<-Z[, c(5, 4, 1, 2, 6, 3)]
				return(Z[order(Z$Sd),])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[order(R$term,R$Sd,R$term_membership),]
		if (neg.out){
		  names(R)[names(R)==outcome]<- paste("~", outcome, sep="")}
		return(R[R$term!='solution_formula', ])
	}
