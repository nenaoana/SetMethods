cases.suf.typ <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 sol=1)
	{if(length(grep("~",outcome)) > 0){
	  outcome<-outcome[grep("~",outcome)]
	  outcome<-gsub('\\~', '', outcome)
	  outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, sol=sol)
		if (!neg.out){
		y <- results$tt$initial.data[, outcome]}
		else{
		y <- 1-results$tt$initial.data[, outcome]}  
		aux <-
			function(i)
			{
				fil <- (X[,i] > 0.5) & (y > 0.5) & (X[,i] <= y)
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								case=rownames(X)[fil])
				s <- ((Z$y-Z$x) + (1-Z$x))
				suppressWarnings(Z$s[s==min(s)] <- TRUE)
				Z$St <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_typical')
				Z<-Z[, c(5, 4, 1, 2, 6, 3)]
				return(Z[order(Z$St),])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[R$term!='solution_formula', ]
		cases <- unique(R$case)
		su <- vapply(cases, function(i) sum(R[R$case==i,3]>0.5), FUN.VALUE=numeric(1))
		R$uniquely_cov <- R$case %in% cases[su==1]
		R <- R[order(R$term,-R$uniquely_cov,R$St),]
		if (neg.out){
		  names(R)[names(R)==outcome]<- paste("~", outcome, sep="")}
		M <- list()
		M[[1]] <- list(title="Typical Cases", results=R)
		class(M) <- 'matchessuf'
		return(M)
	}
