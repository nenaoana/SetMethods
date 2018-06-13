cases.suf.dcv <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 sol=1)
	{ if(length(grep("~",outcome)) > 0){
	  outcome<-outcome[grep("~",outcome)]
	  outcome<-gsub('\\~', '', outcome)
	  outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, sol=sol)
		if (!neg.out){
		  y <- results$tt$initial.data[, outcome]}
		else{
		  y <- 1-results$tt$initial.data[, outcome]}
		CS <- results$tt$recoded.data
		CS <- CS[, -which(colnames(CS)==outcome)]
		TS <- CS
		TS[TS<0.50]<-1-TS[TS<0.50]
		CS[CS<0.50]<-0
		CS[CS>0.50]<-1
		CS["TT_row_membership"]<-do.call(pmin,TS)
		aux <-
			function(i)
			{
				fil <- (X[,i] < 0.5) & (y > 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								case=rownames(X)[fil], ttr=CS[rownames(X)[fil],"TT_row_membership"])
				s <- abs(Z$ttr-Z$y) + (1-Z$ttr)
				suppressWarnings(Z$s[s==min(s)] <- TRUE)
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
				return(Z[, c(5, 4, 1, 2, 7, 3)])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[R$term=='solution_formula', c('case', 'term_membership', outcome,"Sd")]
		names(R)[2] <- 'solution_membership'
		Z <- merge(x=R, y=CS, by.x='case', by.y='row.names')
		names(Z)[5:(ncol(Z)-1)] <- paste('TT_', names(Z)[5:(ncol(Z)-1)], sep='')
		O <-subset(Z,select=3)
		Z <-Z[,-c(3)]
		Z$Outcome <- O
		sortnames<-names(Z)[4:(ncol(Z)-2)]
		Z <- Z[do.call("order", c(Z[sortnames], Z["Sd"])), ]
		if (neg.out){
		  names(Z$Outcome)<- paste("~", outcome, sep="")}
		M <- list()
		M[[1]] <- list(title="Deviant Coverage Cases", results=Z)
		class(M) <- 'matchessuf'
		return(M)
}
