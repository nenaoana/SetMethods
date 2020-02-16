cases.suf.dcn <-
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
  if(length(grep("~",outcome)) > 0){
	  outcome<-outcome[grep("~",outcome)]
	  outcome<-gsub('\\~', '', outcome)
	  outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, sol=sol)
		y <- X[,"out", drop=FALSE]
		names(y) <- outcome
		aux <-
			function(i)
			{
				fil <- (X[,i] > 0.5) & (y < 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								Term=rep(colnames(X)[i], sum(fil)),
								Cases=rownames(X)[fil])
				s <- (1 - (Z$x-Z$y) + (1-Z$x))
				suppressWarnings(Z$s[s==min(s)] <- TRUE)
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('TermMembership', outcome, 'Most_deviant')
				Z<-Z[, c(5, 4, 1, 2, 6, 3)]
				Z[,c(3:5)] <- round(Z[,c(3:5)], digits = 3)
				return(Z[order(Z$Sd),])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[order(R$Term,R$Sd,R$TermMembership),]
		  names(R)[names(R)==outcome]<- "Outcome"
		  names(R)[names(R)=="Sd"]<- "Best"
		M<-list()
		M[[1]] <- list(title="Deviant Consistency Cases", results=R[R$Term!='solution_formula', ])
		class(M) <- 'matchessuf'
		return(M)
	}
