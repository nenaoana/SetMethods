cases.suf.iir <-
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
		CS <- results$tt$recoded.data
		CS <- CS[, -which(colnames(CS)==outcome)]
		TS <- CS
		TS[TS<0.50]<-1-TS[TS<0.50]
		CS[CS<0.50]<-0
		CS[CS>0.50]<-1
		CS["TT_row_membership"]<-do.call(pmin,TS)
		CS["TT_row_membership"] <- round(CS["TT_row_membership"], digits = 3)
		aux <-
			function(i)
			{
				fil <- (X[,i] < 0.5) & (y < 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								Case=rownames(X)[fil])
				s <- 1 - (Z$x-Z$y)/Z$x
				suppressWarnings(Z$s[s==min(s)] <- TRUE)
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
				return(Z[, c(5, 4, 1, 2, 6, 3)])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[R$term=='solution_formula', c('Case', 'term_membership', outcome)]
		names(R)[2] <- 'Solution_membership'
		R[,c(2:3)] <- round(R[,c(2:3)], digits = 3)
		Z <- merge(x=R, y=CS, by.x='Case', by.y='row.names')
		names(Z)[4:(ncol(Z)-1)] <- paste('TT_', names(Z)[4:(ncol(Z)-1)], sep='')
		O <-subset(Z,select=3)
		Z <-Z[,-c(3)]
		Z$Outcome <- O
		sortnames<-names(Z)[3:(ncol(Z)-2)]
		Z <- Z[do.call("order", Z[sortnames]), ]
		  names(Z$Outcome)<- "Outcome"
		M <- list()
		M[[1]] <- list(title="Individually Irrelevant Cases", results=Z)
		class(M) <- 'matchessuf'
		return(M)
	}
