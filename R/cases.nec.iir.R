cases.nec.iir <-
function(results,
		 outcome,
		 sol=1)
	{outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, sol=sol)
		y <- results$tt$initial.data[, outcome]
		CS <- results$tt$recoded.data
		CS <- CS[, -which(colnames(CS)==outcome)]
		aux <-
			function(i)
			{
				fil <- (X[,i] < 0.5) & (y < 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								case=rownames(X)[fil])
				s <- 1 - (Z$x-Z$y)/Z$x
				Z$s[s==min(s)] <- TRUE
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
				return(Z[, c(5, 4, 1, 2, 6, 3)])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		R <- R[R$term=='solution_formula', c('case', 'term_membership', outcome)]
		names(R)[2] <- 'solution_membership'
		Z <- merge(x=R, y=CS, by.x='case', by.y='row.names')
		names(Z)[4:ncol(Z)] <- paste('TT_', names(Z)[4:ncol(Z)], sep='')
		return(Z)
	}
