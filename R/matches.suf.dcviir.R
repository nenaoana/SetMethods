matches.suf.dcviir <-
function(results,
		 outcome,
		 neg.out=FALSE,
		 intermed=FALSE,
		 sol=1,
		 max_pairs=5)
	{outcome <- toupper(outcome)
		X <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
		n <- rownames(X)
		if (!neg.out){
		  y <- results$tt$initial.data[, outcome]}
		else{
		  y <- 1-results$tt$initial.data[, outcome]}  
		FS <- results$tt$recoded.data
		FS <- FS[, -which(colnames(FS)==outcome)]
		#	get tt row membership W
		FA <- FS
		FA[FA<=0.5] <- 1 - FA[FA<=0.5]
		w <- apply(FA, 1, min)
		#
		CS <- round(results$tt$recoded.data)
		CS <- CS[, -which(colnames(CS)==outcome)]
		tt_row <- apply(CS, 1,	function(i) paste(i, collapse=''))
		x <- X[, 'solution_formula']	
		y <- X[, 'out']
		devcove <- ((x<0.5) & (y>0.5)) & (w<=y)
		indirre <- ((x<0.5) & (y<0.5)) 
		rnt <- n[devcove]
		rnd <- n[indirre]                     
		K <- expand.grid(rnt, rnd) 
		if (nrow(K)==0) {
			warning('No pairs')
		 	return(NULL)	
		}
		#	keep only same TT row pairs
		fil <- apply(K, 1, function(p) tt_row[p[1]]==tt_row[p[2]] )            
		K_fil <- K[fil, ]
		#
		aux.f <-
			function(p)
			{
				i <- which(n==p[1])
				j <- which(n==p[2])
				s <- ( (2-(w[i]+w[j])) + (1-(y[i]-y[j])) ) / (w[i]+w[j]) 
				#	2 is the maximum value of this formula. the W value must be
				#	membership in the truth table row all conditions that
				#	constitute the tt row to which the cases belong
				return(s)
			}
		s <- apply(K_fil, 1, aux.f)
		R <- data.frame(deviant_coverage=K_fil[,1],
						individually_irrelevant=K_fil[,2],
						distance=s,
						best_matching_pair=rep(FALSE, length(s)))	
		#	merge with a TT
		CS$ids <- rownames(CS)
		R <- merge(R, CS, by.x='deviant_coverage', by.y='ids')
		colnames(R)[5:ncol(R)] <- paste('TT_', colnames(R)[5:ncol(R)], sep='') 
		tt_row_fil <- apply(R[, grep('TT_', colnames(R))], 1, 
							function(r) paste(r, collapse=''))
		R <- R[order(tt_row_fil), ]
		tt_row_fil <- tt_row_fil[order(tt_row_fil)]
		#	find the best match for each TT row	
		aux.list <-
			function(x)
			{
				x <- x[order(x$distance), ]
				x[x$distance==min(x$distance), 4] <- TRUE
				return(x[1:min(c(nrow(x), max_pairs)), ])
			}
		R_list <- lapply(split(R, tt_row_fil), aux.list)
		R <- do.call(rbind, R_list)
		rownames(R) <- NULL
		return(R)
}
