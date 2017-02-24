QCAfit <-
function(x, y, cond.lab = NULL, necessity = FALSE, neg.out = FALSE, product=FALSE){
		
	x <- as.matrix(x)
	
	v <- matrix(NA, length(x[ ,1]), length(x[1, ]))
	
	out <- matrix(NA, length(x[1, ]), 8)
	
	if(neg.out == TRUE){
		y <- 1 - y
		} 
	
	for(i in 1:length(x[1,])){
		v[, i] <- pmin(x[, i], y) 
		
		
		out[i, 1] <- sum(v[, i])/sum(x[, i]) 			# Con. suf.
		out[i, 2] <- sum(v[, i])/sum(y)		 			# Cov. suf.
		out[i, 3] <- sum(v[, i])/sum(y)		 			# Con. nec.
		out[i, 4] <- sum(v[, i])/sum(x[, i]) 			# Cov. nec.
		out[i, 5] <- sum(1 - x[, i])/sum(1 - v[, i])	# RoN
		
		p1 <- sum(pmin(x[, i], y)) - sum(pmin(x[, i], y, 1 - y))
		p2 <- sum(x[, i]) - sum(pmin(x[, i], y, 1 - y))
		p3 <- sum(v[, i] + sqrt(pmax((x[, i]-y),0)*x[, i]))
		
		out[i, 6] <- p1/p2								# PRI		
		out[i, 7] <- sum(v[, i])/p3 # Haesebrouck Consistency Measure
		out[i, 8] <- out[i, 1] * out[i, 6]				# PRODUCT
		}
	if (product == TRUE) {
	  suf <- matrix(out[, c(1:2, 6:8)], nrow = ncol(x))
	  colnames(suf) <- c("Cons.Suf", "Cov.Suf", "PRI", "Cons.Suf(H)",  "PRODUCT")					  					   
	  rownames(suf) <- cond.lab
	  suf <- format(suf, digits = 3)
	  storage.mode(suf) <- "numeric"}
	else {
	  suf <- matrix(out[, c(1:2, 6:7)], nrow = ncol(x))
	  colnames(suf) <- c("Cons.Suf", "Cov.Suf", "PRI", "Cons.Suf(H)")					  					   
	  rownames(suf) <- cond.lab
	  suf <- format(suf, digits = 3)
	  storage.mode(suf) <- "numeric"
	  }
	 	
	nec <- matrix(out[, 3:5], nrow = ncol(x))	
	colnames(nec) <- c("Cons.Nec", "Cov.Nec", "RoN")					  					   
	rownames(nec) <- cond.lab
	nec <- format(nec, digits = 3)
	storage.mode(nec) <- "numeric" 	
	
	if(necessity == FALSE){
        return(suf)
		} 
	else{
        return(nec)
		}

}
