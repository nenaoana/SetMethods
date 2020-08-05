QCAfit <-
function(x, y, cond.lab = NULL, necessity = TRUE, neg.out = FALSE, product = FALSE, sol=1, ttrows= c(), consH= FALSE)
  {
  # For qca objects:
  if (is(x,'QCA_min'))
  {   
      if (necessity==TRUE) stop('You cannot calculate parameters of fit for necessity for a qca sufficient solution')
      if (!is.character(y)) stop('When using qca object, the outcome must be of type character. 
                                 Please specify the outcome using its exact name in the dataframe. ')
      if (length(ttrows)>0){#for specific tt rows
        results <- x
        dt <- results$tt$initial.data
        oldtt <- results$tt$tt
        newtt <- oldtt[ttrows, ]
        P <- as.data.frame(results$tt$minmat)
        P <- P[colnames(P)%in%rownames(newtt)]
        if (results$options$neg.out | length(grep("~",results$call$outcome)) > 0) {
          P$out <- 1-dt[, y]
        } else {
          P$out <- dt[, y]
        }
        nc <- ncol(P)-1
        a <- data.frame(matrix(ncol = 4, nrow = nc))
        row.names(a) <- colnames(P[,1:nc])
        colnames(a) <- c("Cons.Suf","Cov.Suf","PRI","Cons.Suf(H)")
        for (i in 1:nc) {
          a[i,] <-QCAfit(P[,i],P$out, cond.lab = cond.lab, necessity = necessity, neg.out = neg.out, product = product, sol=sol, consH=TRUE)
        }
      }
    else { 
      X <- pimdata(results=x, outcome=y, sol=sol)
      nc <- ncol(X)-1
      a <- data.frame(matrix(ncol = 4, nrow = nc))
      row.names(a) <- colnames(X[,1:nc])
      colnames(a) <- c("Cons.Suf","Cov.Suf","PRI","Cons.Suf(H)")
      for (i in 1:nc) {
        a[i,] <-QCAfit(X[,i],X$out, cond.lab = cond.lab, necessity = necessity, neg.out = neg.out, product = product, sol=sol, consH = TRUE)
      }}
      if (consH == FALSE){a <- a[,1:length(a)-1]}
      return(a)
  }
  
  
  else {
  x <- as.matrix(x)
  if (ncol(x)>1){
    nx <- 1-x
    colnames(nx) <- paste("~",colnames(nx), sep = "")
    x <- cbind(x,nx)
  }  
	
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
	  suf <- format(suf, digits = 3)
	  storage.mode(suf) <- "numeric"}
	else {
	  suf <- matrix(out[, c(1:2, 6:7)], nrow = ncol(x))
	  colnames(suf) <- c("Cons.Suf", "Cov.Suf", "PRI", "Cons.Suf(H)")					  					   
	  suf <- format(suf, digits = 3)
	  storage.mode(suf) <- "numeric"
	  }
	 	
	nec <- matrix(out[, 3:5], nrow = ncol(x))	
	colnames(nec) <- c("Cons.Nec", "Cov.Nec", "RoN")					  					   
	nec <- format(nec, digits = 3)
	storage.mode(nec) <- "numeric" 	
	
	if (ncol(x)>1){
	rownames(suf) <- colnames(x)
	rownames(nec) <- colnames(x)}
  else {
  rownames(suf) <- cond.lab
  rownames(nec) <- cond.lab}
	if (consH == FALSE){suf <- suf[,1:ncol(suf)-1]}
	if(necessity == FALSE){
        return(suf)
		} 
	else{
        return(nec)
		}

}}
