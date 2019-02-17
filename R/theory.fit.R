theory.fit <-
function(theory_data)
          {            
n_c <- ncol(theory_data)-1
theory_fit <- data.frame(matrix(NA, ncol=4, nrow=0))  
for (i in (1:n_c)){
  theory_fit <- rbind(theory_fit, QCAfit(theory_data[,i], theory_data[, ncol(theory_data)], necessity = FALSE))}
rownames(theory_fit) <- names(theory_data[1:n_c])
return(theory_fit)
}
