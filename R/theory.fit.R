theory.fit <-
function(theory_eval)
          {            
n_c <- ncol(theory_eval)-1
theory_fit <- data.frame(matrix(NA, ncol=4, nrow=0))  
for (i in (1:n_c)){
  theory_fit <- rbind(theory_fit, QCAfit(theory_eval[,i], theory_eval[, ncol(theory_eval)]))}
rownames(theory_fit) <- names(theory_eval[1:n_c])
return(theory_fit)
}
