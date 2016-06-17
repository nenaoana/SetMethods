pimplot <-
function(results,
           outcome,
           intermed=FALSE,
           sol=1,			
           case_labels=TRUE,
           lab_color=rgb(0,0,0,0.5),
           lab_jitter=FALSE)
  {
    P <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
    n_c <- ncol(P)-1
    par(ask=T)
    aux.plot <-
      function(i)
      {
        xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=outcome)
        if (case_labels) {
          fil <- P[,i]>0.5
          if (i==n_c) { fil <- P[,i]<0.5 }
          if (lab_jitter) {
            text(jitter(P[fil, i]), jitter(P[fil, 'out']),
                 col=lab_color, rownames(P)[fil], pos=3, cex=0.75)
          } else {
            text(P[fil, i], P[fil, 'out'], col=lab_color,
                 rownames(P)[fil], pos=3, cex=0.75)
          }
          
        }	
      }
    for (i in 1:n_c) {
      aux.plot(i)
    }
  }
