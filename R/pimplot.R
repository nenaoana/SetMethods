pimplot <-
function(data,
         results,
           outcome,
           incl.tt=NULL,
           necessity=FALSE,
           intermed=FALSE,
           sol=1,			
           case_labels=TRUE,
           lab_color=rgb(0,0,0,0.5),
           lab_jitter=FALSE)
  { if (!necessity){
    if (!incl.tt) {  
    P <- pimdata(results=results, outcome=outcome, intermed=intermed, sol=sol)
    n_c <- ncol(P)-1
    par(ask=F)
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
    }}
    
    else { # for TT rows
      oldtt <- results$tt$tt
      oldtt$incl <- as.numeric(oldtt$incl)
      newtt <- oldtt[ which(oldtt$incl>incl.tt), ]
      P <- as.data.frame(results$tt$minmat)
      P <- P[colnames(P)%in%rownames(newtt)]
      P$out <- data[,outcome]
      n_c <- ncol(P)-1
      par(ask=F)
      aux.plot <-
        function(i)
        {
          xy.plot(P[, i], P[, 'out'], xlab=paste("Row ", colnames(P)[i]), ylab=outcome)
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
  }
  
  
  else { # for necessity
    P <- results$coms
    P$out <- data[,outcome]
    n_c <- ncol(P)-1
    par(ask=F)
    aux.plot <-
      function(i)
      {
        xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=outcome, necessity = TRUE)
        if (case_labels) {
          fil <- P[,'out']>0.5
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
  }

