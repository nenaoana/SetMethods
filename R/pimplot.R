pimplot <-
  function(data = NULL,
           results,
           outcome,
           neg.out=FALSE,
           incl.tt=NULL,
           ttrows= c(),
           necessity=FALSE,
           sol=1,			
           case_labels=TRUE,
           all_labels=FALSE,
           lab_color=rgb(0,0,0,0.5),
           lab_jitter=FALSE)
  { if(length(grep("~",outcome)) > 0){
    outcome<-outcome[grep("~",outcome)]
    outcome<-gsub('\\~', '', outcome)
    outcome<-unlist(outcome)}
    outcome <- toupper(outcome)
    if (!necessity){
      data <- results$tt$initial.data
    if (is.null(incl.tt)) {
      if (length(ttrows)>0){ #for specific tt rows
        oldtt <- results$tt$tt
        newtt <- oldtt[ttrows, ]
        P <- as.data.frame(results$tt$minmat)
        P <- P[colnames(P)%in%rownames(newtt)]
        if (results$options$neg.out) {
          P$out <- 1-data[, outcome]
        } else {
          P$out <- data[, outcome]
        }
        n_c <- ncol(P)-1
        par(ask=F)
        aux.plot <-
          function(i)
          {
            if (!neg.out){
            xy.plot(P[, i], P[, 'out'], xlab=paste("Row ", colnames(P)[i]), ylab=outcome)}
            else {xy.plot(P[, i], P[, 'out'], xlab=paste("Row ", colnames(P)[i]), ylab=paste("~",outcome))}
            if (case_labels) {
              if (all_labels) {fil <- P[,i]>=0}
              else {fil <- P[,i]>0.5}
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
      else { #for solutions
      P <- pimdata(results=results, outcome=outcome, sol=sol)
      n_c <- ncol(P)-1
      par(ask=F)
      aux.plot <-
        function(i)
        { if (!neg.out){
          xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=outcome)}
          else {xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=paste("~",outcome))}
          if (case_labels) {
            if (all_labels) {fil <- P[,i]>=0}
            else {fil <- P[,i]>0.5
            if (i==n_c) { fil <- P[,i]<0.5 }}
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
      }}}
    
    else { # for TT rows
      oldtt <- results$tt$tt
      suppressWarnings(oldtt$incl <- as.numeric(oldtt$incl))
      if (length(incl.tt)>1) {paste("You introduced more than one inclusion cut for Truth Table rows. Please introduce only one!")}
      else {
      newtt <- oldtt[ which(oldtt$incl>incl.tt), ]
      P <- as.data.frame(results$tt$minmat)
      P <- P[colnames(P)%in%rownames(newtt)]
      if (results$options$neg.out) {
        P$out <- 1-data[, outcome]
      } else {
        P$out <- data[, outcome]
      }
      n_c <- ncol(P)-1
      par(ask=F)
      aux.plot <-
        function(i)
        { if (!neg.out){
          xy.plot(P[, i], P[, 'out'], xlab=paste("Row ", colnames(P)[i]), ylab=outcome)}
          else {xy.plot(P[, i], P[, 'out'], xlab=paste("Row ", colnames(P)[i]), ylab=paste("~",outcome))}
          if (case_labels) {
            if (all_labels) {fil <- P[,i]>=0}
            else {fil <- P[,i]>0.5}
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
    }
  }
    
    
    else { # for necessity
      if (is.null(data)) stop ("For analyses of necessity you need to provide the name of the dataframe!")
      P <- results$coms
      if (neg.out) {
        P$out <- 1-data[, outcome]
      } else {
        P$out <- data[, outcome]
      }
      n_c <- ncol(P)-1
      par(ask=F)
      aux.plot <-
        function(i)
        { if (!neg.out){
          xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=outcome, necessity = TRUE)}
          else {xy.plot(P[, i], P[, 'out'], xlab=colnames(P)[i], ylab=paste("~",outcome), necessity = TRUE)}
          if (case_labels) {
            if (all_labels) {fil <- P[,'out']>=0}
            else {fil <- P[,'out']>0.5}
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
