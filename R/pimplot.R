pimplot <-
  function(data = NULL,
           results,
           outcome,
           neg.out=FALSE,
           incl.tt=NULL,
           ttrows= c(),
           necessity=FALSE,
           sol=1,
           all_labels=FALSE,
           labcol="black", 
           jitter = FALSE,
           font = "sans",
           fontface = "italic", 
           fontsize = 3)
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
            { if (all_labels) {fil <- rownames(P)}
              else {fil <- rownames(P)
              fil[with(P, !(P[i] > 0.5))] <- ""}
              if (!neg.out){
                xy.plot(colnames(P[i]), 'out', data = P, xlab=paste("Row ", colnames(P)[i]), ylab=outcome, main="Sufficiency Plot",
                            labcol=labcol, 
                            jitter = jitter,
                            font = font,
                            fontface = fontface, 
                            fontsize = fontsize, 
                            labs = fil)}
              else {xy.plot(colnames(P[i]), 'out', data = P, xlab=paste("Row ", colnames(P)[i]), ylab=paste("~",outcome), main="Sufficiency Plot",
                                labcol=labcol, 
                                jitter = jitter,
                                font = font,
                                fontface = fontface, 
                                fontsize = fontsize, 
                                labs = fil)}
              
            }
          for (i in 1:n_c) {
            print(aux.plot(i))
          }
        }
        
        
        else { #for solutions
          P <- pimdata(results=results, outcome=outcome, sol=sol)
          n_c <- ncol(P)-1
          par(ask=F)
          aux.plot <-
            function(i)
            { if (all_labels) {fil <- rownames(P)}
              else {
              fil <- rownames(P)
              fil[with(P, !(P[i] > 0.5))] <- ""
              if (i==n_c) { fil <- rownames(P)
              fil[with(P, !(P[i] < 0.5))] <- "" }}
              if (!neg.out){
              xy.plot(colnames(P[i]), 'out', data = P, xlab=colnames(P)[i], ylab=outcome, main="Sufficiency Plot",
                labcol=labcol, 
                jitter = jitter,
                font = font,
                fontface = fontface, 
                fontsize = fontsize, 
                labs = fil)}
              else {xy.plot(colnames(P[i]), 'out', data = P, xlab=colnames(P)[i], ylab=paste("~",outcome), main="Sufficiency Plot",
                                labcol=labcol, 
                                jitter = jitter,
                                font = font,
                                fontface = fontface, 
                                fontsize = fontsize, 
                                labs = fil)}
            }
          for (i in 1:n_c) {
            print(aux.plot(i))
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
            { if (all_labels) {fil <- rownames(P)}
              else {fil <- rownames(P)
              fil[with(P, !(P[i] > 0.5))] <- ""}
              if (!neg.out){
              xy.plot(colnames(P[i]), 'out', data = P, xlab=paste("Row ", colnames(P)[i]), ylab=outcome, main="Sufficiency Plot",
                          labcol=labcol, 
                          jitter = jitter,
                          font = font,
                          fontface = fontface, 
                          fontsize = fontsize, 
                          labs = fil)}
              else {xy.plot(colnames(P[i]), 'out', data = P, xlab=paste("Row ", colnames(P)[i]), ylab=paste("~",outcome), main="Sufficiency Plot",
                                labcol=labcol, 
                                jitter = jitter,
                                font = font,
                                fontface = fontface, 
                                fontsize = fontsize, 
                                labs = fil)}
  
            }
          for (i in 1:n_c) {
            print(aux.plot(i))
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
        {  if (all_labels) {fil <- rownames(P)}
            else {fil <- rownames(P)
            fil[with(P, !(P[,'out'] > 0.5))] <- ""}
          if (!neg.out){
          xy.plot(colnames(P[i]), 'out', data = P, xlab=colnames(P)[i], ylab=outcome, necessity = TRUE, main="Necessity Plot",
                      labcol=labcol, 
                      jitter = jitter,
                      font = font,
                      fontface = fontface, 
                      fontsize = fontsize, 
                      labs = fil)}
          else {xy.plot(colnames(P[i]), 'out', data = P, xlab=colnames(P)[i], ylab=paste("~",outcome), necessity = TRUE, main="Necessity Plot",
                            labcol=labcol, 
                            jitter = jitter,
                            font = font,
                            fontface = fontface, 
                            fontsize = fontsize, 
                            labs = fil)}
  
        }
      for (i in 1:n_c) {
        print(aux.plot(i))
      }
    }
  }
