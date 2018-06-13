QCAradar <-
  function(results,
           outcome = NULL,
           fit = FALSE,
           sol= 1)
  { 
    if (is(results,'qca')) {
    PD <- pimdata(results=results, outcome=outcome, sol=sol)
    nc <- ncol(PD)-2
    cond <- results$tt$options$conditions
    pims <- names(PD[,1:nc])
    fitdat <- QCAfit(x=results, y=outcome, sol=sol)
    rdt <- data.frame(matrix(ncol=length(cond), nrow=length(pims)))
    rdt[,] <- 1
    colnames(rdt)=cond
    for (i in 1:length(pims)){
      
      tl <- gsub('\\s', '', pims[i])
      tl <- unlist(strsplit(tl, '\\+')) 
      tl <- strsplit(tl, '\\*') 
      tn <- unique(unlist(tl))
      for (j in 1:length(tn)) { if (tn[j]==tolower(tn[j])) { tn[j] <- toupper(paste("~",tn[j],sep=""))}}
      t_neg<-character(0)
      t_pre<-character(0)
      if(length(grep("~",tn)) > 0){
        t_neg<-tn[grep("~",tn)]
        t_neg<-gsub('\\~', '', t_neg)
        t_neg<-unlist(t_neg)
        t_pre<-tn[!tn %in% tn[grep("~",tn)]]
      }
      else {t_pre <- toupper(tn)}
      
      if (length(t_pre) > 0) {
        rdt[i,t_pre] <- 2    
      }
      if (length(t_neg) > 0) {
        rdt[i,t_neg] <- 0    
      }
    }
    
    par(ask=F)
    aux.plot <-
      function(i)
      { par(mar = c(1,1,1,1))
        radarchart(rdtmod[[i]], axistype=1, 
                   
                   #custom polygon
                   pcol=rgb(0,0,0,0) , pfcol=rgb(0,0,0,0.5) , plwd=1, 
                   
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey13", seg=2, caxislabels=c(0,"-",1), cglwd=1,
                   
                   #custom labels
                   vlcex=0.7,
                   
                   if (!fit) {title(main = paste("\n",pims[i], "\n"),
                                    cex.main = 0.8,   font.main= 2, col.main= "black")}
                   else {
                     cons.c <- paste("Cons.Suf", fitdat[i,1], sep = ": ")
                     cove.c <- paste("Cov.Suf", fitdat[i,2], sep = ": ")
                     pris.c <- paste("PRI", fitdat[i,3], sep = ": ")
                     hcons.c <- paste("Cons.Suf(H)", fitdat[i,4], sep = ": ")
                     title(main = paste("\n",pims[i], "\n",cons.c,cove.c,pris.c,hcons.c,"\n"),
                           cex.main = 0.6,   font.main= 2, col.main= "black")
                   }
                   )
      }
    rdtmod <- list()
    for (i in 1:nc) {
      rdtmod[[i]] <- rdt[i,]
      rdtmod[[i]]=rbind(rep(2,6) , rep(0,6) , rdtmod[[i]])
      aux.plot(i)
    }
    ncs <- nc + 1
    pims[ncs] <- "Solution Formula"
    rdtmod[[ncs]] <- rdt[c(1:nc),]
    rdtmod[[ncs]]=rbind(rep(2,6) , rep(0,6) , rdtmod[[ncs]])
    aux.plot(ncs)
    }
    else {
      if (is.character(results)){
        tl <- gsub('\\s', '', results)
        tl <- unlist(strsplit(tl, '\\+'))
        tlp <- strsplit(tl, '\\*') 
        tnp <- unique(unlist(tlp))
        rdt <- data.frame(matrix(ncol=length(tnp), nrow=length(tl)))
        rdt[,] <- 1
        colnames(rdt) <- toupper(gsub('\\~', '', tnp))
        
      for (i in 1:length(tl)) {
        tlp <- tl
        tlp[i] <- strsplit(tl[i], '\\*') 
        tn <- unique(unlist(tlp[i]))
        for (j in 1:length(tn)) { if (tn[j]==tolower(tn[j])) { tn[j] <- toupper(paste("~",tn[j],sep=""))}}
        t_neg<-character(0)
        t_pre<-character(0)
        if(length(grep("~",tn)) > 0){
          t_neg<-tn[grep("~",tn)]
          t_neg<-gsub('\\~', '', t_neg)
          t_neg<-unlist(t_neg)
          t_pre<-tn[!tn %in% tn[grep("~",tn)]]
        }
        else {t_pre <- toupper(tn)}
        
        if (length(t_pre) > 0) {
          rdt[i,t_pre] <- 2    
        }
        if (length(t_neg) > 0) {
          rdt[i,t_neg] <- 0    
        }
      }
        
      par(ask=F)
        aux.plot <-
          function(i)
          { par(mar = c(1,1,1,1))
            radarchart(rdtmod[[i]], axistype=1, 
                       
                       #custom polygon
                       pcol=rgb(0,0,0,0) , pfcol=rgb(0,0,0,0.5) , plwd=1, 
                       
                       #custom the grid
                       cglcol="grey", cglty=1, axislabcol="grey13", seg=2, caxislabels=c(0,"-",1), cglwd=1,
                       
                       #custom labels
                       vlcex=0.7,
                       
                       if (!fit) {title(main = paste("\n",tl[i], "\n"),
                                        cex.main = 0.8,   font.main= 2, col.main= "black")}
                       else {
                         stop ("Attention! fit is set to TRUE. You cannot have the fit of a boolean expression!")
                       }
            )
          }
        
        rdtmod <- list()
        for (i in 1:length(tl)) {
          rdtmod[[i]] <- rdt[i,]
          rdtmod[[i]]=rbind(rep(2,6) , rep(0,6) , rdtmod[[i]])
          aux.plot(i)
        }
        ncs <- length(tl) + 1
        nc <- ncs-1
        tl[ncs] <- results
        rdtmod[[ncs]] <- rdt[c(1:nc),]
        rdtmod[[ncs]]=rbind(rep(2,6) , rep(0,6) , rdtmod[[ncs]])
        aux.plot(ncs)
        
      }
      else {stop("Argument results is of the wrong format! Please input a 'qca' object or a boolean expression!")}
    } 
  }
