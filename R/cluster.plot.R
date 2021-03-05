cluster.plot <- 
  function(cluster.res,
           labs = TRUE,
           size = 5,
           angle = 0,
           wicons = FALSE)
  {
    if (class(cluster.res)!= 'clusterdiagnostics' & class(cluster.res)!= 'clusterminimize') 
      stop("cluster.res must be a result of a cluster diagnostics obtained with the cluster() function!")
    CT <- list()
    ticklab = unique(as.character(cluster.res$cluster_ids))
    xtick<-seq(1, length(ticklab), by=1)
    
    # CMIN - S
    if (class(cluster.res) == 'clusterminimize'){
    for (i in 1:length(cluster.res$output)){
     CT[[i]] <- cluster.res$output[[i]]$BECOS
     dt <- data.frame(xtick,CT[[i]])
     if (labs == TRUE) {
     p <- ggplot(dt, aes(y = dt[,2], x = factor(dt[,1]))) + 
       geom_point() +  
       ylim( 0,1) + 
       theme_classic(base_size = 16)+
       geom_hline(yintercept = cluster.res$output[[i]]$POCOS)+
       labs(title = names(cluster.res$output[i]), x="Clusters", y="Consistency")+
       theme(axis.text.x = element_text(size=size, angle=angle))+
       scale_x_discrete(breaks=xtick,
                        labels=cluster.res$output[[i]]$cluster_ids)}
     else{
       p <- ggplot(dt, aes(y = dt[,2], x = factor(dt[,1]))) + 
         geom_point() +  
         ylim( 0,1) + 
         theme_classic(base_size = 16)+
         geom_hline(yintercept = cluster.res$output[[i]]$POCOS)+
         labs(title = names(cluster.res$output[i]), x="Clusters", y="Consistency")+
         theme(axis.text.x = element_text(size=size, angle=angle))+
         scale_x_discrete(breaks=xtick,
                          labels=xtick)
     }
     print(p)
    }}
    # CMIN - E
    else{
        CT[[1]] <- cluster.res$BECOS
        dt <- data.frame(xtick,CT[[1]])
        if (labs == TRUE) {
          p <- ggplot(dt, aes(y = dt[,2], x = factor(dt[,1]))) + 
            geom_point() +  
            ylim( 0,1) + 
            theme_classic(base_size = 16)+
            geom_hline(yintercept = cluster.res$POCOS)+
            labs(title = "", x="Clusters", y="Consistency")+
            theme(axis.text.x = element_text(size=size, angle=angle))+
            scale_x_discrete(breaks=xtick,
                             labels=cluster.res$output[[i]]$cluster_ids)}
        else{
          p <- ggplot(dt, aes(y = dt[,2], x = factor(dt[,1]))) + 
            geom_point() +  
            ylim( 0,1) + 
            theme_classic(base_size = 16)+
            geom_hline(yintercept = cluster.res$POCOS)+
            labs(title = "", x="Clusters", y="Consistency")+
            theme(axis.text.x = element_text(size=size, angle=angle))+
            scale_x_discrete(breaks=xtick,
                             labels=xtick)
        }
        print(p)
    }
    
    # CMIN - S
    if (wicons == TRUE){
      CTw <- list()
      ticklabw = unique(as.character(cluster.res$unit_ids))
      xtickw<-seq(1, length(ticklabw), by=1)
      
      if (class(cluster.res) == 'clusterminimize'){
      for (i in 1:length(cluster.res$output)){
        CTw[[i]] <- cluster.res$output[[i]]$WICONS
        dtw <- data.frame(x = xtickw, y = CTw[[i]])
        dtw <- dtw[order(dtw$y),] 
        dtw$xr <- reorder(dtw$x, 1-dtw$y)
        pw <- ggplot(dtw, aes(y = dtw[,2], x = dtw[,3])) + 
          geom_point() +  
          ylim( 0,1) + 
          theme_classic(base_size = 16)+
          geom_hline(yintercept = cluster.res$output[[i]]$POCOS)+
          labs(title = names(cluster.res$output[i]), x="Units", y="Consistency")+
          theme(axis.text.x = element_blank())
        suppressWarnings(print(pw))
      }
    }
    # CMIN - E
    else{
      CTw[[1]] <- cluster.res$WICONS
      dtw <- data.frame(x = xtickw, y = CTw[[1]])
      dtw <- dtw[order(dtw$y),] 
      dtw$xr <- reorder(dtw$x, 1-dtw$y)
      pw <- ggplot(dtw, aes(y = dtw[,2], x = dtw[,3])) + 
        geom_point() +  
        ylim( 0,1) + 
        theme_classic(base_size = 16)+
        geom_hline(yintercept = cluster.res$POCOS)+
        labs(title = "", x="Units", y="Consistency")+
        theme(axis.text.x = element_blank())
      suppressWarnings(print(pw))
    }}
  }

