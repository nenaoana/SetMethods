xy.plot <-
  function(x, y, data, 
           labcol = "black",
           main = "XY plot", 
           ylab = "Outcome", 
           xlab = "Condition",
           necessity = FALSE,
           jitter = FALSE, 
           font = "sans",
           fontface = "italic", 
           fontsize = 3,
           labs = rownames(data))
  { if (is.data.frame(x)){ x <- colnames(x)}
    else{
    if(length(grep("~",x)) > 0){
    x<-gsub('\\~', '', x)
    x<-unlist(x)
    data[,x] <- 1-data[,x]}}
    if (is.data.frame(y)){ y <- colnames(y)}
    else{
    if(length(grep("~",y)) > 0){
      y<-gsub('\\~', '', y)
      y<-unlist(y)
      data[,y] <- 1-data[,y]}}
    if (necessity == TRUE) {   
      # Necessity
      con <- sum(pmin(data[,x], data[,y]))/sum(data[,y])
      cov <- sum(pmin(data[,x], data[,y]))/sum(data[,x])
      ron <- sum(1-data[,x])/sum(1-pmin(data[,x], data[,y]))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      rons <- format(ron, digits = 3)
      storage.mode(rons) <- "numeric"
      lab <- sprintf("Cons.Nec: %.3f; Cov.Nec: %.3f; RoN: %.3f", con, cov, ron)
      cons.c <- paste("Cons.Nec",
                      cons, sep = ": ")
      cove.c <- paste("Cov.Nec",
                      cove, sep = ": ")
      rons.c<- paste("RoN",
                     rons, sep = ": ")
      
    } 
    else {
      # Sufficiency
      con <- sum(pmin(data[,x], data[,y]))/sum(data[,x])
      cov <- sum(pmin(data[,x], data[,y]))/sum(data[,y])
      pri <- (sum(pmin(data[,x], data[,y]))-sum(pmin(data[,x], data[,y],1-data[,y])))/(sum(data[,x])-sum(pmin(data[,x], data[,y],1-data[,y])))
      hcon <- sum(pmin(data[,x], data[,y]))/sum(pmin(data[,x], data[,y]) + sqrt(pmax((data[,x]-data[,y]), 0)*data[,x]))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      pris <- format(pri, digits = 3)
      storage.mode(pris) <- "numeric"
      hcons <- format(hcon, digits = 3)
      storage.mode(hcons) <- "numeric"
      lab <- sprintf("Cons.Suf: %.3f; Cov.Suf: %.3f; PRI: %.3f; Cons.Suf(H): %.3f", con, cov, pri, hcon)
      cons.c <- paste("Cons.Suf",
                      cons, sep = ": ")
      cove.c <- paste("Cov.Suf",
                      cove, sep = ": ")
      pris.c<- paste("PRI",
                     pris, sep = ": ")
      hcons.c<- paste("Cons.Suf(H)",
                      hcons, sep = ": ")
    }
    if (jitter == TRUE) {
  ggplot(data) +
  geom_point(aes(data[,x], data[,y]), color = 'black') +
  geom_text_repel(aes(data[,x], data[,y], label = labs),
                  size = fontsize,
                  family = font,
                  fontface = fontface,
                  segment.size = 0.04,
                  force = 0.07,
                  max.iter = 2e3) +
  xlim(0,1)+
  ylim(0,1)+
  theme_classic(base_size = 16) +
  geom_vline(xintercept = 0.5)+
    geom_vline(xintercept = 1)+
    
  geom_hline(yintercept = 0.5)+
    geom_hline(yintercept = 1)+
    
  geom_abline(intercept = 0)+
  labs(title = main, subtitle = lab , x=xlab,y=ylab)+
  theme(plot.title = element_text(family = "Palatino", color="#666666", face="bold", size=20, hjust=0)) +
  theme(axis.title = element_text(family = "Palatino", color="#666666", face="bold", size=12))+
  theme(plot.subtitle = element_text(family = "Palatino", color="#666666", face="bold", size=10)) 
    }
    else {
      ggplot(data) +
        geom_point(aes(data[,x], data[,y]), color = 'black') +
        geom_text(aes(data[,x] , data[,y],label=labs),hjust=0, vjust=0, color = labcol, fontface = fontface, size = fontsize, family = font)+
        xlim(0,1)+
        ylim(0,1)+
        theme_classic(base_size = 16) +
        geom_vline(xintercept = 0.5)+
        geom_vline(xintercept = 1)+
        
        geom_hline(yintercept = 0.5)+
        geom_hline(yintercept = 1)+
        
        geom_abline(intercept = 0)+
        labs(title = main, subtitle = lab , x=xlab,y=ylab)+
        theme(plot.title = element_text(family = "Palatino", color="#666666", face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(family = "Palatino", color="#666666", face="bold", size=12))+
        theme(plot.subtitle = element_text(family = "Palatino", color="#666666", face="bold", size=10)) 
      
    }
}

 