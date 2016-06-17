xy.plot <-
function(x, y,
           ylim = c(-.05, 1.05), xlim = c(-.05, 1.05),
           pch = 19, col = "black",
           main = "XY plot", ylab = "Outcome", xlab = "Condition",
           mar = c(4, 4, 4, 1), mgp = c(2.2, .8, 0),
           cex.fit = .6, cex.axis = .7, cex.main = 1,
           necessity = FALSE, 
           show.hv = TRUE, show.fit = TRUE, pos.fit = "top",
           case.lab = TRUE, labs = NULL, cex.lab = .8, 
           offset.x = 0, offset.y = 0, 
           pos = 4, srt = 0,
           ident = FALSE)
{	
    
    par(mar = mar, mgp = mgp)
    plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
         axes = FALSE, pch = pch, main = main, cex.main = cex.main,
         col = col)
    axis(1, at = seq(0, 1, .1), labels = seq(0, 1, .1),
         cex.axis = cex.axis)
    axis(2, at = seq(0, 1, .1), labels = seq(0, 1, .1),
         cex.axis = cex.axis, las=2)
    box(); abline(0, 1); 
    
    if (show.hv == TRUE) {
      abline(v = .5, lty = 2); abline(h = .5, lty = 2)
    }
    
    if (necessity == TRUE) {   
      # Necessity
      con <- sum(pmin(x, y))/sum(y)
      cov <- sum(pmin(x, y))/sum(x)
      ron <- sum(1-x)/sum(1-pmin(x, y))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      rons <- format(ron, digits = 3)
      storage.mode(rons) <- "numeric"
      lab <- sprintf("Consistency Necessity: %.3f; Coverage Necessity: %.3f; Relevance Necessity: %.3f", con, cov, ron)
      cons.c <- paste("Consistency Necessity",
                      cons, sep = ": ")
      cove.c <- paste("Coverage Necessity",
                      cove, sep = ": ")
      rons.c<- paste("Relevance Necessity",
                      rons, sep = ": ")
      
    } else {
      # Sufficiency
      con <- sum(pmin(x, y))/sum(x)
      cov <- sum(pmin(x, y))/sum(y)
      pri <- (sum(pmin(x,y))-sum(pmin(x,y,1-y)))/(sum(x)-sum(pmin(x,y,1-y)))
      cons <- format(con, digits = 3)
      storage.mode(cons) <- "numeric"
      cove <- format(cov, digits = 3)
      storage.mode(cove) <- "numeric"
      pris <- format(pri, digits = 3)
      storage.mode(pris) <- "numeric"
      lab <- sprintf("Consistency Sufficiency: %.3f; Coverage Sufficiency: %.3f; PRI: %.3f", con, cov, pri)
      pris.c<- paste("PRI",
                     pris, sep = ": ")
    }
    
    if (show.fit == TRUE) {
      if (pos.fit == "top") {	
        mtext(lab, line = 0.3, cex = cex.fit)
      }
      if (pos.fit == "corner") {
        text(-.05, 1.05, cons, cex = cex.fit, adj = 0)
        text(1.05, -.05, cove, cex = cex.fit, adj = 1)
      }
    }
    
    if (case.lab == TRUE) {
      text(x + offset.x, y + offset.y, 
           labels = labs, cex = cex.lab, pos = pos, srt = srt)
    } 
    
    if (ident == TRUE) {
      id <- identify(x, y, labels = labs, cex = cex.lab)
    }
  }
