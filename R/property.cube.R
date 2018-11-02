property.cube <- function(data, 
                          labs = FALSE, 
                          main = "3D Property Space", 
                          xlab=NULL, 
                          ylab=NULL, 
                          zlab=NULL,
                          highlight.3d=TRUE,
                          dot.cex=0.5,
                          dot.col="black",
                          dot.srt=15)

{
s3d <- scatterplot3d(data, type="p", angle=35, scale.y=0.7, pch=16,
              xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), highlight.3d = highlight.3d,
              main=main, xlab=xlab, ylab=ylab, zlab = zlab)
if (labs){
text(s3d$xyz.convert(data), labels = rownames(data),
     cex= dot.cex, col = dot.col, srt=dot.srt)}
}
