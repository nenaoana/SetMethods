\name{property.cube}
\alias{property.cube}
\title{Function producing a 3D scatter plot.}
\description{A function for visualizing 3D property spaces as a 3D scatter plot using the scatterplot3d package.}
\usage{
property.cube(data, labs = FALSE, 
                          main = "3D Property Space", 
                          xlab=NULL, 
                          ylab=NULL, 
                          zlab=NULL,
                          highlight.3d=TRUE,
                          dot.cex=0.5,
                          dot.col="black",
                          dot.srt=15)
}
\arguments{
  \item{data}{A dataframe with 3 conditions.}
  \item{labs}{Logical. Should the case names be printed? If set to TRUE, it will automatically print the rownames of the dataframe given.}
  \item{main}{an overall title for the plot. The default is "3D Property Space"}
  \item{xlab}{a title for the x-axis. The default is the name of the first column in the dataframe.}
  \item{ylab}{a title for the y-axis. The default is the name of the third column in the dataframe.}
  \item{zlab}{a title for the z-axis. The default is the name of the second column in the dataframe.}
  \item{highlight.3d}{Logical. Should dots be colored differently according to their position in the property space?}
  \item{dot.cex}{size of the case labels}
  \item{dot.col}{color of the case labels}
  \item{dot.srt}{rotation of the case labels}
}
%\details{}
\value{It returns an enhanced 3d scatter plot using the \code{scatterplot3d} package.}
\author{Ioana-Elena Oana}
%\note{}
%\seealso{}
\examples{

# Load the Schneider data:

data(SCHF)

# Create a property space for conditions "EMP","BARGAIN", and outcome "EXPORT":

property.cube(SCHF[,c("EMP","BARGAIN","EXPORT")])

# Create a property space for conditions 1,2, and 3 in the data together with case labels:

property.cube(SCHF[,1:3], labs=TRUE)
}