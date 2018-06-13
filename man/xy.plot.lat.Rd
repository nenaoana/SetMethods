\name{xy.plot.lat}
\alias{xy.plot.lat}
\title{Function producing enhanced XY plots with Lattice}

\description{
\code{xy.plot.lat} produces XY plots using the \code{lattice} package and provides values for consistency, Haesebrouck's consistency, coverage, RoN, and PRI. Several graphic parameters can be decided by the user.}
\usage{
xy.plot.lat(x, y, 
            ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05), 
            main = "", pch = 19, col = "black", cex.fit = 1, 
            ylab = "Outcome", xlab = "Condition", 
            pos.fit = "top", strip.cex = 0.8, 
            necessity = FALSE, show.fit = TRUE, case.lab = FALSE, 
            lab.pos = 4, labs = NULL, 
            show.hv = TRUE)
}
\arguments{
  \item{x}{vector containing the condition.}
  \item{y}{vector containing the outcome.}
  \item{ylim}{limits of y-axis. The default is \code{c(-0.05, 1.05)}.}
  \item{xlim}{limits of x-axis. The default is \code{c(-0.05, 1.05)}.}  
  \item{main}{an overall title for the plot. The default is \code{""}. See ?xyplot.}
  \item{pch}{plotting "character". The default is \code{19}. See ?pch or ?xyplot.}
  \item{col}{color for the plotting "character". The default is \code{"black"}. See ?par or ?xyplot.}
  \item{cex.fit}{character expansion for the parameters of fit. The defaut is \code{0.6}. See ?pch or ?text.}
  \item{ylab}{a title for the y-axis. The default is \code{"Outcome"}. See ?title or ?xyplot.}
  \item{xlab}{a title for the x-axis. The default is \code{"Condition"}. See ?title or ?xyplot.}
  \item{pos.fit}{character. Indicates the position of the parameters of fit. The positions are \code{"top"}, which places the parameters of fit in a strip on top of the plotting area, or \code{"corner"}, which places the parameters of fit in the corner of the plotting are. The default is \code{pos.fit = "top"}.}
  \item{strip.cex}{character expansion for the parameters of fit when \code{pos.fit = "top"}. The defaut is \code{0.8}. See ?pch or ?text.}
  \item{necessity}{
logical. Indicates if the parameters of fit are calculated for a sufficient or necessary condition. The default is \code{FALSE}, therefore it calculates the parameters of fit for sufficiency. To get the parameters of fit for necessary conditions set \code{necessity} as \code{TRUE}.}
  \item{show.fit}{
logical. Indicates if parameters of fit have to be shown. The default is \code{TRUE}.}
  \item{case.lab}{
logical. Indicates if cases have to be labeled. The default is \code{TRUE}.}
  \item{lab.pos}{
a position specifier for the case labels. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates. The default is \code{4}. See ?text.}
\item{labs}{
the vector of case labels. The default is \code{NULL}.}
\item{show.hv}{logical. Indicates if horizontal and vertical lines at 0.5 have to be shown. The default is \code{TRUE}.}
}
%\details{}
\value{It returns an enhanced XY plot using the \code{lattice} package.}
\references{
Haesebrouck, T. (2015) Pitfalls in QCA's consistency measure. Journal of Comparative Politics 2:65-80.

Ragin, C. C. (2008) Redesigning Social Inquiry: Fuzzy Sets and Beyond, The Chicago University Press: Chicago and London.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.

Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}

Sarkar, D. (2008) Lattice: Multivariate Data Visualization with R, Springer: Berlin.
}
\author{Mario Quaranta and Ioana-Elena Oana}
%\note{}
%\seealso{}
\examples{
# Generate fake data to have perfect necessity
set.seed(123)
x <- runif(60, 0, 1)
y <- runif(60, 0, 1)

for(i in 1:length(y)) {
    while(x[i] < y[i]) {
        y[i] <- runif(1, 0, 1)
        x[i] <- runif(1, 0, 1)
    }
}

# Default with blue dots and pch = 1
xy.plot.lat(x, y, pch = 1, col = "blue")

# Parameters of fit in the corners with blue dots and pch = 1
xy.plot.lat(x, y, pch = 1, col = "blue", pos.fit = "corner")

# Load the Schneider data:

data(SCHF)

# Plot of condition EMP as necessary for outcome EXPORT with case labels 
# and names for the plot and axes:

xy.plot.lat(SCHF$EMP, SCHF$EXPORT, necessity = TRUE, case.lab = TRUE, 
            labs = rownames(SCHF), main = "EMP as necessary for EXPORT", 
            ylab = "EXPORT", xlab = "EMP")
}