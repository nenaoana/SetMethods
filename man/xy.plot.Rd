\name{xy.plot}
\alias{xy.plot}
\title{Function producing enhanced XY plots}
\description{
xy.plot produces XY plots and provides values for consistency, Haesebrouck's consistency, coverage, RoN, PRI. Several graphic parameters can be decided by the user.
}
\usage{xy.plot(x, y, 
        ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05), 
        pch = 19, col = "black", main = "XY plot", 
        ylab = "Outcome", xlab = "Condition", 
        mar = c(4, 4, 4, 1), mgp = c(2.2, 0.8, 0), 
        cex.fit = 0.6, cex.axis = 0.7, cex.main = 1, 
        necessity = FALSE, show.hv = TRUE, show.fit = TRUE, 
        pos.fit = "top", case.lab = TRUE, labs = NULL, 
        cex.lab = 0.8, offset.x = 0, offset.y = 0, 
        pos = 4, srt = 0, 
        ident = FALSE)
}
\arguments{
  \item{x}{vector containing the condition.}
  \item{y}{vector containing the outcome.}
  \item{ylim}{limits of y-axis. The default is \code{c(-0.05, 1.05)}.}
  \item{xlim}{limits of x-axis. The default is \code{c(-0.05, 1.05)}.}
  \item{pch}{plotting "character". The default is \code{19}. See ?pch.}
  \item{col}{color for the plotting "character". The default is \code{"black"}. See ?par.}
  \item{main}{an overall title for the plot. The default is \code{"XY plot"}. See ?title.}
  \item{ylab}{a title for the y-axis. The default is \code{"Outcome"}. See ?title.}
  \item{xlab}{a title for the x-axis. The default is \code{"Condition"}. See ?title.}
  \item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of lines of margin to be specified on the four sides of the plot. The default is \code{c(4, 4, 4, 1)}. See ?par.}
  \item{mgp}{The margin line (in \code{mex} units) for the axis title, axis labels and axis line. Note that \code{mgp[1]} affects title whereas \code{mgp[2:3]} affect axis. The default is \code{c(2.2, 0.8, 0)}. See ?par.}
  \item{cex.fit}{character expansion for the parameters of fit. The defaut is \code{0.6}. See ?pch or ?text.}
  \item{cex.axis}{character expansion for the x-axis and y-axis. The defaut is \code{0.7}. See ?pch or ?text.}
  \item{cex.main}{character expansion for the overall title of the plot. The defaut is \code{1}. See ?pch or ?text.}
  \item{necessity}{logical. Indicates if the parameters of fit are calculated for a sufficient or necessary condition. The default is \code{FALSE}, therefore it calculates the parameters of fit for sufficiency. To get the parameters of fit for necessary conditions set \code{necessity} as \code{TRUE}.}
  \item{show.hv}{logical. Indicates if horizontal and vertical lines at 0.5 have to be shown. The default is \code{TRUE}.}
  \item{show.fit}{logical. Indicates if parameters of fit have to be shown. The default is \code{TRUE}.}
  \item{pos.fit}{character. Indicates the position of the parameters of fit. The positions are \code{"top"}, which places the parameters of fit outsite the plotting are just below the main title, or \code{"corner"}, which places the parameters of fit in the corner of the plotting are. The default is \code{pos.fit = "top"}.}
  \item{case.lab}{logical. Indicates if cases have to be labeled. The default is \code{TRUE}.}
  \item{labs}{the vector of case labels. The default is \code{NULL}.}
  \item{cex.lab}{character expansion for case labels. The default is \code{0.8}.}
  \item{offset.x}{is a numerical value that sets the offset for case labels position on the x-axis. The default is \code{0}.}
  \item{offset.y}{is a numerical value that sets the offset for case labels position on the y-axis. The default is \code{0}.}
  \item{pos}{a position specifier for the case labels. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates. The default is \code{4}. See ?text.}
  \item{srt}{indicates the rotation of the case labels in degrees. The default is \code{0}. See ?par.}
  \item{ident}{logical. Indicates if \code{identify()} has to be used to label the cases. When set to \code{TRUE} \code{case.lab} has to be \code{FALSE} and labels have to be provided by the user. The defaults is \code{FALSE}. See ?identify.}
}
%\details{}
\value{It returns an enhanced XY plot.}
\references{

Haesebrouck, T. (2015) Pitfalls in QCA's consistency measure. Journal of Comparative Politics 2:65-80.

Ragin, C. C. (2008) Redesigning Social Inquiry: Fuzzy Sets and Beyond. The Chicago University Press: Chicago and London.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge Univeristy Press: Cambridge.

Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}
}
\author{Mario Quaranta and Ioana-Elena Oana.}
%\note{}
%\seealso{}
\examples{
# Generate fake data
set.seed(123)
x <- runif(40, 0, 1)
y <- runif(40, 0, 1)

# Default
xy.plot(x, y)

# With labels
xy.plot(x, y, case.lab = TRUE, labs = 1:40)

# With labels and bigger measures of fit
xy.plot(x, y, case.lab = TRUE, labs = 1:40, cex.fit = 1)

# With labels and bigger title
xy.plot(x, y, case.lab = TRUE, labs = 1:40, cex.main = 1.5)

# Generate fake data the have perfect sufficiency
set.seed(123)
x <- runif(50, 0, 1)
y <- runif(50, 0, 1)

for(i in 1:length(y)) {
    while(x[i] > y[i]) {
        y[i] <- runif(1, 0, 1)
        x[i] <- runif(1, 0, 1)
    }
}

# Default
xy.plot(x, y)
}
