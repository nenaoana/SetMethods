\name{xy.plot}
\alias{xy.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xy.plot(x, y, ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05), pch = 19, col = "black", main = "XY plot", ylab = "Outcome", xlab = "Condition", mar = c(4, 4, 4, 1), mgp = c(2.2, 0.8, 0), cex.fit = 0.6, cex.axis = 0.7, cex.main = 1, necessity = FALSE, show.hv = TRUE, show.fit = TRUE, pos.fit = "top", case.lab = TRUE, labs = NULL, cex.lab = 0.8, offset.x = 0, offset.y = 0, pos = 4, srt = 0, ident = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{mar}{
%%     ~~Describe \code{mar} here~~
}
  \item{mgp}{
%%     ~~Describe \code{mgp} here~~
}
  \item{cex.fit}{
%%     ~~Describe \code{cex.fit} here~~
}
  \item{cex.axis}{
%%     ~~Describe \code{cex.axis} here~~
}
  \item{cex.main}{
%%     ~~Describe \code{cex.main} here~~
}
  \item{necessity}{
%%     ~~Describe \code{necessity} here~~
}
  \item{show.hv}{
%%     ~~Describe \code{show.hv} here~~
}
  \item{show.fit}{
%%     ~~Describe \code{show.fit} here~~
}
  \item{pos.fit}{
%%     ~~Describe \code{pos.fit} here~~
}
  \item{case.lab}{
%%     ~~Describe \code{case.lab} here~~
}
  \item{labs}{
%%     ~~Describe \code{labs} here~~
}
  \item{cex.lab}{
%%     ~~Describe \code{cex.lab} here~~
}
  \item{offset.x}{
%%     ~~Describe \code{offset.x} here~~
}
  \item{offset.y}{
%%     ~~Describe \code{offset.y} here~~
}
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{srt}{
%%     ~~Describe \code{srt} here~~
}
  \item{ident}{
%%     ~~Describe \code{ident} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05), 
    pch = 19, col = "black", main = "XY plot", ylab = "Outcome", 
    xlab = "Condition", mar = c(4, 4, 4, 1), mgp = c(2.2, 0.8, 
        0), cex.fit = 0.6, cex.axis = 0.7, cex.main = 1, necessity = FALSE, 
    show.hv = TRUE, show.fit = TRUE, pos.fit = "top", case.lab = TRUE, 
    labs = NULL, cex.lab = 0.8, offset.x = 0, offset.y = 0, pos = 4, 
    srt = 0, ident = FALSE) 
{
    par(mar = mar, mgp = mgp)
    plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
        axes = FALSE, pch = pch, main = main, cex.main = cex.main, 
        col = col)
    axis(1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), cex.axis = cex.axis)
    axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), cex.axis = cex.axis, 
        las = 2)
    box()
    abline(0, 1)
    if (show.hv == TRUE) {
        abline(v = 0.5, lty = 2)
        abline(h = 0.5, lty = 2)
    }
    if (necessity == TRUE) {
        con <- sum(pmin(x, y))/sum(y)
        cov <- sum(pmin(x, y))/sum(x)
        ron <- sum(1 - x)/sum(1 - pmin(x, y))
        cons <- format(con, digits = 3)
        storage.mode(cons) <- "numeric"
        cove <- format(cov, digits = 3)
        storage.mode(cove) <- "numeric"
        rons <- format(ron, digits = 3)
        storage.mode(rons) <- "numeric"
        lab <- sprintf("Consistency Necessity: \%.3f; Coverage Necessity: \%.3f; Relevance Necessity: \%.3f", 
            con, cov, ron)
        cons.c <- paste("Consistency Necessity", cons, sep = ": ")
        cove.c <- paste("Coverage Necessity", cove, sep = ": ")
        rons.c <- paste("Relevance Necessity", rons, sep = ": ")
    }
    else {
        con <- sum(pmin(x, y))/sum(x)
        cov <- sum(pmin(x, y))/sum(y)
        pri <- (sum(pmin(x, y)) - sum(pmin(x, y, 1 - y)))/(sum(x) - 
            sum(pmin(x, y, 1 - y)))
        cons <- format(con, digits = 3)
        storage.mode(cons) <- "numeric"
        cove <- format(cov, digits = 3)
        storage.mode(cove) <- "numeric"
        pris <- format(pri, digits = 3)
        storage.mode(pris) <- "numeric"
        lab <- sprintf("Consistency Sufficiency: \%.3f; Coverage Sufficiency: \%.3f; PRI: \%.3f", 
            con, cov, pri)
        pris.c <- paste("PRI", pris, sep = ": ")
    }
    if (show.fit == TRUE) {
        if (pos.fit == "top") {
            mtext(lab, line = 0.3, cex = cex.fit)
        }
        if (pos.fit == "corner") {
            text(-0.05, 1.05, cons, cex = cex.fit, adj = 0)
            text(1.05, -0.05, cove, cex = cex.fit, adj = 1)
        }
    }
    if (case.lab == TRUE) {
        text(x + offset.x, y + offset.y, labels = labs, cex = cex.lab, 
            pos = pos, srt = srt)
    }
    if (ident == TRUE) {
        id <- identify(x, y, labels = labs, cex = cex.lab)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
