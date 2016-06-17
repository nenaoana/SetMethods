\name{xy.plot.lat}
\alias{xy.plot.lat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xy.plot.lat(x, y, ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05), main = "", pch = 19, col = "black", cex.fit = 1, ylab = "Outcome", xlab = "Condition", pos.fit = "top", strip.cex = 0.8, necessity = FALSE, show.fit = TRUE, case.lab = FALSE, lab.pos = 4, labs = NULL, show.hv = TRUE)
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
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{cex.fit}{
%%     ~~Describe \code{cex.fit} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{pos.fit}{
%%     ~~Describe \code{pos.fit} here~~
}
  \item{strip.cex}{
%%     ~~Describe \code{strip.cex} here~~
}
  \item{necessity}{
%%     ~~Describe \code{necessity} here~~
}
  \item{show.fit}{
%%     ~~Describe \code{show.fit} here~~
}
  \item{case.lab}{
%%     ~~Describe \code{case.lab} here~~
}
  \item{lab.pos}{
%%     ~~Describe \code{lab.pos} here~~
}
  \item{labs}{
%%     ~~Describe \code{labs} here~~
}
  \item{show.hv}{
%%     ~~Describe \code{show.hv} here~~
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
    main = "", pch = 19, col = "black", cex.fit = 1, ylab = "Outcome", 
    xlab = "Condition", pos.fit = "top", strip.cex = 0.8, necessity = FALSE, 
    show.fit = TRUE, case.lab = FALSE, lab.pos = 4, labs = NULL, 
    show.hv = TRUE) 
{
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
        pof <- sprintf("Consistency Necessity: \%.3f; Coverage Necessity: \%.3f; Relevance Necessity: \%.3f", 
            con, cov, ron)
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
        pof <- sprintf("Consistency Sufficiency: \%.3f; Coverage Sufficiency: \%.3f; PRI: \%.3f", 
            con, cov, pri)
    }
    if (show.fit == TRUE) {
        if (pos.fit == "top") {
            xyplot(y ~ x | pof, ylim = ylim, xlim = xlim, main = main, 
                pch = pch, col = col, ylab = ylab, xlab = xlab, 
                strip = strip.custom(par.strip.text = list(cex = strip.cex)), 
                par.settings = list(par.strip.text = list(cex = strip.cex), 
                  strip.background = list(col = NA), layout.heights = list(strip = 1.5)), 
                scales = list(x = list(at = seq(0, 1, 0.1)), 
                  y = list(at = seq(0, 1, 0.1))), panel = function(x, 
                  y, ...) {
                  panel.abline(0, 1)
                  panel.xyplot(x, y, ...)
                  if (show.hv == TRUE) {
                    panel.abline(h = 0.5, lty = 2)
                    panel.abline(v = 0.5, lty = 2)
                  }
                  if (case.lab == TRUE) {
                    panel.text(x, y, labels = labs, pos = lab.pos)
                  }
                })
        }
        else {
            xyplot(y ~ x, ylim = ylim, xlim = xlim, main = main, 
                pch = pch, col = col, ylab = ylab, xlab = xlab, 
                strip = strip.custom(par.strip.text = list(cex = strip.cex)), 
                par.settings = list(par.strip.text = list(cex = strip.cex), 
                  strip.background = list(col = NA), layout.heights = list(strip = 1.5)), 
                scales = list(x = list(at = seq(0, 1, 0.1)), 
                  y = list(at = seq(0, 1, 0.1))), panel = function(x, 
                  y, ...) {
                  panel.abline(0, 1)
                  panel.xyplot(x, y, ...)
                  if (show.hv == TRUE) {
                    panel.abline(h = 0.5, lty = 2)
                    panel.abline(v = 0.5, lty = 2)
                  }
                  if (case.lab == TRUE) {
                    panel.text(x, y, labels = labs, pos = lab.pos)
                  }
                  panel.text(-0.02, 1.02, cons, cex = cex.fit, 
                    adj = 0)
                  panel.text(1.02, -0.02, cove, cex = cex.fit, 
                    adj = 1)
                })
        }
    }
    else {
        xyplot(y ~ x, ylim = ylim, xlim = xlim, main = main, 
            pch = pch, col = col, ylab = ylab, xlab = xlab, strip = strip.custom(par.strip.text = list(cex = 1)), 
            par.settings = list(strip.background = list(col = NA), 
                layout.heights = list(strip = 1.5)), scales = list(x = list(at = seq(0, 
                1, 0.1)), y = list(at = seq(0, 1, 0.1))), panel = function(x, 
                y, ...) {
                panel.abline(0, 1)
                panel.xyplot(x, y, ...)
                panel.abline(h = 0.5, lty = 2)
                panel.abline(v = 0.5, lty = 2)
                panel.text(x, y, labels = 1:40, pos = lab.pos)
            })
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
