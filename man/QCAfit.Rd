\name{QCAfit}
\alias{QCAfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QCAfit(x, y, cond.lab = NULL, necessity = FALSE, negation = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{cond.lab}{
%%     ~~Describe \code{cond.lab} here~~
}
  \item{necessity}{
%%     ~~Describe \code{necessity} here~~
}
  \item{negation}{
%%     ~~Describe \code{negation} here~~
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
function (x, y, cond.lab = NULL, necessity = FALSE, negation = FALSE) 
{
    x <- as.matrix(x)
    v <- matrix(NA, length(x[, 1]), length(x[1, ]))
    out <- matrix(NA, length(x[1, ]), 7)
    if (negation == TRUE) {
        y <- 1 - y
    }
    for (i in 1:length(x[1, ])) {
        v[, i] <- pmin(x[, i], y)
        out[i, 1] <- sum(v[, i])/sum(x[, i])
        out[i, 2] <- sum(v[, i])/sum(y)
        out[i, 3] <- sum(v[, i])/sum(y)
        out[i, 4] <- sum(v[, i])/sum(x[, i])
        out[i, 5] <- sum(1 - x[, i])/sum(1 - v[, i])
        p1 <- sum(pmin(x[, i], y)) - sum(pmin(x[, i], y, 1 - 
            y))
        p2 <- sum(x[, i]) - sum(pmin(x[, i], y, 1 - y))
        out[i, 6] <- p1/p2
        out[i, 7] <- out[i, 1] * out[i, 6]
    }
    suf <- matrix(out[, c(1:2, 6:7)], nrow = ncol(x))
    colnames(suf) <- c("Cons. Suf.", "Cov. Suf.", "PRI", "PRODUCT")
    rownames(suf) <- cond.lab
    suf <- format(suf, digits = 3)
    storage.mode(suf) <- "numeric"
    nec <- matrix(out[, 3:5], nrow = ncol(x))
    colnames(nec) <- c("Cons. Nec.", "Cov. Nec.", "RoN")
    rownames(nec) <- cond.lab
    nec <- format(nec, digits = 3)
    storage.mode(nec) <- "numeric"
    if (necessity == FALSE) {
        return(suf)
    }
    else {
        return(nec)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
