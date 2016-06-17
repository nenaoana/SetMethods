\name{cluster.diagnostics}
\alias{cluster.diagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cluster.diagnostics(x, y, unit, cluster, necessity = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
}
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
}
  \item{necessity}{
%%     ~~Describe \code{necessity} here~~
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
function (x, y, unit, cluster, necessity = FALSE) 
{
    X <- xtabs(x ~ unit + cluster)
    Y <- xtabs(y ~ unit + cluster)
    con.ragin <- function(X, Y) {
        sum(apply(cbind(X, Y), 1, min))/sum(X)
    }
    con.pool <- function(x, y) {
        z <- cbind(as.vector(x), as.vector(y))
        sum(apply(z, 1, min))/sum(x)
    }
    con.betw <- function(X, Y) {
        unlist(lapply(1:ncol(X), function(j) con.ragin(X[, j], 
            Y[, j])))
    }
    con.with <- function(X, Y) {
        unlist(lapply(1:nrow(X), function(i) con.ragin(X[i, ], 
            Y[i, ])))
    }
    cvr.ragin <- function(X, Y) {
        sum(apply(cbind(X, Y), 1, min))/sum(Y)
    }
    cvr.pool <- function(x, y) {
        z <- cbind(as.vector(x), as.vector(y))
        sum(apply(z, 1, min))/sum(y)
    }
    cvr.betw <- function(X, Y) {
        J <- ncol(X)
        unlist(lapply(1:J, function(j) cvr.ragin(X[, j], Y[, 
            j])))
    }
    cvr.with <- function(X, Y) {
        N <- nrow(X)
        unlist(lapply(1:N, function(i) cvr.ragin(X[i, ], Y[i, 
            ])))
    }
    if (!necessity) {
        p <- 2
        dBP <- function(X, Y, p = 2) {
            J <- ncol(X)
            bc <- con.betw(X, Y)
            sqrt(sum(((bc/sum(bc)) - (1/J))^p))
        }
        dWP <- function(X, Y, p = 2) {
            N <- nrow(X)
            wc <- con.with(X, Y)
            sqrt(sum(((wc/sum(wc)) - (1/N))^p))
        }
    }
    else {
        dBP <- function(X, Y, p = 2) {
            J <- ncol(X)
            bc <- cvr.betw(X, Y)
            sqrt(sum(((bc/sum(bc)) - (1/J))^p))
        }
        dWP <- function(X, Y, p = 2) {
            N <- nrow(X)
            wc <- cvr.with(X, Y)
            sqrt(sum(((wc/sum(wc)) - (1/N))^p))
        }
    }
    if (!necessity) {
        r1 <- con.pool(x, y)
        r2 <- con.betw(X, Y)
        r3 <- dBP(X, Y, p = p)
        r4 <- con.with(X, Y)
        r5 <- dWP(X, Y, p = p)
        r6 <- list(pooled = cvr.pool(x, y), between = cvr.betw(X, 
            Y), within = cvr.with(X, Y))
    }
    else {
        r1 <- cvr.pool(x, y)
        r2 <- cvr.betw(X, Y)
        r3 <- dBP(X, Y, p = p)
        r4 <- cvr.with(X, Y)
        r5 <- dWP(X, Y, p = p)
        r6 <- list(pooled = con.pool(x, y), between = con.betw(X, 
            Y), within = con.with(X, Y))
    }
    r <- list(POCOS = r1, BECOS = r2, dBP = r3, WICOS = r4, dWP = r5, 
        Coverages = r6)
    class(r) <- "clusterdiagnostics"
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
