\name{print.clustereqmcc}
\alias{print.clustereqmcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.clustereqmcc(x, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (x, digits = 3) 
{
    aux.pocos <- function(y) return(y$POCOS)
    aux.becos <- function(y) return(y$BECOS)
    aux.wicos <- function(y) return(y$WICOS)
    aux.dBP <- function(y) return(y$dBP)
    aux.dWP <- function(y) return(y$dWP)
    aux.pocvr <- function(y) return(y$Coverages$pooled)
    aux.becvr <- function(y) return(y$Coverages$between)
    aux.wicvr <- function(y) return(y$Coverages$within)
    pocos <- do.call(cbind, lapply(x$output, aux.pocos))
    becos <- do.call(cbind, lapply(x$output, aux.becos))
    wicos <- do.call(cbind, lapply(x$output, aux.wicos))
    dBP <- do.call(cbind, lapply(x$output, aux.dBP))
    dWP <- do.call(cbind, lapply(x$output, aux.dWP))
    pocvr <- do.call(cbind, lapply(x$output, aux.pocvr))
    becvr <- do.call(cbind, lapply(x$output, aux.becvr))
    wicvr <- do.call(cbind, lapply(x$output, aux.wicvr))
    te <- names(x$output)
    colnames(pocos) <- colnames(becos) <- colnames(wicos) <- te
    colnames(dWP) <- colnames(dBP) <- te
    rownames(pocos) <- rownames(pocvr) <- "Pooled"
    rownames(becos) <- rownames(becvr) <- paste("Between", names(table(x$cluster_ids)))
    rownames(wicos) <- rownames(wicvr) <- paste("Within", names(table(x$unit_ids)))
    rownames(dBP) <- "From Between to Pooled"
    rownames(dWP) <- "From Within to Pooled"
    coses <- rbind(pocos, becos, wicos)
    dists <- rbind(dBP, dWP)
    cvres <- rbind(pocvr, becvr, wicvr)
    cat("Consistencies:\n---------------\n")
    print(round(coses, digits))
    cat("\n\nDistances:\n")
    cat("----------\n")
    print(round(dists, digits))
    cat("\n\nCoverages:\n")
    cat("----------\n")
    print(round(cvres, digits))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
