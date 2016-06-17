\name{cluster.eqmcc}
\alias{cluster.eqmcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cluster.eqmcc(results, data, outcome, unit_id, cluster_id, intermed = FALSE, sol = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
}
  \item{unit_id}{
%%     ~~Describe \code{unit_id} here~~
}
  \item{cluster_id}{
%%     ~~Describe \code{cluster_id} here~~
}
  \item{intermed}{
%%     ~~Describe \code{intermed} here~~
}
  \item{sol}{
%%     ~~Describe \code{sol} here~~
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
function (results, data, outcome, unit_id, cluster_id, intermed = FALSE, 
    sol = 1) 
{
    if (!intermed) {
        s <- results$solution[[sol]]
        P <- results$pims[colnames(results$pims) \%in\% s]
    }
    else {
        s <- results$i.sol$C1P1$solution[[sol]]
        P <- results$i.sol$C1P1$pims[colnames(results$i.sol$C1P1$pims) \%in\% 
            s]
    }
    if (results$options$neg.out) {
        P$outcome <- 1 - data[, outcome]
    }
    else {
        P$outcome <- data[, outcome]
    }
    P$unit_id <- data[, unit_id]
    P$cluster_id <- data[, cluster_id]
    n_c <- ncol(P) - 3
    old_names <- names(P)[1:n_c]
    names(P) <- gsub("\\*", "", names(P))
    aux <- function(i) {
        return(cluster.diagnostics(P[, i], P$outcome, P$unit_id, 
            P$cluster_id))
    }
    O <- lapply(1:n_c, aux)
    names(O) <- old_names
    E <- list()
    E$output <- O
    E$unit_ids <- P$unit_id
    E$cluster_ids <- P$cluster_id
    class(E) <- "clustereqmcc"
    return(E)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
