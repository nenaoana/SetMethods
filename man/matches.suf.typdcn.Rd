\name{matches.suf.typdcn}
\alias{matches.suf.typdcn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matches.suf.typdcn(results, outcome, neg.out = FALSE, intermed = FALSE, sol = 1, max_pairs = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
}
  \item{neg.out}{
%%     ~~Describe \code{neg.out} here~~
}
  \item{intermed}{
%%     ~~Describe \code{intermed} here~~
}
  \item{sol}{
%%     ~~Describe \code{sol} here~~
}
  \item{max_pairs}{
%%     ~~Describe \code{max_pairs} here~~
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
function (results, outcome, neg.out = FALSE, intermed = FALSE, 
    sol = 1, max_pairs = 5) 
{
    X <- pimdata(results = results, outcome = outcome, intermed = intermed, 
        sol = sol)
    if (!neg.out) {
        y <- results$tt$initial.data[, outcome]
    }
    else {
        y <- 1 - results$tt$initial.data[, outcome]
    }
    nt <- ncol(X) - 2
    tn <- colnames(X)[1:nt]
    L <- list()
    for (i in 1:nt) {
        term <- tn[i]
        x <- X[, term]
        y <- X[, "out"]
        typical <- (x > 0.5) & (y > 0.5) & (x <= y)
        devcons <- (x > 0.5) & (y < 0.5)
        rnt <- rownames(X)[typical]
        rnd <- rownames(X)[devcons]
        K <- expand.grid(rnt, rnd)
        if (nrow(K) > 0) {
            aux.f <- function(p) {
                i <- which(rownames(X) == p[1])
                j <- which(rownames(X) == p[2])
                s <- ((2 - (x[i] + x[j])) + (1 - (y[i] - y[j])))/(x[i] + 
                  x[j])
                return(s)
            }
            s <- apply(K, 1, aux.f)
            R <- data.frame(typical = K[, 1], deviant_consistency = K[, 
                2], distance = s, term = rep(term, length(s)), 
                best_matching_pair = rep(FALSE, length(s)))
            R <- R[order(s), ]
            R[R$distance == min(R$distance), "best_matching_pair"] <- TRUE
            rownames(R) <- NULL
            L[[i]] <- R[1:(min(c(nrow(R), max_pairs))), ]
        }
        else {
            R <- data.frame(typical = NULL, deviant_consistency = NULL, 
                distance = NULL, term = NULL, best_matching_pair = NULL)
            L[[i]] <- R
        }
    }
    return(L)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
