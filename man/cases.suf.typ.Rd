\name{cases.suf.typ}
\alias{cases.suf.typ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cases.suf.typ(results, outcome, neg.out = FALSE, intermed = FALSE, sol = 1)
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
    sol = 1) 
{
    X <- pimdata(results = results, outcome = outcome, intermed = intermed, 
        sol = sol)
    if (!neg.out) {
        y <- results$tt$initial.data[, outcome]
    }
    else {
        y <- 1 - results$tt$initial.data[, outcome]
    }
    aux <- function(i) {
        fil <- (X[, i] > 0.5) & (y > 0.5) & (X[, i] <= y)
        Z <- data.frame(x = X[fil, i], y = y[fil], s = rep(FALSE, 
            sum(fil)), term = rep(colnames(X)[i], sum(fil)), 
            case = rownames(X)[fil])
        s <- (Z$y - Z$x)/Z$x
        suppressWarnings(Z$s[s == min(s)] <- TRUE)
        Z$St <- s
        colnames(Z)[1:3] <- c("term_membership", outcome, "most_typical")
        Z <- Z[, c(5, 4, 1, 2, 6, 3)]
        return(Z[order(Z$St), ])
    }
    R <- do.call(rbind, lapply(1:(ncol(X) - 1), aux))
    R <- R[R$term != "solution_formula", ]
    cases <- unique(R$case)
    su <- vapply(cases, function(i) sum(R[R$case == i, 3] > 0.5), 
        FUN.VALUE = numeric(1))
    R$uniquely_cov <- R$case \%in\% cases[su == 1]
    if (neg.out) {
        names(R)[names(R) == outcome] <- paste("~", outcome, 
            sep = "")
    }
    return(R)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
