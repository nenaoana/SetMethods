\name{cases.suf.iir}
\alias{cases.suf.iir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cases.suf.iir(results, outcome, neg.out = FALSE, intermed = FALSE, sol = 1)
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
    CS <- results$tt$recoded.data
    CS <- CS[, -which(colnames(CS) == outcome)]
    TS <- CS
    TS[TS < 0.5] <- 1 - TS[TS < 0.5]
    CS[CS < 0.5] <- 0
    CS[CS > 0.5] <- 1
    CS["TT_row_membership"] <- do.call(pmin, TS)
    aux <- function(i) {
        fil <- (X[, i] < 0.5) & (y < 0.5)
        Z <- data.frame(x = X[fil, i], y = y[fil], s = rep(FALSE, 
            sum(fil)), term = rep(colnames(X)[i], sum(fil)), 
            case = rownames(X)[fil])
        s <- 1 - (Z$x - Z$y)/Z$x
        suppressWarnings(Z$s[s == min(s)] <- TRUE)
        Z$Sd <- s
        colnames(Z)[1:3] <- c("term_membership", outcome, "most_deviant")
        return(Z[, c(5, 4, 1, 2, 6, 3)])
    }
    R <- do.call(rbind, lapply(1:(ncol(X) - 1), aux))
    R <- R[R$term == "solution_formula", c("case", "term_membership", 
        outcome)]
    names(R)[2] <- "solution_membership"
    Z <- merge(x = R, y = CS, by.x = "case", by.y = "row.names")
    names(Z)[4:(ncol(Z) - 1)] <- paste("TT_", names(Z)[4:(ncol(Z) - 
        1)], sep = "")
    O <- subset(Z, select = 3)
    Z <- Z[, -c(3)]
    Z$Outcome <- O
    sortnames <- names(Z)[3:(ncol(Z) - 2)]
    Z <- Z[do.call("order", Z[sortnames]), ]
    if (neg.out) {
        names(Z$Outcome) <- paste("~", outcome, sep = "")
    }
    return(Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
