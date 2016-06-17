\name{cases.nec.irr}
\alias{cases.nec.irr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cases.nec.irr(results, outcome, solution = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
}
  \item{solution}{
%%     ~~Describe \code{solution} here~~
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
function (results, outcome, solution = 1) 
{
    X <- pimdata(results = results, outcome = outcome, solution = solution)
    y <- results$tt$initial.data[, outcome]
    CS <- results$tt$recoded.data
    CS <- CS[, -which(colnames(CS) == outcome)]
    aux <- function(i) {
        fil <- (X[, i] < 0.5) & (y < 0.5)
        Z <- data.frame(x = X[fil, i], y = y[fil], s = rep(FALSE, 
            sum(fil)), term = rep(colnames(X)[i], sum(fil)), 
            case = rownames(X)[fil])
        s <- 1 - (Z$x - Z$y)/Z$x
        Z$s[s == min(s)] <- TRUE
        Z$Sd <- s
        colnames(Z)[1:3] <- c("term_membership", outcome, "most_deviant")
        return(Z[, c(5, 4, 1, 2, 6, 3)])
    }
    R <- do.call(rbind, lapply(1:(ncol(X) - 1), aux))
    R <- R[R$term == "solution_formula", c("case", "term_membership", 
        outcome)]
    names(R)[2] <- "solution_membership"
    Z <- merge(x = R, y = CS, by.x = "case", by.y = "row.names")
    names(Z)[4:ncol(Z)] <- paste("TT_", names(Z)[4:ncol(Z)], 
        sep = "")
    return(Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
