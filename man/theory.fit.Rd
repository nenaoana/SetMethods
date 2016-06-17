\name{theory.fit}
\alias{theory.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
theory.fit(theory_eval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theory_eval}{
%%     ~~Describe \code{theory_eval} here~~
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
function (theory_eval) 
{
    n_c <- ncol(theory_eval) - 1
    theory_fit <- data.frame(matrix(NA, ncol = 4, nrow = 0))
    for (i in (1:n_c)) {
        theory_fit <- rbind(theory_fit, QCAfit(theory_eval[, 
            i], theory_eval[, ncol(theory_eval)]))
    }
    rownames(theory_fit) <- names(theory_eval[1:n_c])
    return(theory_fit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
