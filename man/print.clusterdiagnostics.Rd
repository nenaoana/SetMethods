\name{print.clusterdiagnostics}
\alias{print.clusterdiagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.clusterdiagnostics(R, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
%%     ~~Describe \code{R} here~~
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
function (R, digits = 3) 
{
    cat("Consistencies:\n--------------\n")
    cat("Pooled:\t\t", round(R$POCOS, digits = 3), "\n")
    for (b in 1:length(R$BECOS)) {
        cat("Between", b, ":\t", round(R$BECOS[b], digits = 3), 
            "\n")
    }
    for (w in 1:length(R$WICOS)) {
        cat("Within", w, ":\t", round(R$WICOS[w], digits = 3), 
            "\n")
    }
    cat("\n")
    cat("Distances:\n----------\n")
    cat("Between to Pooled:\t", round(R$dBP, digits = 3), "\n")
    cat("Within to Pooled:\t", round(R$dWP, digits = 3), "\n\n")
    cat("Coverages:\n----------\n")
    cat("Pooled:\t\t", round(R$Coverages$pooled, digits = 3), 
        "\n")
    for (b in 1:length(R$Coverages$between)) {
        cat("Between", b, ":\t", round(R$Coverages$between[b], 
            digits = 3), "\n")
    }
    for (w in 1:length(R$Coverages$within)) {
        cat("Within", w, ":\t", round(R$Coverages$within[w], 
            digits = 3), "\n")
    }
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
