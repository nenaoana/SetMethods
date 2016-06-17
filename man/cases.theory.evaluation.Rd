\name{cases.theory.evaluation}
\alias{cases.theory.evaluation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cases.theory.evaluation(theory, empirics, outcome, intermed = FALSE, sol = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theory}{
%%     ~~Describe \code{theory} here~~
}
  \item{empirics}{
%%     ~~Describe \code{empirics} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
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
function (theory, empirics, outcome, intermed = FALSE, sol = 1) 
{
    ND <- theory.evaluation(theory = theory, empirics = empirics, 
        outcome = outcome, intermed = intermed, sol = sol)
    list(`Covered Most Likely (T*E and Y > 0.5)` = rownames(ND)[(ND$"T*E" > 
        0.5) & (ND[, outcome] > 0.5)], `Covered Least Likely (t*E and Y > 0.5)` = rownames(ND)[(ND$"t*E" > 
        0.5) & (ND[, outcome] > 0.5)], `Uncovered Most Likely (T*e and Y > 0.5)` = rownames(ND)[(ND$"T*e" > 
        0.5) & (ND[, outcome] > 0.5)], `Uncovered Least Likely (t*e and Y > 0.5)` = rownames(ND)[(ND$"t*e" > 
        0.5) & (ND[, outcome] > 0.5)], `Inconsistent Most Likely (T*E and Y < 0.5)` = rownames(ND)[(ND$"T*E" > 
        0.5) & (ND[, outcome] < 0.5)], `Inconsistent Least Likely (t*E and Y < 0.5)` = rownames(ND)[(ND$"t*E" > 
        0.5) & (ND[, outcome] < 0.5)], `Consistent Most Likely (T*e and Y < 0.5)` = rownames(ND)[(ND$"T*e" > 
        0.5) & (ND[, outcome] < 0.5)], `Consistent Least Likely (t*e and Y < 0.5)` = rownames(ND)[(ND$"t*e" > 
        0.5) & (ND[, outcome] < 0.5)])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
