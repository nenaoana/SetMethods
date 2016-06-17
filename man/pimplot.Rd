\name{pimplot}
\alias{pimplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pimplot(results, outcome, intermed = FALSE, sol = 1, case_labels = TRUE, lab_color = rgb(0, 0, 0, 0.5), lab_jitter = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
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
  \item{case_labels}{
%%     ~~Describe \code{case_labels} here~~
}
  \item{lab_color}{
%%     ~~Describe \code{lab_color} here~~
}
  \item{lab_jitter}{
%%     ~~Describe \code{lab_jitter} here~~
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
function (results, outcome, intermed = FALSE, sol = 1, case_labels = TRUE, 
    lab_color = rgb(0, 0, 0, 0.5), lab_jitter = FALSE) 
{
    P <- pimdata(results = results, outcome = outcome, intermed = intermed, 
        sol = sol)
    n_c <- ncol(P) - 1
    par(ask = T)
    aux.plot <- function(i) {
        xy.plot(P[, i], P[, "out"], xlab = colnames(P)[i], ylab = outcome)
        if (case_labels) {
            fil <- P[, i] > 0.5
            if (i == n_c) {
                fil <- P[, i] < 0.5
            }
            if (lab_jitter) {
                text(jitter(P[fil, i]), jitter(P[fil, "out"]), 
                  col = lab_color, rownames(P)[fil], pos = 3, 
                  cex = 0.75)
            }
            else {
                text(P[fil, i], P[fil, "out"], col = lab_color, 
                  rownames(P)[fil], pos = 3, cex = 0.75)
            }
        }
    }
    for (i in 1:n_c) {
        aux.plot(i)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
