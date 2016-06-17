\name{indirectCalibration}
\alias{indirectCalibration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
indirectCalibration(x, x_cal, binom = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{x_cal}{
%%     ~~Describe \code{x_cal} here~~
}
  \item{binom}{
%%     ~~Describe \code{binom} here~~
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
function (x, x_cal, binom = TRUE) 
{
    if (binom == TRUE) {
        options(warn = -1)
        Z_bin <- glm(x_cal ~ x, family = binomial)
        b_bin <- coef(Z_bin)
        y_hat_bin <- 1/(1 + (exp(-(b_bin[1] + b_bin[2] * x))))
        return(structure(y_hat_bin, class = "SetMethod"))
        options(warn = 1)
    }
    else {
        N <- length(x_cal)
        x_cal_t <- ((x_cal * (N - 1) + 1/2)/N)
        Z_bet <- betareg(x_cal_t ~ x, link = "logit", phi = TRUE)
        y_hat_bet <- b_bet <- coef(Z_bet)
        y_hat_bet <- 1/(1 + (exp(-(b_bet[1] + b_bet[2] * x))))
        return(structure(y_hat_bet, class = "SetMethod"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
