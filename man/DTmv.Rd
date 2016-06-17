\name{DTmv}
\alias{DTmv}
\docType{data}
\title{Fake data for mvQCA}
\description{\code{mvQCA} data frame has 25 rows and 4 variables.}
\usage{data(DTmv)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{Y}}{a numeric vector. Outcome with 2 categories (crisp).}
    \item{\code{A}}{a numeric vector. Condition with 2 categories (crisp).}
    \item{\code{B}}{a numeric vector. Condition with 3 categories (multi-value).}
    \item{\code{C}}{a numeric vector. Condition with 3 categories (multi-value).}
  }
}
\details{The data frame has only exercise purpuses to let the user learn how to perform mvQCA in R.}
%\source{}
\references{Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}.
	
Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.}
\examples{data(DTmv)}
\keyword{datasets}
