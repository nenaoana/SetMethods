\name{QCAfit}
\alias{QCAfit}
\title{Function calculating the parameters of fit}
\description{QCAfit is a function calculating parameters of fit useful in QCA and fsQCA that are consistency, coverage, PRI and PRODUCT. It works with both single and multiple conditions.}
\usage{QCAfit(x, y, cond.lab = NULL, necessity = FALSE, negation = FALSE)}
\arguments{
  \item{x}{is a vector containing the values of a condition or a matrix with more than one conditions.}
  \item{y}{is a vector containing the values of the output.}
  \item{cond.lab}{is a vector containing the label(s) of the condition(s).}
  \item{necessity}{logical. It indicates if the output should be for sufficient or necessary condition(s). By default, \code{TRUE}, the function returns a table of parameters of fit for sufficient condition(s) (Consistency, Coverage, PRI and Product). When it set to \code{FALSE} the function returns a table of parameters of fit for necessary condition(s) (Consistency, Coverage, Relevance of Necessity).}
  \item{negation}{logical. It indicates if the parameters of fit should be computed for the positive or the negative outcome. By default, \code{FALSE}, the function returns parameters of fit for the positive outcome.}
}
%\details{}
\value{It returns a matrix containing the parameters of fit for each condition.}
\references{
Ragin, C. C. 2006. Set Relations in Social Research: Evaluating Their Consistency and Coverage. Political Analysis 14(3): 291-310.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.	

Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}}
\author{Mario Quaranta}
%\note{}
%\seealso{}
\examples{
# Generate fake data
set.seed(1234)

a <- runif(100, 0, 1)
b <- runif(100, 0, 1)
c <- runif(100, 0, 1)
y <- runif(100, 0, 1)

# Only one condition, for sufficiency
QCAfit(a, y, cond.lab = "A")

# With three conditions, for sufficiency
QCAfit(cbind(a, b, c), y, cond.lab = c("A", "B", "C"))

# Only one condition, for necessity
QCAfit(a, y, cond.lab = "A", necessity = TRUE)

# With three conditions, for necessity
QCAfit(cbind(a, b, c), y, cond.lab = c("A", "B", "C"), necessity = TRUE)

# With three conditions and negated output, for sufficiency
QCAfit(cbind(a, b, c), y, cond.lab = c("A", "B", "C"), negation = TRUE)
}
