\name{QCAfit}
\alias{QCAfit}
\title{Function calculating the parameters of fit}
\description{QCAfit is a function calculating parameters of fit useful in QCA and fsQCA that are consistency, coverage, PRI, Haesebrouck's consistency, RoN and PRODUCT. It works with both single and multiple conditions.}
\usage{QCAfit(x, y, cond.lab = NULL, necessity = FALSE, neg.out = FALSE, 
              product = FALSE, sol=1, ttrows= c())}
\arguments{
  \item{x}{
    A vector containing the values of a condition, a matrix with more than one conditions, or an object of         class "qca" when necessity is \code{FALSE} and when outcome is specifyied as a character string.}
  \item{y}{
    A vector containing the values of the output or a character string when y is of class "qca".}
  \item{cond.lab}{
    is a vector containing the label(s) of the condition(s).}
  \item{necessity}{
    logical. It indicates if the output should be for sufficient or necessary condition(s). By default,             \code{FALSE}, the function returns a table of parameters of fit for sufficient condition(s) (Consistency,       Coverage, PRI, Haesebrouck's Consistency, and optionally Product). When it set to \code{TRUE} the              function returns a table of parameters of fit for necessary condition(s) (Consistency, Coverage,               Relevance of Necessity).}
  \item{neg.out}{logical. It indicates if the parameters of fit should be computed for the positive or the        negative outcome. By default, \code{FALSE}, the function returns parameters of fit for the positive outcome   .}
  \item{product}{logical. It indicates whether the parameter of fit PRODUCT should be shown. This stands for     the product between the consistency sufficiency parameter and the PRI parameter.}
  \item{sol}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.}
  \item{ttrows}{
		A vector specifying the names of the truth table rows for which the function reports parameters of fit. 
		For using this option y must be a "qca" object.
	}
}
%\details{}
\value{It returns a matrix containing the parameters of fit for each condition.}
\references{
Haesebrouck, T. (2015) Pitfalls in QCA's consistency measure. Journal of Comparative Politics 2:65-80.

Ragin, C. C. 2006. Set Relations in Social Research: Evaluating Their Consistency and Coverage. Political Analysis 14(3): 291-310.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.	

Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}}
\author{Mario Quaranta and Ioana-Elena Oana}
%\note{}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
}
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
QCAfit(cbind(a, b, c), y, cond.lab = c("A", "B", "C"), neg.out = TRUE)

# Load the Schneider data:

data(SCHF)

# Get parameters of fit for condition EMP as necessary for outcome EXPORT:

QCAfit(SCHF$EMP, SCHF$EXPORT, necessity = TRUE, cond.lab = "EMP")

# Get parameters of fit for condition ~EMP as necessary for outcome ~EXPORT:

QCAfit(1-SCHF$EMP, SCHF$EXPORT, neg.out=TRUE, necessity = TRUE, cond.lab = "~EMP")

# Get parameters of fit for all conditions as necessary for outcome EXPORT:

QCAfit(SCHF[,1:6], SCHF$EXPORT, necessity = TRUE, cond.lab = names(SCHF[,1:6]))

# Obtain the intermediate solution for outcome "EXPORT":

sol_yi <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))
                
# Get parameters of fit for the second model of the intermediate solution:

QCAfit(x = sol_yi, y = "EXPORT", sol=2)

# Get parameters of fit for truth table rows 2,8, and 10:

QCAfit(x = sol_yi, y = "EXPORT", ttrows=c("2","8","10"))

}
