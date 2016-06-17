\name{CronBerg}
\alias{CronBerg}
\docType{data}
\title{Berg-Schlosser and Cronqvist (2005)}
\description{The \code{CronBerg} data frame has 18 rows and 5 variables}
\usage{data(CronBerg)}
\format{
  A data frame with 18 observations on the following 5 variables.
  \describe{
    \item{\code{GNP}}{a numeric vector. Condition, Gross National Product/Capita (ca. 1930). 0 if below 500$, 1 if between 550 and 850$, 2 above 850$.}
    \item{\code{URB}}{a numeric vector. Condition, urbanization (population in towns with 20000 and more inhabitants); 0 if below 50 per cent; 1 if above. }
    \item{\code{LIT}}{a numeric vector. Condition, literacy: 0 if below 75 per cent; 1 if above.}
    \item{\code{INDUS}}{a numeric vector. Condition, Industrial Labour Force (incl. mining); 0 if below 30 per cent of active population; 1 if above.}
    \item{\code{DEMOC}}{a numeric vector. Condition, stability of a democracy: 0 if not stable; 1 if stable.}
  }
}
\details{
The data are used by Berg-Schlosser and Cronqvist (2005) to demostrate mvQCA. The original data are from Lipset (1963). Data are multi-value.}
%\source{}
\references{
Berg-Schlosser, D. and Cronqvist, L. (2005)	"Macro-Quantitative vs. Macro-Qualitative Methods in the Social Sciences - An Example from Empirical Democratic Theory", Historical Social Research 30, pp. 154-175.

Lipset, Seymour M. (1963) Political Man. The Social Bases of Politics. Doubleday: New York.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.

Schneider, C. Q., Wagemann, C., Quaranta, M. (2012) How To... Use Software for Set-Theoretic Analysis. Online Appendix to "Set-Theoretic Methods for the Social Sciences". Available at \url{www.cambridge.org/schneider-wagemann}
}
\examples{data(CronBerg)}
\keyword{datasets}
