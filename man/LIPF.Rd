\name{LIPF}
\alias{LIPF}
\docType{data}
\title{Lipset (1959), fuzzy-set}
\description{The \code{LIPF} data frame has 18 rows and 6 sets}

\usage{data(LIPF)}
\format{
  A data frame with 18 observations on the following 6 sets.
  \describe{
    \item{\code{SURVIVED}}{a numeric vector. Outcome, survival of democracy during the inter-war period.}
    \item{\code{DEVELOPED}}{a numeric vector. Condition, economically developed countries.}
    \item{\code{URBAN}}{a numeric vector. Condition, urbanized countries.}
    \item{\code{LITERATE}}{a numeric vector. Condition, countries with high literacy rate.}
    \item{\code{INDUSTRIAL}}{a numeric vector. Condition, industrialized countries.}
    \item{\code{STABLE}}{a numeric vector. Condition, politically stable countries.}
  }
}
\details{Data used by Ragin (2009) to illustrates the variants of QCA. Originally by Lipset (1959). Data are fuzzy-sets.}
\references{
Lipset, S. M. (1959) "Some Social Requisites of Democracy: Economic Development and Political Legitimacy", American Political Science Review 53, pp. 69-105.

Ragin, C. C. (2009) "Qualitative Comparative Analysis. Using Fuzzy Sets (fsQCA)." In Rihoux, B., and Ragin, C. C. (eds.) Configurational Comparative Methods. Qualitative Comparative Analysis (QCA) and Related Techniques. Thousand Oaks, CA and London: Sage, pp. 87-121.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.
}
\examples{data(LIPF)}
\keyword{datasets}
