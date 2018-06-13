\name{print.clusterdiagnostics}
\alias{print.clusterdiagnostics}
\title{Print a clusterdiagnostics Object}
\description{
  Information regarding pooled, within, and between consistencies for 
  for the relationship between two sets in the case of clustered data is printed.
}
\usage{
\method{print}{clusterdiagnostics}(x, \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{cluster.diagnostics}, representing
    pooled, within, and between consistencies and coverages for the relationship between 
    two sets in the case of clustered data
}
  \item{\dots}{optional arguments passed to \code{print.default}; see
    the documentation on that method function}
}
\references{
Garcia-Castro, A., Arino, M. A.. 2013. A General Approach to Panel Data Set-Theoretic Research. COMPASSS Working Paper 2013-76
}
\author{
Juraj Medzihorsky and Ioana-Elena Oana
}
\seealso{
\code{\link[QCA:minimize]{minimize}}
}
\keyword{QCA}
