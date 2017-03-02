\name{print.clustereqmcc}
\alias{print.clustereqmcc}
\title{Print a clustereqmcc Object}
\description{
  Information regarding pooled, within, and between consistencies for an object of class "qca" is printed. 
}
\usage{
\method{print}{clustereqmcc}(x, \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{cluster.eqmcc}, representing
    pooled, within, and between consistencies and coverages for an object inheriting 
    from class 'qca' in the case of clustered data
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
\code{\link[QCAGUI:eqmcc]{eqmcc}}
}
\keyword{QCAGUI}
