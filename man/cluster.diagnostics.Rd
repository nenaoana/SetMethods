\name{cluster.diagnostics}
\alias{cluster.diagnostics}
\title{
	Diagnostic tool for clustered data.
}
\description{
	Function returns pooled, within, and between consistencies and coverages for the relationship between two sets.
}
\usage{
cluster.diagnostics(x, y, unit, cluster, p=2, necessity= FALSE)
}
\arguments{
	\item{x}{
		A vector containing the condition.
	}
	\item{y}{
		A vector containing the outcome.
	}
	\item{unit}{
		A vector containing the names of the units (i.e. countries).
	}
	\item{cluster}{
		A vector containing the names of the clustering unit (i.e. years).
	}
	\item{necessity}{
		Logical. Perform the diagnostic for the relationship of necessity?
	}
}
\details{
}
\value{
}
\references{
Garcia-Castro, A., Arino, M. A.. 2013. A General Approach to Panel Data Set-Theoretic Research. COMPASSS Working Paper 2013-76
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
}
\examples{
}
\keyword{QCA}
