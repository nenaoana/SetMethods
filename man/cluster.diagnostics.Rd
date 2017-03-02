\name{cluster.diagnostics}
\alias{cluster.diagnostics}
\title{
	Diagnostic tool for clustered data.
}
\description{
	Function returns pooled, within, and between consistencies and coverages for the relationship between two sets.
}
\usage{
cluster.diagnostics(x, y, unit, cluster, necessity= FALSE)
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
\references{
Garcia-Castro, A., Arino, M. A.. 2013. A General Approach to Panel Data Set-Theoretic Research. COMPASSS Working Paper 2013-76
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\seealso{
	\code{\link[QCAGUI:eqmcc]{eqmcc}}
}
\examples{
# Import your clustered data in the long format. 
# For example:

data(SchneiderLong)

# Get pooled, within, and between consistencies and coverages 
# for STOCK as necessary for EXPORT:


cluster.diagnostics(SchneiderLong$STOCK, SchneiderLong$EXPORT, unit = SchneiderLong$COUNTRY, 
                    cluster = SchneiderLong$YEAR, necessity= TRUE)

}
\keyword{QCAGUI}
