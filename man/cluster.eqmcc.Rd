\name{cluster.eqmcc}
\alias{cluster.eqmcc}
\title{
	Diagnostic tool for clustered data/
}
\description{
	Function returns pooled, within, and between consistencies for an object of class "qca".
}
\usage{
cluster.eqmcc(results, data, outcome, unit_id, cluster_id, intermed=FALSE, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{data}{
		A data frame.
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{unit_id}{
		A character string with the name of the vector containing the units (i.e. countries).
	}
	\item{cluster_if}{
		A character string with the name of the vector containing the clustering units (i.e. years).
	}
	\item{intermediate}{
		Logical. Use the intermediate solution?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution according to the order in the "qca" object.
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
