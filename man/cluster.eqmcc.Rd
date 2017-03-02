\name{cluster.eqmcc}
\alias{cluster.eqmcc}
\title{
	Diagnostic tool for clustered data
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
	\item{cluster_id}{
		A character string with the name of the vector containing the clustering units (i.e. years).
	}
	\item{intermed}{
		Logical. Use the intermediate solution?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution according to the order in the "qca" object.
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

# Get the intermediate solution:

sol_yi <- eqmcc(SchneiderLong, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Get pooled, within, and between consistencies for the second intermediate solution:

cluster.eqmcc(results = sol_yi, data = SchneiderLong, outcome = "EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", intermed = TRUE, sol = 2)
}
\keyword{QCAGUI}
