\name{cluster.minimize}
\alias{cluster.minimize}
\title{
	Diagnostic tool for clustered data
}
\description{
	Function returns pooled, within, and between consistencies for an object of class "qca".
}
\usage{
cluster.minimize(results, data, outcome, unit_id, cluster_id, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca". For performing cluster diagnostics of the sufficient solution for the negated     outcome one must only use the \code{minimize()} result from the sufficiency analysis of the negated outcome.
	}
	\item{data}{
		A data frame with column names in capital letters.
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing cluster 
		diagnostics of the sufficient solution for the negated outcome one must only use the \code{minimize()}         result from the sufficiency analysis of the negated outcome in the argument \code{results}. Changing 
    the name in the argument \code{outcome} or using a tilde is not necessary.
	}
	\item{unit_id}{
		A character string with the name of the vector containing the units (i.e. countries).
	}
	\item{cluster_id}{
		A character string with the name of the vector containing the clustering units (i.e. years).
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
	\code{\link[QCA:minimize]{minimize}}
}
\examples{

# Import your clustered data in the long format. 
# For example:

data(SCHLF)

# Get the intermediate solution:

sol_yi <- minimize(SCHLF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Get pooled, within, and between consistencies for the second intermediate solution:

cluster.minimize(results = sol_yi, data = SCHLF, outcome = "EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", sol = 2)
}
\keyword{QCA}
