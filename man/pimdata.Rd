\name{pimdata}
\alias{pimdata}
\title{
	Function to extract prime implicants table from object of class "qca"	
}
\description{
	Function extracts a table with prime implicants from an object of class
	"qca".  
}
\usage{
pimdata(results, outcome, intermed = FALSE, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{intermed}{
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
	A table with set memberships.
	\item{solution_formula}{The solution formula.}
	\item{out}{Membership in the outcome.}
}
\references{
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
	\code{\link[SetMethods:pimplot]{pimplot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{QCA}
