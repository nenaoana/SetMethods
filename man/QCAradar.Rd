\name{QCAradar}
\alias{QCAradar}
\title{
	Function for displaying a radar chart.
}
\description{
	Function displays a radar chart for an object of class "qca" or for a boolean expression.
}
\usage{
QCAradar(results, outcome= NULL, fit= FALSE, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca". For performing radar charts of the sufficient solution for 
		the negated outcome one must only use the \code{minimize()} result from the sufficiency 
		analysis of the negated outcome.
		The argument results can also be a boolean expression of the form e.g. "A*~B + ~B*C".
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters when results is of type 'qca'. 
		When performing radar charts of the sufficient solution for the negated outcome one must 
		only use the \code{minimize()}result from the sufficiency analysis of the negated outcome 
		in the argument \code{results}. Changing the name in the argument \code{outcome} or using 
		a tilde is not necessary.
	}
	\item{fit}{
		Logical. Print parameters of fit when results is of type 'qca'
	}
	\item{sol}{
		A vector where the first number indicates the number of the conservative or parsimonious solution 
		according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
}
\references{
Maerz, F. Seraphine. 2017. "Pathways of Authoritarian Persistence." Paper presented at the CEU Annual Doctoral Conference 
}
\author{
	Ioana-Elena Oana
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
}
\examples{

# Import data. 
# For example:

data(SCHF)

# Get the intermediate solution:

sol_yi <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Display radar chart for the second intermediate solution:

QCAradar(results = sol_yi, outcome = "EXPORT", fit=TRUE, sol = 2)

# Show a radar chart for the following boolean expression "A + ~B*Z*~C"

QCAradar(results = "A + ~B*Z*~C")

}
\keyword{QCA}
