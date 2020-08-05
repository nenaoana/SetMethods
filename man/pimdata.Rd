\name{pimdata}
\alias{pimdata}
\title{
	Function to extract prime implicants table from object of class "qca"	
}
\description{
	A function that displays each case's set membership scores in each sufficient term, the solution formula, and the outcome from an object of class
	"qca".  
}
\usage{
pimdata(results, outcome, sol = 1, ...)
}
\arguments{
	\item{results}{
    An object of class "qca". For performing pimdata of the sufficient solution for the negated outcome one        must only use the \code{minimize()} result from the sufficiency analysis of the negated outcome.
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing pimdata of the             sufficient solution for the negated outcome one must only use the \code{minimize()} result from the            sufficiency analysis of the negated outcome in the argument \code{results}. Changing the name in the           argument \code{outcome} or using a tilde is not necessary.
	}
	\item{sol}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{...}{
		Deprecated arguments (neg.out, use.tilde)
	}
}
\value{
	A table with set memberships.
	\item{solution_formula}{The solution formula.}
	\item{out}{Membership in the outcome.}
}
%\references{}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
	\code{\link[SetMethods:pimplot]{pimplot}}
}
\examples{
# Import your data. For example:

data(SCHF)

# Get the parsimonious solution:


sol_yp <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))


# Get the prime implicants table for the parsimonious solution:

pimdata(results = sol_yp, outcome = "EXPORT")

# Get the prime implicants table for the first intermediate solution:

pimdata(results = sol_yi, outcome = "EXPORT", sol = 1)

}
\keyword{QCA}
