\name{LR.intersect}
\alias{LR.intersect}
\title{
	Function for identifying contradictory simplifying assumptions or easy counterfactuals.	
}
\description{
	A function for identifying contradictory simplifying assumptions or easy counterfactuals by intersecting the    SAs or ECs of two solutions.	
}
\usage{
LR.intersect(results1, results2, sol1 = 1, sol2 = 1)
}
\arguments{
	\item{results1}{
    An object of class "qca". It can be a parsimonious or an intermediate solution for an outcome or its            negation obtained via \code{minimize()}.
	}
		\item{results2}{
    An object of class "qca". It can be a parsimonious or an intermediate solution for an outcome or its            negation obtained via \code{minimize()}.
	}
	\item{sol1}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{sol2}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
}
%\references{}
\author{
	Ioana-Elena Oana
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
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

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- minimize(SCHF, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)


# Get the contradictory simplofying assumptions:

LR.intersect(sol_yp, sol_nyp)
}
\keyword{QCA}
