\name{cases.suf.typ.fct}
\alias{cases.suf.typ.fct}
\title{
	List cases typical with regards to sufficiency for each focal conjunct.
}
\description{
	A function that extracts cases typical with regards to sufficiency from an object of class "qca".
}
\usage{
cases.suf.typ.fct(results, outcome, term = 1, neg.out=FALSE, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing pimdata of the          sufficient solution for the negated outcome one must only use the \code{minimize()} result from the            sufficiency analysis of the negated outcome in the argument \code{results}. Changing the name in the        argument \code{outcome} or using a tilde is not necessary.
	}
	\item{neg.out}{
		Logical. Should the negated outcome be used?
	}
	\item{term}{
		A numeric vector where the first number indicates the number of the
		term according to the order in the "qca" object.
	}
	\item{sol}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
}
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97
}
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

# Get the intermediate solution:

sol_yi <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))


# Return typical cases for sufficiency for the parsimonious solution:

cases.suf.typ.fct(results = sol_yp, outcome = "EXPORT")

# Return typical cases for sufficiency for the intermediate solution:

cases.suf.typ.fct(results = sol_yi, outcome = "EXPORT")

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- minimize(SCHF, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Return typical cases for sufficiency for the second parsimonious solution 
# for the absence of the outcome:

cases.suf.typ.fct(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCA}
