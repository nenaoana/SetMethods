\name{mmr}
\alias{mmr}
\title{
	Function for performing set-theoretic multi-method research.
}
\description{
	A function that selects best available cases for single case studies and best pairs of matching cases for comparative case studies.
}
\usage{
mmr(results, outcome, neg.out = FALSE, sol = 1, match = NULL, 
      cases = NULL, max_pairs = 5, term = 1, use.tilde = TRUE)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{neg.out}{
		Logical. Should the negated outcome be used?
	}
  \item{sol}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{match}{
		Logical. Should comparative MMR be used?
	}
	\item{cases}{
	  A numerical vector indicating the type of cases to be returned.
		\cr For single case studies: \cr \verb{ 1 = typical cases,}
		                         \cr \verb{ 2 = typical cases for each focal conjunct in a sufficient term,}   
		                         \cr \verb{ 3 = deviant consistency,}
		                         \cr \verb{ 4 = deviant coverage,}
		                         \cr \verb{ 5 = individually irrelevant,}
		                         \cr \verb{ 6 = all of the above;}
		\cr For comparative case studies:\cr \verb{ 1 = Typical-Typical for each focal conjuct in a sufficient term,}
		                              \cr \verb{ 2 = Typical-IIR for each focal conjuct in a sufficient term,}
		                              \cr \verb{ 3 = Typical-Dev.Cons.,}
		                              \cr \verb{ 4 = Dev.Cov.-IIR;}
		                              \cr \verb{ 5 = all of the above;}
	}
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
	\item{term}{
		A numeric vector where the first number indicates the number of the
		term according to the order in the "qca" object.
	}
	\item{use.tilde}{
		Logical. Are sets negated with a tilde in the solution?
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

# Get typical cases for each focal conjunct in the third term of the parsimonious solution:

mmr(results = sol_yp, outcome = "EXPORT", match=FALSE, cases=2, term = 3)

# Get matching typical-typical cases for the second term of the parsimonious solution:

mmr(results = sol_yp, outcome = "EXPORT", match=TRUE, cases=1, term = 2)

# Get matching typical-DCN cases:

mmr(results = sol_yp, outcome = "EXPORT", match=TRUE, cases=3)

}
\keyword{QCA}
