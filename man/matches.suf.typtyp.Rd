\name{matches.suf.typtyp}
\alias{matches.suf.typtyp}
\title{
	Match typical cases for each conjunct in a sufficient term..
}
\description{
	A function that matches typical cases with regards to sufficiency for each INUS condition.
}
\usage{
matches.suf.typtyp(results, outcome, term = 1, neg.out=FALSE, 
                   sol=1, max_pairs = 5)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing pimdata of the          sufficient solution for the negated outcome one must only use the \code{minimize()} result from the            sufficiency analysis of the negated outcome in the argument \code{results}. Changing the name in the        argument \code{outcome} or using a tilde is not necessary.
	}
	\item{term}{
		A numeric vector where the first number indicates the number of the
		term according to the order in the "qca" object.
	}
	\item{neg.out}{
		Logical. Should the negated outcome be used?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
}
\references{
Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597
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

# Match typical cases for each conjunct for the second term of the parsimonious solution:

matches.suf.typtyp(results = sol_yp, outcome = "EXPORT", term = 2)

# Match typical cases for each conjunct for the first term (default) of 
# the parsimonious solution and return only the best 3 pairs:

matches.suf.typtyp(results = sol_yp, outcome = "EXPORT", max_pairs=3)

# Match typical cases for each conjunct for the first term (default) 
# of the intermediate solution:

matches.suf.typtyp(results = sol_yi, outcome = "EXPORT")

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- minimize(SCHF, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Match typical cases for each conjunct of the second term
# of the second parsimonious solution for the absence of the outcome:

matches.suf.typtyp(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, term = 2, sol = 2)
}
\keyword{QCA}
