\name{matches.suf.typiir}
\alias{matches.suf.typiir}
\title{
	Match typical cases and individually irrelevant cases for each conjunct in a sufficient term.
}
\description{
	A function that matches typical cases and individually irrelevant cases with regards to sufficiency for each INUS condition.
}
\usage{
matches.suf.typiir(results, outcome, term = 1, neg.out=FALSE, 
                   intermed=FALSE, sol=1, max_pairs = 5)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{term}{
		A numeric vector where the first number indicates the number of the
		term according to the order in the "qca" object.
	}
	\item{neg.out}{
		Logical. Should the negated outcome be used?
	}
	\item{intermed}{
		Logical. Should the intermediate solution be used?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
}
\details{
}
\value{
}
\references{
Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597 

}
\author{
	Ioana-Elena Oana
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
}
\examples{
# Import your data. For example:

data(Schneider)

# Get the truth table for the presence of the outcome:

TT_y <- truthTable(Schneider, outcome = "EXPORT",
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut1 = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))

# Get the parsimonious solution:

sol_yp <- eqmcc(TT_y, include = "?", details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- eqmcc(TT_y, include = "?", details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Match typical cases and individually irrelevant cases for each conjunct 
# for the second term of the parsimonious solution:

matches.suf.typiir(results = sol_yp, outcome = "EXPORT", term = 2)

# Match typical cases and individually irrelevant cases for each conjunct 
# for the first term (default) of the parsimonious solution and return only the best 3 pairs:

matches.suf.typiir(results = sol_yp, outcome = "EXPORT", max_pairs=3)

# Match typical cases and individually irrelevant cases for each conjunct  
# for the first term (default) of the intermediate solution:

matches.suf.typiir(results = sol_yi, outcome = "EXPORT", intermed = TRUE)

# Get the truth table and the parsimonious solution for the absence of the outcome:

TT_ny <- truthTable(Schneider, outcome = "EXPORT", neg.out = TRUE,
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut1 = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))


sol_nyp <- eqmcc(TT_ny, include = "?", details = TRUE, show.cases = TRUE)

# Match typical cases and individually irrelevant cases for each conjunct for 
# the first term (default) of the second parsimonious solution for the absence of the outcome:

matches.suf.typiir(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCA}
