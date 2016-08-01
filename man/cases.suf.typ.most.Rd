\name{cases.suf.typ.most}
\alias{cases.suf.typ.most}
\title{
	Lists most typical cases with regards to sufficiency.
}
\description{
	A function that extracts most typical cases with regards to sufficiency from an object of class "qca".
}
\usage{
cases.suf.typ.most(results, outcome, neg.out = FALSE, intermed = FALSE, sol = 1)
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
	\item{intermed}{
		Logical. Should the intermediate solution be used?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
}
\details{
}
\value{
}
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97
}
\author{
	Juraj Medzihorsky and Ioana-Elena Oana
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

# Return most typical cases for sufficiency for the parsimonious solution:

cases.suf.typ.most(results = sol_yp, outcome = "EXPORT")

# Return most typical cases for sufficiency for the intermediate solution:

cases.suf.typ.most(results = sol_yi, outcome = "EXPORT", intermed = TRUE)

# Get the truth table and the parsimonious solution for the absence of the outcome:

TT_ny <- truthTable(Schneider, outcome = "EXPORT", neg.out = TRUE,
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut1 = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))


sol_nyp <- eqmcc(TT_ny, include = "?", details = TRUE, show.cases = TRUE)

# Return most typical cases for sufficiency for the second parsimonious solution 
# for the absence of the outcome:

cases.suf.typ.most(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCA}
