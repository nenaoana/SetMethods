\name{matches.suf.typdcn}
\alias{matches.suf.typdcn}
\title{
	Match typical cases and deviant consistency cases with regards to sufficiency.
}
\description{
	A function that matches typical cases and deviant consistency cases with regards to sufficiency.
}
\usage{
matches.suf.typdcn(results, outcome, neg.out=FALSE, intermed=FALSE, sol=1, max_pairs = 5)
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
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
}
\references{
Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597
}
\author{
	Juraj Medzihorsky
}
\seealso{
	\code{\link[QCAGUI:eqmcc]{eqmcc}}
}
\examples{
# Import your data. For example:

data(Schneider)

# Get the parsimonious solution:


sol_yp <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))
                

# Match typical cases and deviant consistency cases for the parsimonious solution:

matches.suf.typdcn(results = sol_yp, outcome = "EXPORT")

# Match typical cases and deviant consistency cases for the parsimonious solution
# and return only the best 3 pairs:

matches.suf.typdcn(results = sol_yp, outcome = "EXPORT", max_pairs=3)

# Match typical cases and deviant consistency cases for the intermediate solution:

matches.suf.typdcn(results = sol_yi, outcome = "EXPORT", intermed = TRUE)

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- eqmcc(Schneider, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Match typical cases and deviant consistency cases for the second 
# parsimonious solution for the absence of the outcome:

matches.suf.typdcn(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCAGUI}

