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
\references{
Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597

Schneider, C. Q., Rohlfing I. 2016. Case Studies Nested in Fuzzy-Set QCA on Sufficiency: Formalizing Case Selection and Causal Inference. Sociological Methods & Research 45(3): 526-68

Rohlfing, I., Schneider C. Q. 2016. A Unifying Framework for Causal Analysis in Set-Theoretic Multi-Method Research. Sociological Methods & Research: 1-27. DOI: 10.1177/0049124115626170
}
\author{
	Ioana-Elena Oana
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


# Match typical cases and individually irrelevant cases for each conjunct 
# for the first term (default) of the parsimonious solution and return only the best 3 pairs:

matches.suf.typiir(results = sol_yp, outcome = "EXPORT", max_pairs=2)

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- eqmcc(Schneider, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Match typical cases and individually irrelevant cases for each conjunct for 
# the first term (default) of the second parsimonious solution for the absence of the outcome:

matches.suf.typiir(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)

}
\keyword{QCAGUI}
