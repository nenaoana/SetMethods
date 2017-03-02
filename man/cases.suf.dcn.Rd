\name{cases.suf.dcn}
\alias{cases.suf.dcn}
\title{
	List cases deviant with regards to consistency for sufficiency.
}
\description{
	A function that extracts cases deviant with regards to consistency for sufficiency from an object of class "qca".
}
\usage{
cases.suf.dcn(results, outcome, neg.out=FALSE, intermed=FALSE, sol = 1)
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
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97

Schneider, C. Q., Rohlfing I. 2016. Case Studies Nested in Fuzzy-Set QCA on Sufficiency: Formalizing Case Selection and Causal Inference. Sociological Methods & Research 45(3): 526-68
}
\author{
	Juraj Medzihorsky and Ioana-Elena Oana
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
                
# Return deviant cases consistency for sufficiency for the parsimonious solution:

cases.suf.dcn(results = sol_yp, outcome = "EXPORT")

# Return deviant cases consistency for sufficiency for the intermediate solution:

cases.suf.dcn(results = sol_yi, outcome = "EXPORT", intermed = TRUE)

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- eqmcc(Schneider, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Return deviant cases consistency for sufficiency for the second parsimonious solution 
# for the absence of the outcome:

cases.suf.dcn(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)

}
\keyword{QCAGUI}
