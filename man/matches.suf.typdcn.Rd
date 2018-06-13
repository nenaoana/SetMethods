\name{matches.suf.typdcn}
\alias{matches.suf.typdcn}
\title{
	Match typical cases and deviant consistency cases with regards to sufficiency.
}
\description{
	A function that matches typical cases and deviant consistency cases with regards to sufficiency.
}
\usage{
matches.suf.typdcn(results, outcome, neg.out=FALSE, sol=1, max_pairs = 5)
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
	Juraj Medzihorsky and Ioana-Elena Oana
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
                

# Match typical cases and deviant consistency cases for the parsimonious solution:

matches.suf.typdcn(results = sol_yp, outcome = "EXPORT")

# Match typical cases and deviant consistency cases for the parsimonious solution
# and return only the best 3 pairs:

matches.suf.typdcn(results = sol_yp, outcome = "EXPORT", max_pairs=3)

# Match typical cases and deviant consistency cases for the intermediate solution:

matches.suf.typdcn(results = sol_yi, outcome = "EXPORT")

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- minimize(SCHF, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Match typical cases and deviant consistency cases for the second 
# parsimonious solution for the absence of the outcome:

matches.suf.typdcn(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCA}

