\name{cases.suf.iir}
\alias{cases.suf.iir}
\title{
	List individually irrelevant cases for sufficiency.
}
\description{
	A function that extracts individually irrelevant cases for sufficiency from an object of class "qca".
}
\usage{
cases.suf.iir(results, outcome, neg.out=FALSE, sol = 1)
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
}
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97

Schneider, C. Q., Rohlfing I. 2016. Case Studies Nested in Fuzzy-Set QCA on Sufficiency: Formalizing Case Selection and Causal Inference. Sociological Methods & Research 45(3): 526-68

Rohlfing, I., Schneider C. Q. 2016. A Unifying Framework for Causal Analysis in Set-Theoretic Multi-Method Research. Sociological Methods & Research: 1-27. DOI: 10.1177/0049124115626170
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
                


# Return individually irrelevant cases for sufficiency for the parsimonious solution:

cases.suf.iir(results = sol_yp, outcome = "EXPORT")

# Return individually irrelevant cases cases for sufficiency for the intermediate solution:

cases.suf.iir(results = sol_yi, outcome = "EXPORT")

# Get the parsimonious solution for the absence of the outcome:

sol_nyp <- minimize(SCHF, outcome = "EXPORT", neg.out = TRUE,
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Return individually irrelevant cases for sufficiency for the second parsimonious solution 
# for the absence of the outcome:

cases.suf.iir(results = sol_nyp, outcome = "EXPORT", neg.out = TRUE, sol = 2)
}
\keyword{QCA}
