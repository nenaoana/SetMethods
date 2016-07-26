\name{theory.evaluation}
\alias{theory.evaluation}
\title{
	Membership of cases in the intersections between theory and the empirical solution.
}
\description{
	Function that returns membership of cases in the intersections between theory and 
	the empirical solution in the form of a data frame.
}
\usage{
theory.evaluation(theory, empirics, outcome, intermed=FALSE, sol = 1)
}
\arguments{
	\item{theory}{
		A character string specifying the theory.
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}
	\item{empirics}{
		An object of class 'qca'.
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{intermed}{
		Logical. Use the intermediate solution?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
}
\details{
}
\value{ It returns a data frame containing the intersections between theory and empirics.
}
\references{
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
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

# Get the intermediate solution:

sol_yi <- eqmcc(TT_y, include = "?", details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Specify the theory. Let's assume the theory says that the 
# absence of EMP and the presence of MA is sufficient for EXPORT:

t<-"~EMP*MA"

# Get membership of cases in the intersections between theory and 
# the second intermediate solution:

theory.evaluation(theory = t, empirics = sol_yi, outcome = "EXPORT", intermed = TRUE, sol = 2)

}
\keyword{QCA}
