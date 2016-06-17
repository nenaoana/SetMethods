\name{theory.evaluation}
\alias{theory.evaluation}
\title{
	Function that returns membership of cases in the intersections between theory and the empirical solution.
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
	\item{intermediate}{
		Logical. Use the intermediate solution?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution according to the order in the "qca" object.
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
}
\keyword{QCA}
