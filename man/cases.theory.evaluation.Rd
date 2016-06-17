\name{cases.theory.evaluation}
\alias{cases.theory.evaluation}
\title{
	Function that returns names of cases in the intersections between theory and the empirical solution.
}
\description{
	Function that returns names of cases in the intersections between theory and the empirical solution.
}
\usage{
cases.theory.evaluation(theory, empirics, outcome, intermed=FALSE, sol = 1)
}
\arguments{
	\item{theory}{
		A character string specifying the theory. Negated conditions should be introduced with a "~".
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
\value{
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
