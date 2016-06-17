\name{cases.suf.dcv}
\alias{cases.suf.dcv}
\title{
	List cases deviant with regards to coverage for sufficiency.	
}
\description{
	Function extracts cases deviant with regards to coverage for sufficiency from an object of class "qca".
}
\usage{
cases.suf.dcv(results, outcome, neg.out=FALSE, intermed=FALSE, sol = 1)
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
	Juraj Medzihorsky and Ioana-Elena Oana
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
}
\examples{}
\keyword{QCA}
