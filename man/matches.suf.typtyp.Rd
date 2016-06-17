\name{matches.suf.typtyp}
\alias{matches.suf.typtyp}
\title{
	A function that matches typical cases with regards to sufficiency.
}
\description{
	A function that matches typical cases with regards to sufficiency for each INUS condition.
}
\usage{
matches.suf.typtyp(results, outcome, term = 1, neg.out=FALSE, 
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
		solution according to the order in the "qca" object.
	}
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
}
\details{
}
\value{
}
\references{
Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597
}
\author{
	Ioana-Elena Oana
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
}
\examples{
}
\keyword{QCA}
