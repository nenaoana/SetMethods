\name{cases.nec.iir}
\alias{cases.nec.iir}
\title{
	List individually irrelevant cases.
}
\description{
	A function that extracts individually irrelevant cases for an analysis of necessity from an object of class "qca".
}
\usage{
cases.nec.iir(results, outcome, sol = 1)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution according to the order in the "qca" object.
	}
}
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97

Rohlfing, I., Schneider C. Q. 2013. Combining QCA With Process Tracing in Analyses of Necessity. Political Research Quarterly 66(1): 220-35
}
\author{
	Juraj Medzihorsky
}
\seealso{
	\code{\link[QCAGUI:eqmcc]{eqmcc}}
}
%\examples{}
\keyword{QCAGUI}
