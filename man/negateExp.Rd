\name{negateExp}
\alias{negateExp}
\title{
	Negates a boolean expression.
}
\description{
	Function that negates a boolean expression.
}
\usage{
negateExp(expression)
}
\arguments{
	\item{expression}{
		A boolean expression.
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}

}
%\details{}
\value{ It returns a negated boolean expression.
}

\author{
	Ioana-Elena Oana
}

\examples{
negateExp("~EMP*MA")
negateExp("~A*B + C*~D")
}

