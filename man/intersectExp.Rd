\name{intersectExp}
\alias{intersectExp}
\title{
	Intersects two boolean expressions.
}
\description{
	Function that intersects two boolean expressions.
}
\usage{
intersectExp(expression1, expression2)
}
\arguments{
	\item{expression1}{
		A boolean expression.
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}
	\item{expression2}{
		A boolean expression.
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}

}
%\details{}
\value{ It returns the boolean expression representing the intersection of the two inputed expressions.
}

\author{
	Ioana-Elena Oana
}

\examples{
intersectExp("~EMP*MA", "MA+~STOCK*OCCUP")
intersectExp("~A*B + C*~D","A*B+~D")
}

