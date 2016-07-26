\name{esa}
\alias{esa}
\title{
	Function that performs the Enhanced Standard Analysis.
}
\description{
	Function that performs the Enhanced Standard Analysis.
}
\usage{
esa(oldtt, nec_cond, imposs_LR, contrad_rows)
}
\arguments{
	\item{oldtt}{
		A truthTable object.
	}
	\item{nec_cond}{
		A vector of character strings containing the necessary conditions. 
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}
	\item{imposs_LR}{
		A vector of character strings containing the impossible logical remainders. 
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
	}
	\item{contrad_rows}{
		A vector containing the names of the rows which are contradictory.
	}
	}
\details{
}
\value{ It returns a new truthtable in which untenable assumptions are banned.
}
\references{
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
# Import your data. For example:

data(Schneider)

# Get the truth table for the presence of the outcome:

TT_y <- truthTable(Schneider, outcome = "EXPORT",
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut1 = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))

# Exclude the union of STOCK and MA as necessary for EXPORT
# Exclude the rows "13", "14", "46", "51" as contradictory:

newtt <- esa(oldtt = TT_y, nec_cond = "STOCK+~MA", contrad_rows = c("13", "14", "46", "51"))

# The truth table newly created can afterwards be used in further analyses

}
\keyword{QCA}
