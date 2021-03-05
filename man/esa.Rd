\name{esa}
\alias{esa}
\title{
	Function that performs the Enhanced Standard Analysis.
}
\description{
	Function that performs the Enhanced Standard Analysis.
}
\usage{
esa(oldtt, nec_cond, untenable_LR, contrad_rows)
}
\arguments{
	\item{oldtt}{
		A truthTable object.
	}
	\item{nec_cond}{
		A vector of character strings containing the necessary conditions. 
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Unions of conditions are performed with a "+". Using this argument, logical remainder 
		rows that contradict the statement of necessity will not be used in the analysis 
		(i.e. OUT will be set to 0 in the truth table).
	}
	\item{untenable_LR}{
		A Boolean expression containing the untenable logical remainders. 
		Conditions should be capitalized and negated conditions should be inserted with a "~".
		Intersections of conditions are performed with a "*". Using this argument, logical remainder
		rows containing the particular intersection specified will not be used in the analysis 
		(i.e. OUT will be set to 0 in the truth table).
	}
	\item{contrad_rows}{
		A vector containing the names of the rows that are contradictory. Using this argument,
		all rows with the names specified (both logical remainders and rows containing empirical information) will not be      used in the analysis (i.e. OUT will be set to 0 in the truth table).
	}
	}
\value{ It returns a new truth table in which all truth table rows are set to outcome value 0 that would otherwise present untenable assumptions.
}
\references{Schneider, C. Q., Wagemann, C. 2012. Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis. Cambridge: Cambridge University Press, chapter 8.
}
\author{
	Ioana-Elena Oana
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
}
\examples{
# Import your data. For example:

data(SCHF)

# Get the truth table for the presence of the outcome:

TT_y <- truthTable(SCHF, outcome = "EXPORT",
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))

# Exclude condition STOCK + MA and condition EMP as necessary for EXPORT
# Exclude all remainder rows containing the combination BARGAIN*~OCCUP
# Exclude the rows "19", "14", "46", "51" as contradictory:

newtt <- esa(oldtt = TT_y, nec_cond = c("STOCK+MA", "EMP"), 
          untenable_LR = "BARGAIN*~OCCUP", contrad_rows = c("19", "14", "46", "51"))

# The truth table newly created can afterwards be used in further analyses

}
\keyword{QCA}
