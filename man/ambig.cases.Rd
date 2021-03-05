\name{ambig.cases}
\alias{ambig.cases}
\title{
	Function for identifying cases with 0.5 fuzzy-set values.
}
\description{
	A function that identifies cases with 0.5 fuzzy-set values.}
\usage{ambig.cases(data)}
\arguments{
	\item{data}{
		A datafarme, a subset of a dataframe, or a vector (i.e. single column in a dataframe). If the function is       provided with the dataframe it will return the name of the cases with 0.5 values together with their            location in the dataframe. If the function is provided with a vector (i.e. a single column), it will return     the position of the case having a 0.5 in that vector. The function should be used for calibrated data and       will give an error if the data contains uncalibrated scores. However, if you have both calibrated and           uncalibrated data in the same dataframe, it is possible to use the function only for the calibrated subset      of that data. 
	}
}
\author{
	Ioana-Elena Oana
}
\examples{
# Import your data. For example:

data(SCHF)

# Get cases with 0.5 in the entire dataframe:

ambig.cases(SCHF)

# Get cases with 0.5 in the column "EMP" in the dataframe:

ambig.cases(SCHF$EMP)

# Get cases with 0.5 in the 7th column of the dataframe:

ambig.cases(SCHF[,7])

}
\keyword{QCA}
