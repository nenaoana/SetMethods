\name{theory.fit}
\alias{theory.fit}
\title{
	Parameters of fit for the intersections between theory and the empirical solution.
}
\description{
	Function that returns parameters of fit for the intersections between theory and the empirical solution.
}
\usage{
theory.fit(theory_data)
}
\arguments{
	\item{theory_data}{
		A data frame with the intersections between theory and empirics. 
		Usually the output of the theory.data function should be inputed.
	}
	}
\references{
Schneider, C. Q., Wagemann, C. 2012. Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis. Cambridge: Cambridge University Press, chapter 11.3
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

# Get the intermediate solution:

sol_yi <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Specify the theory. Let's assume the theory says that the 
# absence of EMP and the presence of MA is sufficient for EXPORT:

t<-"~EMP*MA"

# Get membership of cases in the intersections between theory and 
# the second intermediate solution and place it in an object:

INT <- theory.data(theory = t, empirics = sol_yi, outcome = "EXPORT", sol = 2)

# Get the parameters of fit for the intersections between theory and the empirical solution
# using the output of the theory.data function:

theory.fit(INT)

}
\keyword{QCA}
