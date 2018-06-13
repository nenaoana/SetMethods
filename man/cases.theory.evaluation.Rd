\name{cases.theory.evaluation}
\alias{cases.theory.evaluation}
\title{
	Names of cases in the intersections between theory and the empirical solution.
}
\description{
Function that returns names of cases in the intersections between theory and the empirical solution.
}
\usage{
cases.theory.evaluation(theory_data)
}
\arguments{
	\item{theory_data}{
		A data frame with the intersections between theory and empirics. 
		Usually the output of the theory.data function should be inputed.
	}
}
\references{Ragin, C. C. 1987. The Comparative Method: Moving Beyond Qualitative and Quantitative Strategies. Berkeley: University of California Press, pp. 118-121.

Schneider, C. Q., Wagemann, C. 2012. Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis. Cambridge: Cambridge University Press, chapter 11.3.
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
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

# Get the names of cases in the intersections between theory and the empirical solution
# using the output of the theory.data function:

cases.theory.evaluation(INT)

}
\keyword{QCA}
