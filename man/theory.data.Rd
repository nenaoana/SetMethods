\name{theory.data}
\alias{theory.data}
\title{
	Membership of cases in the intersections between theory and the empirical solution.
}
\description{
	Function that returns membership of cases in the intersections between theory and 
	the empirical solution in the form of a data frame.
}
\usage{
theory.data(theory, empirics, outcome, sol = 1, use.tilde = TRUE)
}
\arguments{
	\item{theory}{
		A character string specifying the theory.
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
		If use.tilde is set to TRUE, conditions should be capitalized and negated conditions should be inserted with     a "~". If use.tilde is set to FALSE, negation can be denoted with lowercase letters, but the output will        always transform negation to a tilde.
	}
	\item{empirics}{
		An object of class 'qca'. When performing analyses for the negated outcome, just use the 
		results from the \code{minimize()} function for the negation of the outcome.
	}
	\item{outcome}{
		A character string with the name of the outcome. When performing analyses of the sufficient solution for the negated outcome one must only use the \code{minimize()} result from the sufficiency analysis of the negated outcome in the argument \code{empirics}. Changing the name in the argument \code{outcome} or using a tilde is not necessary.
	}
	\item{sol}{
		A vector where the first number indicates the number of the conservative or parsimonious solution 
		according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{use.tilde}{
	Logical. If the theory or the solution formula do not use tilde for negation, set use.tilde to FALSE to         transform negation with a tilde.
	}
}
%\details{}
\value{ It returns a data frame containing the intersections between theory and empirical findings.
}
\references{Ragin, C. C. 1987. The Comparative Method: Moving Beyond Qualitative and Quantitative Strategies. Berkeley: University of California Press, pp. 118-121

Schneider, C. Q., Wagemann, C. 2012. Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis. Cambridge: Cambridge University Press, chapter 11.3
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
# the second intermediate solution:

theory.data(theory = t, empirics = sol_yi, outcome = "EXPORT", sol = 2)

}
\keyword{QCA}
