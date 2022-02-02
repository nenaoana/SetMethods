\name{theory.evaluation}
\alias{theory.evaluation}
\title{
	Performs theory evaluation.
}
\description{
	Function that returns membership of cases in the intersections between theory and 
	the empirical solution in the form of a data frame, the names of cases in the intersections 
	between theory and the empirical solution, and the parameters of fit for these intersections.
}
\usage{
theory.evaluation(theory, empirics, outcome, sol = 1, print.fit=FALSE, 
                  print.data=FALSE, consH = FALSE, ...)
}
\arguments{
	\item{theory}{
		A character string specifying the theory.
		Unions of conditions are performed with a "+", while intersections are performed with a "*".
		Conditions should be capitalized and negated conditions should be inserted with a "~".
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
	\item{print.data}{
		Logical. Print also the membership of cases in all the intersections between theory and empirics?
	}
	\item{print.fit}{
		Logical. Print also the parameters of fit for the intersections between theory and empirics?
	}
	\item{consH}{
		Logical. Print also the Haesebrouck's consistency among the parameters of fit?
	}
	\item{...}{
	Deprecated arguments (use.tilde).
	}
}
%\details{}
\references{Ragin, C. C. 1987. The Comparative Method: Moving Beyond Qualitative and Quantitative Strategies. Berkeley: University of California Press, pp. 118-121

Schneider, C. Q., Wagemann, C. 2012. Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis. Cambridge: Cambridge University Press, chapter 11.3
}
\author{
	Ioana-Elena Oana
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
}
\examples{
\dontrun{
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


# Perform theory evaluation (get only the names of the cases and the Boolean intersections):

TH <- theory.evaluation(theory = t, empirics = sol_yi, outcome = "EXPORT", sol = 1, 
                  print.data=FALSE, print.fit=FALSE)
TH

# Get only the case names:

TH$cases

# Or only the parameters of fit:

TH$fit}

}
\keyword{QCA}
