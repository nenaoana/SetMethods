\name{cluster}
\alias{cluster}
\title{
	Diagnostic tool for clustered data.
}
\description{
	Function returns pooled, within, and between consistencies for the relationship between two sets, for an       object of class "qca", and for a Boolean expression.
}
\usage{
cluster(data=NULL, results, outcome, 
      unit_id, cluster_id, sol = 1, 
      necessity = FALSE, wicons = FALSE)
}
\arguments{
  \item{data}{
		A data frame in the long format containing both a colum with the unit names and a column with 
		the cluster names. Column names should be in capital letters.
	}
	\item{results}{
		An object of class "qca". For performing cluster diagnostics of the sufficient solution for the negated        outcome one must only use the \code{minimize()} result from the sufficiency analysis of the negated outcome.
		The argument results can also be a vector, a character string, or a boolean expression of the form 
		e.g. "A*~B + ~B*C".
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing cluster 
		diagnostics of the sufficient solution for the negated outcome one must only use the \code{minimize()}            result from the sufficiency analysis of the negated outcome in the argument \code{results}. 
		When performing cluster diagnostics for boolean expressions or vectors the negated outcome can be used by      inserting a tilde in the outcome name in the argument \code{outcome}.
    The outcome can also be a vector. 
	}
	\item{unit_id}{
		A character string with the name of the vector containing the units (e.g. countries).
	}
	\item{cluster_id}{
		A character string with the name of the vector containing the clustering units (e.g. years).
	}
	\item{sol}{
		A vector where the first number indicates the number of the conservative or parsimonious solution 
		according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{necessity}{
		Logical. Perform the diagnostic for the relationship of necessity?
	}
	\item{wicons}{
		Logical. Should within consistencies and coverages be printed?
	}
}
\references{
Garcia-Castro, Roberto, and Miguel A. Arino. 2016. "A General Approach to Panel Data Set-Theoretic Research."" Journal of Advances in Management Sciences & Information Systems 2: 6376.
}
\author{
	Ioana-Elena Oana
}
\seealso{
	\code{\link[QCA:minimize]{minimize}}
}
\examples{

# Import your clustered data in the long format. 
# For example:

data(SCHLF)

# Get the intermediate solution:

sol_yi <- minimize(SCHLF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Get pooled, within, and between consistencies for the intermediate solution:

cluster(SCHLF, sol_yi, "EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", sol = 1)
              
# or:

cluster(SCHLF, sol_yi, "EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", sol = "c1p1i1")

# Get pooled, within, and between consistencies for EMP as necessary for EXPORT:

cluster(SCHLF, results="EMP", outcome="EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", necessity=TRUE)
# or:

cluster(results=SCHLF$EMP, outcome=SCHLF$EXPORT, unit_id = SCHLF$COUNTRY, 
              cluster_id = SCHLF$YEAR, necessity=TRUE)
              
# Get pooled, within, and between consistencies for ~EMP as necessary for EXPORT:

cluster(SCHLF, results="~EMP", outcome="EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR", necessity=TRUE)
# or:

cluster(results=1-SCHLF$EMP, outcome=SCHLF$EXPORT, unit_id = SCHLF$COUNTRY, 
              cluster_id = SCHLF$YEAR, necessity=TRUE)                  
              
# Get pooled, within, and between consistencies for EMP*~MA*STOCK as sufficient for EXPORT:

cluster(SCHLF, "EMP*~MA*STOCK", "EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR") 
              
# Get pooled, within, and between consistencies for EMP*MA + ~STOCK as sufficient for ~EXPORT:

cluster(SCHLF, "EMP*MA + ~STOCK", "~EXPORT", unit_id = "COUNTRY", 
              cluster_id = "YEAR")              
}
\keyword{QCA}
