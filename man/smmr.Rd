\name{smmr}
\alias{smmr}
\title{
	Function for performing set-theoretic multi-method research.
}
\description{
	A function that selects best available cases for single case studies and best pairs of matching cases for comparative case studies.
}
\usage{
smmr(results, outcome, sol = 1, match = NULL, 
      cases = NULL, max_pairs = 5, term = 1, nec.cond =NULL, necessity = FALSE, ...)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome. If the negated outcome is analyzed, one can 
		also write for example outcome = "~Y", however this is unnecessary as the values in the outcome 
		will be taken from the appropriate minimized solution.
	}
  \item{sol}{
		A vector where the first number indicates the number of the
		conservative or parsimonious solution according to the order in the "qca" object.
		For more complicated structures of model ambiguity, the intermediate solution can also be specified
		by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution
		and i = intermediate solution.
	}
	\item{match}{
		Logical. Should comparative SMMR be used?
	}
	\item{cases}{
	  A numerical vector indicating the type of cases to be returned.
		\cr For single case studies for SUFFICIENCY: \cr \verb{ 1 = typical cases,}
		                         \cr \verb{ 2 = typical cases for each focal conjunct in a sufficient term,}   
		                         \cr \verb{ 3 = deviant consistency,}
		                         \cr \verb{ 4 = deviant coverage,}
		                         \cr \verb{ 5 = individually irrelevant,}
		                         \cr \verb{ 6 = all of the above;}
		\cr For comparative case studies for SUFFICIENCY:\cr \verb{ 1 = Typical-Typical for each focal conjuct in a sufficient term,}
		                              \cr \verb{ 2 = Typical-IIR for each focal conjuct in a sufficient term,}
		                              \cr \verb{ 3 = Typical-Dev.Cons.,}
		                              \cr \verb{ 4 = Dev.Cov.-IIR;}
		                              \cr \verb{ 5 = Typical-Typical for each term;}
                                  \cr \verb{ 6 = Typical-IIR for each term;}
		                              \cr \verb{ 7 = all of the above;}
		\cr For single case studies for NECESSITY: \cr \verb{ 1 = typical cases,}
		                         \cr \verb{ 2 = deviant consistency,}
		                         \cr \verb{ 3 = deviant relevance,}
		                         \cr \verb{ 4 = all of the above;}
		\cr For comparative case studies for NECESSITY:\cr \verb{ 1 = Typical-Deviant Relevance ,}
		                              \cr \verb{ 2 = Typical-Deviant Consistency,}
		                              \cr \verb{ 3 = Typical-IIR,}
		                              \cr \verb{ 4 = Typical-Typical,}
		                              \cr \verb{ 5 = all of the above;}
	}
	\item{max_pairs}{
		Maximum number of pairs to extract.
	}
	\item{term}{
		A numeric vector where the first number indicates the number of the
		term according to the order in the "qca" object.
	}
	\item{nec.cond}{
		A character vector specifying the necessity results obtained. This could be of the form "A", "A+B", or "A*B", making sure no spaces are left between the name of the conditions and the Boolean operator. Notice than nec.cond can also be specified for sufficiency results in single SMMR typical cases for each focal conjunct in a sufficient term, and for comparative SMMR in typical-typical and typical-iir comparisons for each focal conjunct. Note that in the later case only expressions of the forms "A" or "A+B" are accepted. If dealing with a necessary condition of the form "A*B", each of the individual conditions should be entered separately when performing analyses of sufficiency.
	}
	\item{necessity}{
		Logical. Should SMMR results for necessity be shown?
	}
	\item{...}{
		Deprecated arguments (neg.out, use.tilde)
	}
}
\references{Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods and Research 42(4): 559-97
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

## SUFFICIENCY SMMR examples:

# Get the parsimonious solution:

sol_yp <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Get typical cases for each focal conjunct in the third term of the parsimonious solution:

smmr(results = sol_yp, outcome = "EXPORT", match=FALSE, cases=2, term = 3)

# Get typical cases for each focal conjunct in the third term of the parsimonious solution
# and specifying STOCK as a necessary condition:

smmr(results = sol_yp, outcome = "EXPORT", 
      match=FALSE, cases=2, term = 3, nec.cond="STOCK")

# Get matching typical-typical cases for the second term of the parsimonious solution:

smmr(results = sol_yp, outcome = "EXPORT", match=TRUE, cases=1, term = 2)

# Get matching typical-DCN cases:

smmr(results = sol_yp, outcome = "EXPORT", match=TRUE, cases=3)

## NECESSITY SMMR examples:

# Imagine you found condition "STOCK + MA" is nececssary for outcome "EXPORT".

# Get typical cases for each disjunct of the necessary condition:

smmr(results = sol_yp, outcome = "EXPORT", match=FALSE, cases=1, 
     nec.cond = "STOCK+MA", necessity = TRUE)

# Get typical - deviant relevance cases for each disjunct of the necessary condition:

smmr(results = sol_yp, outcome = "EXPORT", match=TRUE, cases=1, 
     nec.cond = "STOCK+MA", necessity = TRUE)


}
\keyword{QCA}
