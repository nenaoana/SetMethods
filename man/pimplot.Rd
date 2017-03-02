\name{pimplot}
\alias{pimplot}
\title{
	Prime implicants, truth table rows, and necessity plots.
}
\description{
		A function that displays XY plots for each sufficient term and the solution formula plotted against the outcome from an object of class "qca" (obtained by using the \code{eqmcc} function in package \code{QCA}). The function can also plot truth table rows against the outcome. Additionally, the function can plot results obtained from necessity analyses using an object of class "sS" (obtained by using the \code{superSubset} function in package \code{QCA}).
}
\usage{
pimplot(data, results, outcome, incl.tt = NULL , ttrows = c() , 
        necessity = FALSE, intermed = FALSE, sol = 1, case_labels = TRUE, 
        lab_color = rgb(0,0,0,0.5), lab_jitter = FALSE)
}
\arguments{
  \item{data}{
		A dataframe with the name of the outcome and of the conditions in capital letters. 
	}
	\item{results}{
		An object of class "qca" when necessity is \code{FALSE}. An object of class "sS" when necessity is \code{TRUE}
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. 
	}
	\item{incl.tt}{
		A numerical vector of length 1 specifying the row consistency threshold above which it 
		should plot truth table rows. By default it is \code{NULL} and the function will produce 
		plots using "qca" or "sS" objects. If a numerical value is specifyied, then it 
		automatically only plots truth table rows above that consistency value.
	}
	\item{ttrows}{
		A vector of character strings specifying the names of the truth table rows to be printed.
		By default this vector is empty and the function will produce plots using "qca" or "sS" objects. 
		If a value is specifyied, then it automatically only plots those particular truth table rows.
		N.B. This argument cannot be used simultaneously with the \code{incl.tt} argument.
	}
	\item{necessity}{
		logical. It indicates if the output should be for the results of sufficiency or necessity analyses. 
		By default, \code{FALSE}, the function works with an object of class "qca" obtained from the 
		\code{eqmcc} function in package \code{QCA}. When it set to \code{TRUE} the function returns plots for     an object of class "sS" obtained from the \code{superSubset} function in package \code{QCA}.
	}
	\item{intermed}{
		Logical. Use the intermediate solution? 
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
	\item{case_labels}{
		Logical. Print case labels?
	}
	\item{lab_color}{
		Color of the labels.
	}
	\item{lab_jitter}{
		Logical. Add jitter to label positions?
	}
}
\value{
	XY plots.
}
\references{
Haesebrouck, T. (2015) Pitfalls in QCA's consistency measure. Journal of Comparative Politics 2:65-80.

Schneider, C. Q., Rohlfing, I. 2013. Combining QCA and Process Tracing in Set-Theoretic Multi-Method Research. Sociological Methods Research 42(4): 559-597
}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\seealso{
	\code{\link[QCAGUI:eqmcc]{eqmcc}}
	\code{\link[SetMethods:pimdata]{pimdata}}
}
\examples{
# Import your data. For example:

data(Schneider)

# Get the parsimonious solution:


sol_yp <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Plot the prime implicants of the parsimonious solution:

pimplot(data = Schneider, results = sol_yp, outcome = "EXPORT")

# Plot the prime implicants of the first intermediate solution:

pimplot(data = Schneider, results = sol_yi, outcome = "EXPORT", intermed=TRUE, sol = 1)

# Plot all truth table rows with a consistency higher than 0.9:

pimplot(data=Schneider, results = sol_yi, incl.tt=0.9, outcome = "EXPORT", intermed=TRUE, sol = 1)

# Plot truth table rows "60" and "61":

pimplot(data=Schneider, results = sol_yi, ttrows =c("60","61"), outcome = "EXPORT", intermed=TRUE, sol = 1)

# For plotting results of necessity analyses using superSubset,
# the first stept is to obtain an "sS" object:

SUPSUB <- superSubset(Schneider, outcome="EXPORT", 
                      conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"), 
                      relation = "necessity", incl.cut = 0.8)
SUPSUB

# This can be imputed as result and necessity should be set to \code{TRUE}:

pimplot(data = Schneider, results = SUPSUB, outcome = "EXPORT", necessity = TRUE)

}
\keyword{ QCAGUI }
