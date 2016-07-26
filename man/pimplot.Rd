\name{pimplot}
\alias{pimplot}
\title{
	Prime implicants plots.
}
\description{
	A function that returns prime implicants plots from an object of class "qca".  
}
\usage{
pimplot(results, outcome, intermed=FALSE, sol = 1, case_labels=TRUE, lab_color=rgb(0,0,0,0.5), lab_jitter=FALSE)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. 
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
\details{
}
\value{
	A plot.
}
\references{
}
\author{
	Juraj Medzihorsky
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
	\code{\link[SetMethods:pimdata]{pimdata}}
}
\examples{
# Import your data. For example:

data(Schneider)

# Get the truth table:

TT_y <- truthTable(Schneider, outcome = "EXPORT",
                   conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                   incl.cut1 = .9,
                   complete = TRUE,
                   PRI = TRUE,
                   sort.by = c("out", "incl", "n"))

# Get the parsimonious solution:

sol_yp <- eqmcc(TT_y, include = "?", details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- eqmcc(TT_y, include = "?", details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))

# Plot the prime implicants of the parsimonious solution:

pimplot(results = sol_yp, outcome = "EXPORT")

# Plot the prime implicants of the first intermediate solution:

pimplot(results = sol_yi, outcome = "EXPORT", intermed=TRUE, sol = 1)

}
\keyword{ QCA }
