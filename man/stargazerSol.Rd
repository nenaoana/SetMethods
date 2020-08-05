\name{stargazerSol}
\alias{stargazerSol}
\title{
Function for exporting a sufficienct solution from minimize in latex, html, or text format.}
\description{
Function for exporting a sufficienct solution from minimize in latex, html, or text format.
}
\usage{
stargazerSol(results,
           outcome,
           sol = 1,
           show.cases = FALSE,
           type = "latex", 
           title = "",
           out = NULL,
           digits = 3)
}
\arguments{
  \item{results}{An object of class "qca": a sufficient solution obtained with the minimize function.
}
\item{outcome}{A character string with the name of the outcome in capital letters. When performing pimplot of the sufficient solution for the negated outcome one must only use the minimize() result from the sufficiency analysis of the negated outcome in the argument results. Changing the name in the argument outcome or using a tilde is not necessary, but recommended.
}
\item{sol}{A vector where the first number indicates the number of the conservative or parsimonious solution according to the order in the "qca" object. For more complicated structures of model ambiguity, the intermediate solution can also be specified by using a character string of the form "c1p3i2" where c = conservative solution, p = parsimonious solution and i = intermediate solution.
}
\item{show.cases}{ Logical. Should the names of cases be printed?
}
\item{type}{character string that specifies what type of output the command should produce. The possible values are "latex" (default), "html", "text".
}
\item{title}{title for the table.
}
\item{out}{name of the file to be saved containing the extension (e.g. "mysol.tex", "mysol.txt")
}
\item{digits}{To how many digits should the parameters of fit be rounded up.
}
}
\references{
Oana, Ioana-Elena, Carsten Q. Schneider, and Eva Thomann (forthcoming). Qualitative Comparative Analysis (QCA) using R: A Gentle Introduction. Cambridge: Cambridge University Press.
}
\author{
Ioana-Elena Oana}
\examples{
# Import your data. For example:

data(SCHF)

# Get the parsimonious solution:


sol_yp <- minimize(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)
                
# Print in latex format:

stargazerSol(sol_yp, "EXPORT")
}
\keyword{QCA}
