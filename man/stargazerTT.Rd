\name{stargazerTT}
\alias{stargazerTT}
\title{
Function for exporting a sufficienct solution from minimize in latex, html, or text format.}
\description{
Function for exporting a sufficienct solution from minimize in latex, html, or text format.
}
\usage{
stargazerTT(truthtable,
           show.cases = FALSE,
           type = "latex", 
           title = "",
           out = NULL,
           digits = 3)
}
\arguments{
  \item{truthtable}{A truth table obtained with the truthTable function.
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

# Get the truth table:

mytt <- truthTable(SCHF, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut = .9, complete = TRUE)

# Export as latex:
stargazerTT(mytt)

# Export as text:
stargazerTT(mytt, type = "text")

}
\keyword{QCA}
