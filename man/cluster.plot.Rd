\name{cluster.plot}
\alias{cluster.plot}
\title{
Function for plotting pooled, between, and within consistencies for a cluster diagnostics.
}
\description{
Function for plotting pooled, between, and within consistencies for a cluster diagnostics. For a sufficient solution, the function returns plots for the entire solution and each sufficient term.
}
\usage{
cluster.plot(cluster.res,
           labs = TRUE,
           size = 5,
           angle = 0,
           wicons = FALSE,
           wiconslabs = FALSE,
           wiconssize = 5,
           wiconsangle = 90)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster.res}{An object of class "cluster.res". This is the result of a cluster diagnostics obtained with the cluster() function.
}
\item{labs}{
Logical. Should labels for the clusters be printed?  
}
\item{size}{
Label font size for the clusters.
}
\item{angle}{
Label rotation for the clusters.
}
\item{wicons}{
Logical. Should within consistency plots be returned?
}
\item{wiconslabs}{
Logical. Should labels for the units be printed?  
}
\item{wiconssize}{
Label font size for the units.
}
\item{wiconsangle}{
Label rotation for the units.
}
}
\references{
Garcia-Castro, Roberto, and Miguel A. Arino. 2016. "A General Approach to Panel Data Set-Theoretic Research."" Journal of Advances in Management Sciences & Information Systems 2: 6376.
}
\author{
Ioana-Elena Oana}
\examples{
# Load the data:
data(PAYF)

# Create a sufficient solution using minimize:
PS <- minimize(data = PAYF,
                        outcome  = "HL",
                        conditions = c("HE","GG","AH","HI","HW"),
                        incl.cut = 0.9,
                        n.cut = 2,
                        include = "?",
                        details = TRUE, 
                        show.cases = TRUE)
PS

# Perform cluster diagnostics:

CB <- cluster(data = PAYF,
           results = PS,
           outcome = "HL",
           unit_id = "COUNTRY",
           cluster_id = "REGION",
           necessity=FALSE,
           wicons = FALSE)
CB

# Plot pooled, between, and within consistencies:

cluster.plot(cluster.res = CB,
             labs = TRUE,
             size = 8,
             angle = 6,
             wicons = TRUE)

}
\keyword{QCA}
