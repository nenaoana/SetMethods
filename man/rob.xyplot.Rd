\name{rob.xyplot}
\alias{rob.xyplot}
\title{
Function  for plotting an initial solution against the test set.}
\description{
Function  for plotting an initial solution against the test set.}
\usage{
rob.xyplot(test_sol, 
            initial_sol, 
            outcome,
            all_labels = FALSE,
            jitter = TRUE,
            fontsize = 3,
            labs = TRUE)
}
\arguments{
  \item{test_sol}{
The different alternative solutions created with minimize() and placed in a list using list().
}
\item{initial_sol}{
The initial solution created with minimize().
}
\item{outcome}{
A character string containing the name of the outcome.
}
\item{all_labels}{
Logical. Should all the case labels be printed? If FALSE, only shaky and possible cases are printed.
}
\item{jitter}{
Logical. Should case labels be jitter so as not to overlap?
}
\item{fontsize}{
The size of the font for case labels.
}
\item{labs}{
Logical.Should case labels be printed?
}}
\references{
Oana, Ioana-Elena, and Carsten Q. Schneider. 2020. Robustness tests in QCA: A fit-oriented and case-oriented perspective using R. Unpublished Manuscript.

Oana, Ioana-Elena, Carsten Q. Schneider, and Eva Thomann (forthcoming). Qualitative Comparative Analysis (QCA) using R: A Gentle Introduction. Cambridge: Cambridge University Press.
}
\author{
Ioana-Elena Oana
}
\examples{
# Load the data:
data(PAYF)

# Store the name of the conditions in one vector:
conds <-  c("HE","GG","AH","HI","HW")

# Create several solutions:

# The initial solution
IS <- minimize(data = PAYF,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.87,
                n.cut = 2,
                include = "?",
                details = TRUE, 
                show.cases = TRUE)

# altering consistency
TS1 <- minimize(data = PAYF,
                 outcome  = "HL",
                 conditions = conds,
                 incl.cut = 0.7,
                 n.cut = 2,
                 include = "?",
                 details = TRUE, show.cases = TRUE)

#altering n.cut
TS2 <- minimize(data = PAYF,
                 outcome  = "HL",
                 conditions = conds,
                 incl.cut = 0.87,
                 n.cut = 1,
                 include = "?",
                 details = TRUE, show.cases = TRUE)
                 
# Create the test set in a list:
TS <- list(TS1, TS2)

# Plotting the initial solution against the test set:

rob.xyplot(test_sol = TS, 
                  initial_sol = IS, 
                  outcome = "HL", 
                  fontsize = 2.5, 
                  jitter=TRUE)
}
\keyword{QCA}