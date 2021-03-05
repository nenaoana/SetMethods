\name{rob.singletest}
\alias{rob.singletest}

\title{
Function returning robustness parameters for each single test solution in TS.
}
\description{
Function returning robustness parameters for each single test solution in TS. The function returns the set-coincidence and RC_Rank between each alternative solution and the initial solution.
}
\usage{
rob.singletest(test_sol, 
              initial_sol, 
              outcome)
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
}
\references{
Oana, Ioana-Elena, and Carsten Q. Schneider. 2021. Robustness tests in QCA: A fit-oriented and case-oriented perspective using R. Unpublished Manuscript.

Oana, Ioana-Elena, Carsten Q. Schneider, and Eva Thomann (2021). Qualitative Comparative Analysis (QCA) using R: A Gentle Introduction. Cambridge: Cambridge University Press.
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

# Calculate robustness parameters, i.e. the ratio of the parameters
# of fit for the core vis-a-vis for the initial solution:

RST <- rob.singletest(test_sol = TS, 
           initial_sol = IS,  
           outcome = "HL")
RST
}
\keyword{QCA}