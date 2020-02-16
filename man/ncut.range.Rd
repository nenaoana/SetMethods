\name{ncut.range}
\alias{ncut.range}
\title{
Function for identifying the frequency threshold range within which the Boolean formula for the solution does not change.
}
\description{
Function for identifying the frequency threshold range for a truth table within which the Boolean formula for the solution does not change. The function gradually increases and, then, decreases an inital selected threshold by the value specifyied in the step argument and checks whether the solution formula changes for finding the lower and upper ranges for the frequency threshold. The function performs this iteration for the number of times specified in the max.runs argument. If the solution formula does not change given the number of runs specified, it will return an NA, meaning that it could not find a limit to the range.
}
\usage{
ncut.range(data,
    step = 1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    dir.exp = "",
    ...)
}
\arguments{
  \item{data}{A data frame containing the calibrated data for the sufficient solution.
}
 \item{step}{The value to be gradually added and subtracted from the threshold tested.
}
 \item{max.runs}{The maximum number of times the step value gets gradually added and subtracted.
}
 \item{outcome}{A character string with the name of the outcome in capital letters. For the negated outcome a tilde "~" should be used. This had the same usage as the outcome argument in the minimize function.
}
 \item{conditions}{
 A vector of character strings containing the names of the conditions.This had the same usage as the conditions argument in the minimize function.
}
 \item{incl.cut}{
 The raw consistency threshold for the truth table rows.
}
 \item{n.cut}{
 The frequency threshold for the truth table rows.
}
 \item{include}{
 A vector of other output values (for example "?" for logical remainders) to include in the minimization. This had the same usage as the include argument in the minimize function.
}
 \item{dir.exp}{
 A vector of directional expectations for the intermediate solution. This had the same usage as the dir.exp argument in the minimize function.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Oana, Ioana-Elena, and Carsten Q. Schneider. 2020. Robustness tests in QCA: A fit-oriented and case-oriented perspective using R. Unpublished Manuscript.

Oana, Ioana-Elena, Carsten Q. Schneider, and Eva Thomann (forthcoming). Qualitative Comparative Analysis (QCA) using R: A Gentle Introduction. Cambridge: Cambridge University Press.
}
\author{
Ioana-Elena Oana
}
\examples{
# Load the calibrated data:
data(PAYF)

# Check frequency ranges:
ncut.range(
  data = PAYF,
  step = 1,
  max.runs = 10,
  outcome  = "HL",
  conditions = c("HE","GG","AH","HI","HW"),
  incl.cut = 0.87,
  n.cut = 2,
  include = "?"
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{QCA}
\keyword{robustness}