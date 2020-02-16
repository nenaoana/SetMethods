\name{calib.range}
\alias{calib.range}
\title{
Function for identifying the calibration threshold ranges for a condition within which the Boolean formula for the solution does not change.
}
\description{
Function for identifying the calibration threshold ranges for a conditio within which the Boolean formula for the solution does not change. The function gradually increases and, then, decreases each of the three (0, 0,5, 1) calibration thresholds of a condition by the value specifyied in the step argument and checks whether the solution formula changes. The function performs this iteration for the number of times specified in the max.runs argument. If the solution formula does not change given the number of runs specified, it will return an NA, meaning that it could not find a limit to the range.
}
\usage{
calib.range(raw.data,
    calib.data,
    test.cond,
    test.thresholds,
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
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{raw.data}{A data frame containing the raw data used to test the calibration thresholds for the chosen condition.
}
\item{calib.data}{A data frame containing the calibrated data for the sufficient solution.
}
\item{test.cond}{A character string specifying the name of the condition for which to test the calibration ranges. Note that the name of the conditions must be the same in both the raw and the calibrated data.
}
\item{test.thresholds}{
The initial qualitative anchors used for calibrating the chosen condition.
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
\details{
%%  ~~ If necessary, more details than the description above ~~
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
data(LIPF)

# Load the raw data:
data(LIPR)

# Get calibration ranges for condition DEVELOPED:

calib.range(
  raw.data = LIPR,
  calib.data = LIPF,
  test.cond = "DEVELOPED",
  test.thresholds = c(400, 550, 900),
  step = 500,
  max.runs = 10,
  outcome  = "SURVIVED",
  conditions = c("URBAN","LITERATE","INDUSTRIAL","STABLE"),
  incl.cut = 0.8,
  n.cut = 1,
  include = "?"
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{QCA}
\keyword{robustness}