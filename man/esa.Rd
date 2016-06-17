\name{esa}
\alias{esa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
esa(oldtt, nec_cond = c(), imposs_LR = c(), contrad_rows = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oldtt}{
%%     ~~Describe \code{oldtt} here~~
}
  \item{nec_cond}{
%%     ~~Describe \code{nec_cond} here~~
}
  \item{imposs_LR}{
%%     ~~Describe \code{imposs_LR} here~~
}
  \item{contrad_rows}{
%%     ~~Describe \code{contrad_rows} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (oldtt, nec_cond = c(), imposs_LR = c(), contrad_rows = c()) 
{
    TT <- oldtt
    if (length(nec_cond) > 0) {
        for (i in 1:length(nec_cond)) {
            if (length(grep("+", nec_cond[i])) > 0) {
                tl <- gsub("\\s", "", nec_cond[i])
                tl <- unlist(strsplit(tl, "\\+"))
                ncon <- c()
                pcon <- c()
                for (i in 1:length(tl)) {
                  if (length(grep("~", tl[i])) > 0) {
                    cstr <- strsplit(tl[i], "~")
                    cstr1 <- unlist(cstr)
                    ncon <- c(ncon, cstr1[2])
                  }
                  else {
                    pcon <- c(pcon, tl[i])
                  }
                }
                if (length(ncon) > 0 & length(pcon) > 0) {
                  TT$tt[(TT$tt[, ncon] == 1) & (TT$tt[, pcon] == 
                    0) & (TT$tt$OUT == "?"), "OUT"] <- 0
                }
                else {
                  if (length(ncon) > 0) {
                    TT$tt[(TT$tt[, ncon] == 1) & (TT$tt$OUT == 
                      "?"), "OUT"] <- 0
                  }
                  else {
                    if (length(pcon) > 0) {
                      TT$tt[(TT$tt[, pcon] == 0) & (TT$tt$OUT == 
                        "?"), "OUT"] <- 0
                    }
                  }
                }
            }
            else {
                if (length(grep("~", nec_cond[i])) > 0) {
                  cstr <- strsplit(nec_cond[i], "~")
                  cstr1 <- unlist(cstr)
                  TT$tt[(TT$tt[, cstr1[2]] == 1) & (TT$tt$OUT == 
                    "?"), "OUT"] <- 0
                }
                else {
                  TT$tt[(TT$tt[, nec_cond[i]] == 0) & (TT$tt$OUT == 
                    "?"), "OUT"] <- 0
                }
            }
        }
    }
    if (length(imposs_LR) > 0) {
        for (i in 1:length(imposs_LR)) {
            if (length(grep("*", imposs_LR[i])) > 0) {
                tl <- gsub("\\s", "", imposs_LR[i])
                tl <- unlist(strsplit(tl, "\\*"))
                ncon <- c()
                pcon <- c()
                for (i in 1:length(tl)) {
                  if (length(grep("~", tl[i])) > 0) {
                    cstr <- strsplit(tl[i], "~")
                    cstr1 <- unlist(cstr)
                    ncon <- c(ncon, cstr1[2])
                  }
                  else {
                    pcon <- c(pcon, tl[i])
                  }
                }
                if (length(ncon) > 0 & length(pcon) > 0) {
                  TT$tt[(TT$tt[, ncon] == 0) & (TT$tt[, pcon] == 
                    1) & (TT$tt$OUT == "?"), "OUT"] <- 0
                }
                else {
                  if (length(ncon) > 0) {
                    TT$tt[(TT$tt[, ncon] == 0) & (TT$tt$OUT == 
                      "?"), "OUT"] <- 0
                  }
                  else {
                    if (length(pcon) > 0) {
                      TT$tt[(TT$tt[, pcon] == 1) & (TT$tt$OUT == 
                        "?"), "OUT"] <- 0
                    }
                  }
                }
            }
            else {
                if (length(grep("~", imposs_LR[i])) > 0) {
                  cstr <- strsplit(imposs_LR[i], "~")
                  cstr1 <- unlist(cstr)
                  TT$tt[(TT$tt[, cstr1[2]] == 0) & (TT$tt$OUT == 
                    "?"), "OUT"] <- 0
                }
                else {
                  TT$tt[(TT$tt[, nec_cond[i]] == 1) & (TT$tt$OUT == 
                    "?"), "OUT"] <- 0
                }
            }
        }
    }
    if (length(contrad_rows) > 0) {
        TT$tt[contrad_rows, "OUT"] <- 0
    }
    return(TT)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
