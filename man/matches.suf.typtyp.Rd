\name{matches.suf.typtyp}
\alias{matches.suf.typtyp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matches.suf.typtyp(results, outcome, term = 1, neg.out = FALSE, intermed = FALSE, sol = 1, max_pairs = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
}
  \item{term}{
%%     ~~Describe \code{term} here~~
}
  \item{neg.out}{
%%     ~~Describe \code{neg.out} here~~
}
  \item{intermed}{
%%     ~~Describe \code{intermed} here~~
}
  \item{sol}{
%%     ~~Describe \code{sol} here~~
}
  \item{max_pairs}{
%%     ~~Describe \code{max_pairs} here~~
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
function (results, outcome, term = 1, neg.out = FALSE, intermed = FALSE, 
    sol = 1, max_pairs = 5) 
{
    pdata <- pimdata(results = results, outcome = outcome, intermed = intermed, 
        sol = sol)
    outcome <- toupper(outcome)
    nterm <- colnames(pdata[term])
    data <- results$tt$initial.data
    data1 <- data.frame(matrix(NA, ncol = 0, nrow = nrow(data)))
    row.names(data1) <- row.names(data)
    tl <- gsub("\\s", "", nterm)
    tl <- strsplit(tl, "\\*")
    tn <- unique(unlist(tl))
    t_pre <- toupper(tn)[toupper(tn) == tn]
    t_neg <- toupper(tn)[tolower(tn) == tn]
    if (length(t_pre) > 0) {
        data1[t_pre] <- data[t_pre]
        colnames(data1[t_pre]) <- toupper(colnames(data1[t_pre]))
    }
    if (length(t_neg) > 0) {
        data1[t_neg] <- 1 - data[t_neg]
        colnames(data1[t_neg]) <- tolower(colnames(data1[t_neg]))
    }
    for (i in (1:length(tn))) {
        print(paste("Focal Conjunct", tn[i], sep = " "))
        x <- data1[toupper(tn[i])]
        if (!neg.out) {
            y <- data[outcome]
        }
        else {
            y <- 1 - data[outcome]
        }
        co <- tn[-grep(tn[i], tn)]
        co <- toupper(co)
        codata <- data1[co]
        if (ncol(codata) > 1) {
            a <- do.call(pmin, codata[, ])
            codata1 <- data.frame(a)
            row.names(codata1) <- row.names(codata)
        }
        else {
            codata1 <- codata
            names(codata1)[1] <- "a"
        }
        codata$term <- pmin(codata1$a, x[, ])
        typical <- (codata$term > 0.5) & (y > 0.5) & (codata$term <= 
            y)
        typ1 <- (x <= codata1$a)
        typ2 <- (x > codata1$a)
        d <- rownames(data1)[typical]
        e <- rownames(data1)[typical]
        if (identical(e, character(0))) {
            print("no typical cases")
        }
        else {
            casesm <- c()
            casesn <- c()
            val <- c()
            order <- c()
            for (m in d) {
                for (n in e) {
                  s <- (((y[m, ] - x[m, ]) + (y[n, ] - x[n, ]) + 
                    (1.5 - (x[m, ] + x[n, ])))/((x[m, ] + x[n, 
                    ])))
                  sm <- ((y[m, ] - codata[m, "term"])/codata[m, 
                    "term"])
                  sn <- ((y[n, ] - codata[n, "term"])/codata[n, 
                    "term"])
                  if (sm >= sn) {
                    casesm <- c(casesm, m)
                    casesn <- c(casesn, n)
                    val <- c(val, s)
                  }
                  if ((x[m, ] <= codata1[m, ]) & (x[n, ] <= codata1[n, 
                    ])) {
                    order <- c(order, 1)
                  }
                  else {
                    if ((x[m, ] <= codata1[m, ]) & (x[n, ] > 
                      codata1[n, ])) {
                      order <- c(order, 2)
                    }
                    else {
                      if ((x[m, ] > codata1[m, ]) & (x[n, ] <= 
                        codata1[n, ])) {
                        order <- c(order, 3)
                      }
                      else {
                        if ((x[m, ] > codata1[m, ]) & (x[n, ] > 
                          codata1[n, ])) {
                          order <- c(order, 4)
                        }
                      }
                    }
                  }
                }
            }
            matcres <- data.frame(casesm, casesn, val, order)
            matcres[, 5] <- NA
            matcres[, 6] <- NA
            colnames(matcres) <- c("Typical1", "Typical2", "Distance", 
                "PairRank", "UniqCov1", "UniqCov2")
            R <- cases.suf.typ(results = results, outcome = outcome, 
                neg.out = neg.out, intermed = intermed, sol = sol)
            for (u in 1:nrow(matcres)) {
                for (uu in 1:nrow(R)) {
                  if (as.character(matcres[u, 1]) == as.character(R[uu, 
                    1])) {
                    matcres[u, 5] <- R[uu, 7]
                  }
                  if (as.character(matcres[u, 2]) == as.character(R[uu, 
                    1])) {
                    matcres[u, 6] <- R[uu, 7]
                  }
                }
            }
            maxl <- min(max_pairs, nrow(matcres))
            matcres <- matcres[order(matcres$PairRank, -matcres$UniqCov1, 
                -matcres$UniqCov2, matcres$Distance), ]
            print(head(matcres, maxl))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
