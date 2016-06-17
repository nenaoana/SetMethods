\name{matches.suf.dcviir}
\alias{matches.suf.dcviir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matches.suf.dcviir(results, outcome, neg.out = FALSE, intermed = FALSE, sol = 1, max_pairs = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
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
function (results, outcome, neg.out = FALSE, intermed = FALSE, 
    sol = 1, max_pairs = 5) 
{
    X <- pimdata(results = results, outcome = outcome, intermed = intermed, 
        sol = sol)
    n <- rownames(X)
    if (!neg.out) {
        y <- results$tt$initial.data[, outcome]
    }
    else {
        y <- 1 - results$tt$initial.data[, outcome]
    }
    FS <- results$tt$initial.data
    FS <- FS[, -which(colnames(FS) == outcome)]
    FA <- FS
    FA[FA <= 0.5] <- 1 - FA[FA <= 0.5]
    w <- apply(FA, 1, min)
    CS <- round(results$tt$initial.data)
    CS <- CS[, -which(colnames(CS) == outcome)]
    tt_row <- apply(CS, 1, function(i) paste(i, collapse = ""))
    x <- X[, "solution_formula"]
    y <- X[, "out"]
    devcove <- ((x < 0.5) & (y > 0.5)) & (w <= y)
    indirre <- ((x < 0.5) & (y < 0.5))
    rnt <- n[devcove]
    rnd <- n[indirre]
    K <- expand.grid(rnt, rnd)
    if (nrow(K) == 0) {
        warning("No pairs")
        return(NULL)
    }
    fil <- apply(K, 1, function(p) tt_row[p[1]] == tt_row[p[2]])
    K_fil <- K[fil, ]
    aux.f <- function(p) {
        i <- which(n == p[1])
        j <- which(n == p[2])
        s <- ((2 - (w[i] + w[j])) + (1 - (y[i] - y[j])))/(w[i] + 
            w[j])
        return(s)
    }
    s <- apply(K_fil, 1, aux.f)
    R <- data.frame(deviant_coverage = K_fil[, 1], individually_irrelevant = K_fil[, 
        2], distance = s, best_matching_pair = rep(FALSE, length(s)))
    CS$ids <- rownames(CS)
    R <- merge(R, CS, by.x = "deviant_coverage", by.y = "ids")
    colnames(R)[5:ncol(R)] <- paste("TT_", colnames(R)[5:ncol(R)], 
        sep = "")
    tt_row_fil <- apply(R[, grep("TT_", colnames(R))], 1, function(r) paste(r, 
        collapse = ""))
    R <- R[order(tt_row_fil), ]
    tt_row_fil <- tt_row_fil[order(tt_row_fil)]
    aux.list <- function(x) {
        x <- x[order(x$distance), ]
        x[x$distance == min(x$distance), 4] <- TRUE
        return(x[1:min(c(nrow(x), max_pairs)), ])
    }
    R_list <- lapply(split(R, tt_row_fil), aux.list)
    R <- do.call(rbind, R_list)
    rownames(R) <- NULL
    return(R)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
