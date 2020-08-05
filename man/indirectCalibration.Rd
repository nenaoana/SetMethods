\name{indirectCalibration}
\alias{indirectCalibration}
\title{Function performing the indirect calibration}
\description{indirectCalibration is a function for the indirect calibration procedure as described by Ragin (2008). It uses a binomial or a beta regression for tranforming raw scores into calibrated scores. In our opinion, using a fractional polynomial may not be appropriate to this case. In fact, we do not deal with proportions. This function requires the package \code{betareg}.}
\usage{indirectCalibration(x, x_cal, binom = TRUE)}
\arguments{
  \item{x}{vector of raw scores.}
  \item{x_cal}{vector of theoretically calibrated scores.}
  \item{binom}{logical. If indirect calibration has to be performed using binomial regression or beta regression.  The default is \code{TRUE}, which means that binomial regression is used.}
}
%\details{}
\value{It returns a vector of indirectly calibrated values.}
\references{Ragin, C. C. (2008) Redesigning Social Inquiry: Fuzzy Sets and Beyond, The Chicago University Press: Chicago and London.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.
}
\author{Mario Quaranta}
%\note{}
%\seealso{}
\examples{

# Generate fake data
set.seed(4)
x <- runif(20, 0, 1)

# Find quantiles
quant <- quantile(x, c(.2, .4, .5, .6, .8))

# Theoretical calibration
x_cal <- NA  
x_cal[x <= quant[1]] <- 0
x_cal[x > quant[1] & x <= quant[2]] <- .2
x_cal[x > quant[2] & x <= quant[3]] <- .4
x_cal[x > quant[3] & x <= quant[4]] <- .6
x_cal[x > quant[4] & x <= quant[5]] <- .8
x_cal[x > quant[5]] <- 1
x_cal

# Indirect calibration (binomial)
a <- indirectCalibration(x, x_cal, binom = TRUE)

# Indirect calibration (beta regression)
b <- indirectCalibration(x, x_cal, binom = FALSE)

# Correlation
cor(a, b)

# Plot
plot(x, a); points(x, b, col = "red")
}
