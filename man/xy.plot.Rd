\name{xy.plot}
\alias{xy.plot}
\title{Function producing enhanced XY plots}
\description{
xy.plot produces XY plots and provides values for consistency, Haesebrouck's consistency, coverage, RoN, PRI. Several graphic parameters can be decided by the user.
}
\usage{xy.plot(x, y, data, 
           labcol = "black",
           main = "XY plot", 
           ylab = "Outcome", 
           xlab = "Condition",
           necessity = FALSE,
           jitter = FALSE, 
           font = "sans",
           fontface = "italic", 
           fontsize = 3,
           labs = rownames(data),
           crisp = FALSE,
           shape = 19,
           consH = FALSE,
           ...)
}
\arguments{
  \item{x}{vector containing the condition.}
  \item{y}{vector containing the outcome.}
  \item{data}{The dataset used}
  \item{labcol}{color of the dots.}
  \item{main}{an overall title for the plot. The default is \code{"XY plot"}. See ?title.}
  \item{ylab}{a title for the y-axis. The default is \code{"Outcome"}. See ?title.}
  \item{xlab}{a title for the x-axis. The default is \code{"Condition"}. See ?title.}
  \item{necessity}{logical. Indicates if the parameters of fit are calculated for a sufficient or necessary condition. The default is \code{FALSE}, therefore it calculates the parameters of fit for sufficiency. To get the parameters of fit for necessary conditions set \code{necessity} as \code{TRUE}.}
  \item{jitter}{
		Logical. Should labels be jitter to not overlap?
	}
		\item{font}{
		Font of the labels. Accepts "sans", "serif", and "mono" fonts.
	}
	\item{fontface}{
		Fontface of the labels. Accepts "plain", "bold", "italic", "bold.italic".
	}
	\item{fontsize}{
		Fontsize of the labels.
	}
	\item{labs}{the vector of case labels. The default is the rownames of the dataset.}
	\item{crisp}{Logical. Should a two-by-two table for crisp sets be returned?}
	\item{shape}{The shape for the markers.}
	\item{consH}{Logical. Should Haesebrouck's consistency be printed?}
	\item{...}{Other internal arguments. Do not specify!}
}
%\details{}
\value{It returns an enhanced XY plot.}
\references{

Haesebrouck, T. (2015) Pitfalls in QCA's consistency measure. Journal of Comparative Politics 2:65-80.

Ragin, C. C. (2008) Redesigning Social Inquiry: Fuzzy Sets and Beyond. The Chicago University Press: Chicago and London.

Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge Univeristy Press: Cambridge.

}
\author{Mario Quaranta and Ioana-Elena Oana.}
%\note{}
%\seealso{}
\examples{

# Load the Schneider data:

data(SCHF)

# Plot of condition EMP as necessary for outcome EXPORT with case labels 
# and names for the plot and axes:

xy.plot("EMP", "EXPORT", data=SCHF, necessity = TRUE,
            main = "EMP as necessary for EXPORT", ylab = "EXPORT", xlab = "EMP")
}
