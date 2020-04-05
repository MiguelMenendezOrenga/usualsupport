\name{plotErrorSample}
\alias{plotErrorSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Error-Sample Size graph
}
\description{
Plots a Error-Sample Size graph from another object (a list) containing a vector of measurement error and its related vector of estimated sample sizes
}
\usage{
plotErrorSample(x, point = TRUE, ...)
}

\arguments{
  \item{x}{a list, usually produced with other sample size estimation formulas as CochranCont(), which contains at least:

  x$Evect, a vector of possible measurement error values

  x$nvect, a vector of estimated sample sizes for each value of x$Evect
}
  \item{point}{logical, if set to TRUE a point will be marked at x$Error, x$n, which is the sample size estimated for this Error value
}
  \item{printlegend}{logical, if set to TRUE a legend will be print with values used to compute sample size: alfa, and sigma (if a continuous variable) or p/q (if a discrete one). If no sigma nor p/q could be found, a warning will be shown
}
  \item{...}{other arguments passed from plot(), like xlim(), ylim(), etc.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An Error-Sample Size plot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Miguel Menéndez
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
size1 <- CochranCont(alfa = 0.05, sigma = 15, E = 2, N = 2000, correct = TRUE)
plotErrorSample(size1, xlim = c(0,6), ylim = c(0,200),
                xlab = "Error", ylab = "Sample Size")

size2 <- CochranDis(alfa = 0.05, p = 0.20, E = 0.05)
plotErrorSample(size2, point = TRUE,
                xlab = "Error (en tanto por uno)", ylab = "Tamaño muestral")
}
