\name{CochranCont}
\alias{CochranCont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cochran's Sample Size for continuous variables
}
\description{
Sample size for the mean of a continuous variable, according to Cochran's formula
}
\usage{
CochranCont(alfa = 0.05, sigma, E, N = Inf, correct = FALSE)
}

\arguments{
  \item{alfa}{numeric, alfa error to use
}
  \item{sigma}{numeric, standard deviation to use
}
  \item{E}{numeric, Measurement Error
}
  \item{N}{numeric, Size of the study population
}
  \item{correct}{logical, if set to TRUE, it will check if the internally computed sample size is 5 per cent or bigger than the whole population, if this is the case, it will correct the sample size according to the formula n / (1 + n/N)
}
}
\details{
n <- Zalpha^2 * sigma^2 / E^2
}


\value{
A list containing:
  \item{n}{numeric value with the sample size estimated for Measurement Error choosen. This value, rounded, is the only printed output}
  \item{Error}{numeric value of Measurement Error as it was set with 'E' argument}
  \item{alfa}{numeric value of alfa error as it was set with 'alfa' argument}
  \item{Evect}{vector of numeric values, with different alternative values of Measurement Error}
  \item{nvector}{vector of numeric values, with different alternative values of sample size estimated for the Evect values}

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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
