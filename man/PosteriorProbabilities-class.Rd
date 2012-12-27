\name{PosteriorProbabilities-class}
\Rdversion{1.1}
\docType{class}
\alias{PosteriorProbabilities-class}
\alias{[,PosteriorProbabilities-method}
\alias{plot,PosteriorProbabilities-method}

\title{Class \code{"PosteriorProbabilities"}}
\description{
  This S4 class summarizes the posterior probabilities over time for objects of class \code{"ArtCohort"}
}
\section{Objects from the Class}{
  Objects are created by calls to the function \code{simulateCohort}.
}
\section{Slots}{
  \describe{
    \item{\code{states}:}{Object of class \code{"character"}: names of states }
    \item{\code{times}:}{Object of class \code{"numeric"}: time points at which probabilities are evaluated }
    \item{\code{probabilities}:}{Object of class \code{"matrix"}: posterior Probabilities to be in each state at each time }
    \item{\code{lower}:}{Object of class \code{"matrix"}: lower prediction bound to be in each state at each time }
    \item{\code{upper}:}{Object of class \code{"matrix"}: upper prediction bound to be in each state at each time }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "PosteriorProbabilities")}: accesses the probabilities slot }
    \item{plot}{\code{signature(x = "PosteriorProbabilities")}: plots the posterior probabilities }
	 }
}

\author{
  Luisa Salazar Vizcaya, Nello Blaser, Thomas Gsponer
}

\seealso{
  \code{\link{posteriorProbabilities}}, \code{\link{ArtCohort-class}},
  
}
\examples{
showClass("PosteriorProbabilities")
}
\keyword{classes}
