\name{posteriorProbabilities}
\alias{posteriorProbabilities}

\title{
  posterior probabilities
}
\description{
  Calculates the probabilities after first state
}
\usage{
posteriorProbabilities(object, times, 
  stateNames = paste("State", as.list(1:dim(cohorts)[1])))
}

\arguments{
  \item{object}{
  either the output of \code{\link{simulateCohort}} or the \code{matrix} with the \code{probabilities} slot of that output.
}
  \item{times}{
  a time vector.
}
  \item{stateNames}{
  a list with the names of states.
}
}

\value{
  an object of class \code{"PosteriorProbabilities"}, containing the statenames, timepoints and the posterior probabilities over time.
}

\author{
  Luisa Salazar Vizcaya, Nello Blaser, Thomas Gsponer
}

\seealso{
  \code{\link{PosteriorProbabilities-class}},
  \code{\link{ArtCohort-class}},
  \code{\link{simulateCohort}}    
}

\keyword{utilities}