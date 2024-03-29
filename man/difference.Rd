% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/difference.R
\name{difference}
\alias{difference}
\alias{difference.matriks}
\title{Difference distractor (Method)}
\usage{
difference(obj, seed, ...)

\method{difference}{matriks}(obj, seed = 666, ...)
}
\arguments{
\item{obj}{matriks, The matriks for which the distractor is generated}

\item{seed}{seed}

\item{...}{other arguments}
}
\value{
An object of class figure that is the difference distractor of a matrix

An object of class figure that is the difference distractor of a matrix
}
\description{
Generate difference distractor from a matriks
}
\section{Methods (by class)}{
\itemize{
\item \code{difference(matriks)}: Difference distractors

}}
\examples{
# create a matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# draw the matrix
draw(m1)
# draw the difference distractor
draw(difference(m1))
# create a matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# draw the matrix
draw(m1)
# draw the difference distractor
draw(difference(m1))
}
