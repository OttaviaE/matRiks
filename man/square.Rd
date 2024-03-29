% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/closed_figures.R, R/lines.R
\name{square}
\alias{square}
\alias{square4}
\title{Coordinates of a square}
\usage{
square(
  size.x = 15,
  size.y = size.x,
  rot = pi/4,
  pos.x = 0,
  pos.y = 0,
  shd = NA,
  vis = 1,
  lty = 1,
  lwd = 3
)

square4(
  size.x = sqrt(square()$size.x[[1]]^2/2),
  size.y = size.x,
  pos.x = size.x,
  pos.y = size.x,
  lwd = 3,
  lty = 1
)
}
\arguments{
\item{size.x}{numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)}

\item{size.y}{numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.}

\item{rot}{define the rotation. Default is \eqn{\frac{pi}{4}}}

\item{pos.x}{numeric, position on the x axis. Default is 0.}

\item{pos.y}{numeric, position the y axis, Default is 0.}

\item{shd}{character, define the shading of the figure. Default is NA which results in a transparent figure}

\item{vis}{Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0}

\item{lty}{integer, define the line type of the figure, default is 1 (solid line).}

\item{lwd}{integer, define the line width of the figure. Default is 3.}
}
\value{
Return the coordinates for drawing a square

Return the coordinates for drawing a square composed of 4 lines
}
\description{
Define the coordinates for drawing a square
}
\section{Functions}{
\itemize{
\item \code{square4()}: Coordinates of a square composed of 4 lines

Define the coordinates for drawing a square composed of 4 lines

}}
\examples{
# return the default coordinates for drawing a square
square()
# change the coordinates for drawing a smaller square
square(size.x = 5)
# default coordinates of square composed of 4 lines
square4()
# draw square composed of 4 lines with different lty
draw(square4(lty = 2))
}
