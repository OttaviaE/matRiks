% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concatenation.R
\name{cof}
\alias{cof}
\alias{cof.figure}
\alias{cof.character}
\alias{com}
\alias{com.matriks}
\alias{concatenation}
\alias{concatenation.list}
\alias{concatenation.double}
\alias{cof.double}
\alias{cof.numeric}
\alias{concatenation.character}
\alias{concatenation.integer}
\title{Concatenation of figures (method)}
\usage{
cof(..., name, single)

\method{cof}{figure}(..., name = NULL, single = FALSE)

\method{cof}{character}(...)

com(...)

\method{com}{matriks}(...)

concatenation(...)

\method{concatenation}{list}(...)

\method{concatenation}{double}(...)

\method{cof}{double}(...)

\method{cof}{numeric}(...)

\method{concatenation}{character}(...)

\method{concatenation}{integer}(...)
}
\arguments{
\item{...}{The to be concatenated}

\item{name}{character, name of the figure created with cof()}

\item{single}{logical, force the figure to be a single figure to be used in shape(). Default is FALSE}
}
\value{
An object of class figure

An object of class figure

A concatenation of character

An object of class matriks resulting from the hierarchical concatenation of the original matrices

An object of class matriks resulting from the hierarchical concatenation of the original matrices
}
\description{
Concatenation of different figures to create a new figure
}
\section{Methods (by class)}{
\itemize{
\item \code{cof(figure)}: Concatenation of figures (figures)

Concatenation of different figures to create a new figure

\item \code{cof(character)}: Concatenation of character

Concatenation of different figures to create a new figure

\item \code{cof(double)}: Concatenation of double

\item \code{cof(numeric)}: Concatenation of numeric

}}
\section{Functions}{
\itemize{
\item \code{com()}: Concatenation of matrices (Method)

Hierarchical concatenation of 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.

\item \code{com(matriks)}: Concatenation of matrices

Hierarchical concatenation of 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.

\item \code{concatenation()}: Concatenation (Method)

\item \code{concatenation(list)}: Concatenation of lists

\item \code{concatenation(double)}: Concatenation of double

\item \code{concatenation(character)}: Concatenation of characters

\item \code{concatenation(integer)}: Concatenation of stuff

}}
\examples{

# concatenate figures without creating a new figure
new_figure <- cof(square(), size(malta(), 2))
# structure of new_figure
str(new_figure)
# concatenate figures and create a new figure
my_figure <- cof(square(), size(malta(), 2),
                  single = TRUE,
                   name = "my_figure")
# structure of new_figure
 str(my_figure)
# concatenate figures without creating a new figure
new_figure <- cof(square(), size(malta(), 2))
# structure of new_figure
str(new_figure)
# concatenate figures and create a new figure
my_figure <- cof(square(), size(malta(), 2),
                  single = TRUE,
                   name = "my_figure")
# structure of new_figure
 str(my_figure)
# concatenate figures without creating a new figure
new_figure <- cof(square(), size(malta(), 2))
# structure of new_figure
str(new_figure)
# concatenate figures and create a new figure
my_figure <- cof(square(), size(malta(), 2),
                  single = TRUE,
                   name = "my_figure")
# structure of new_figure
 str(my_figure)
# create the first layer matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# create the second matrix
m2 <- mat_apply(size(malta(), 2), vrules = "shade")
# concatenate the matrices
the_mat <- com(m1, m2)
# draw the final matrix
draw(the_mat)
# create the first layer matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# create the second matrix
m2 <- mat_apply(size(malta(), 2), vrules = "shade")
# concatenate the matrices
the_mat <- com(m1, m2)
# draw the final matrix
draw(the_mat)
# concatenate two characters
concatenation("a", "b")
# create some lists
a <- list(letters[c(14,13)], LETTERS[c(4, 3)])
b <- list(letters[c(12, 13)], LETTERS[c(4, 3)])
concatenation(a, b)
# create the first layer matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# create the second matrix
m2 <- mat_apply(size(malta(), 2), vrules = "shade")
# concatenate the matrices
the_mat <- com(m1, m2)
# draw the final matrix
draw(the_mat)
# create the first layer matrix
m1 <- mat_apply(hexagon(), hrules = "lty")
# create the second matrix
m2 <- mat_apply(size(malta(), 2), vrules = "shade")
# concatenate the matrices
the_mat <- com(m1, m2)
# draw the final matrix
draw(the_mat)
# concatenate two numeric
cof(rnorm(1, 25), rnorm(4, 34))
# concatenate two numeric
cof("a", "b", "d")
# concatenate two numeric
cof(1:3, 22:20)
}
