% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/field_string.R
\name{dp_to_string}
\alias{dp_to_string}
\title{Convert a vector to 'string' using the specified fielddescriptor}
\usage{
dp_to_string(x, fielddescriptor = list(), ...)
}
\arguments{
\item{x}{the vector to convert.}

\item{fielddescriptor}{the field descriptor for the field.}

\item{...}{passed on to other methods.}
}
\value{
Will return an \code{character} vector with \code{fielddescriptor} added as
the 'fielddescriptor' attribute.
}
\description{
Convert a vector to 'string' using the specified fielddescriptor
}
\details{
When \code{fielddescriptor} is missing a default field descriptor is
generated.
}
