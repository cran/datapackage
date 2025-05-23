% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dp_resource.R
\name{dp_resource}
\alias{dp_resource}
\alias{dp_resource.datapackage}
\alias{dp_resource<-}
\alias{dp_resource<-.readonlydatapackage}
\alias{dp_resource<-.editabledatapackage}
\title{Modifying the resources of a Data Package}
\usage{
dp_resource(x, resource_name)

\method{dp_resource}{datapackage}(x, resource_name)

dp_resource(x, resource_name) <- value

\method{dp_resource}{readonlydatapackage}(x, resource_name) <- value

\method{dp_resource}{editabledatapackage}(x, resource_name) <- value
}
\arguments{
\item{x}{a \code{datapackage} object.}

\item{resource_name}{the name of a resource.}

\item{value}{a \code{dataresource} object.}
}
\value{
Either returns a Data Resource object or modifies the Data Package.
}
\description{
Modifying the resources of a Data Package
}
\details{
When a resource with the name already exists this resource is
overwritten. Therefore, the assignment operator can also be used to modify
existing resources.
}
