% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/library.R
\name{disconnect}
\alias{disconnect}
\title{Disconnects any existing connection saved globally.}
\usage{
disconnect(warn = TRUE, ...)
}
\arguments{
\item{warn}{Indicates if warning messages to be raised.}

\item{...}{Additional parameters to be passed to
\code{DBI::dbDisconnect} function.}
}
\value{
The status of the disconnect operation.
}
\description{
Disconnects any existing connection saved globally.
}

