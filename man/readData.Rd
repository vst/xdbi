% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/library.R
\name{readData}
\alias{readData}
\title{Reads table contents from database using a dynamic SQL statement.}
\usage{
readData(sql, ...)
}
\arguments{
\item{sql}{The SQL statement.}

\item{...}{Additional parameters as \code{sprintf} function
arguments along with the \code{sql} parameter.}
}
\value{
The resulting table from the SQL query.
}
\description{
Reads table contents from database using a dynamic SQL statement.
}

