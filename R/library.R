##' Provides an auxiliary method to check if the provided connection
##' is valid or not.
##'
##' @param connection The connection to be checked for validity.
##' @param ... Additional parameters to be passed to
##'     \code{DBI::dbIsValid} function.
##' @return Boolean indicating validity.
.isConnectionValid <- function (connection, ...) {
    DBI::dbIsValid(connection, ...)
}


##' Provides an auxiliary method to escape values for insert command.
##'
##' @param x A vector of values.
##' @return A vector of escaped values.
##'
##' @import stringr
.escapeValues <- function (x) {
    ifelse(is.na(x), "NULL", sprintf("'%s'", stringr::str_replace_all(x, "'", "\'")))
}


##' Connects to an RDBMS.
##'
##' Note that this function returns available valid connection if
##' there exists any instead of creating one. If the connection does
##' not exist, a new one is created and saved globally in
##' \code{options} before being returned.
##'
##' @param drv The \code{DBI} driver to use.
##' @param ... Additional parameters to pass to the underlying
##'     \code{DBI::dbConnect}
##' @return \code{DBI} connection.
##'
##' @examples
##' ## Establish a connection:
##' connection <- connect(RSQLite::SQLite(), ":memory:")
##'
##' ## The connection is the same as the one saved globally:
##' identical(connection, getConnection())
##'
##' ## Disconnect:
##' disconnect()
##'
##' @import DBI
##' @export
connect <- function (drv, ...) {
    ## First, attempt to get any valid connections:
    connection <- try(getConnection(validate=TRUE), silent=TRUE)

    ## Is the connection valid?
    if (!inherits(connection, "try-error") && .isConnectionValid(connection)) {
        ## OK, we simply return this connection without attempting to
        ## create a new one:
        return(connection)
    }

    ## Apparently, we don't have any valid connections. To ensure to
    ## close any teardown on invalid connections, call disconnect:
    disconnect(warn=FALSE)

    ## Establish a connection:
    connection <- DBI::dbConnect(drv, ...)

    ## Save the connection in options:
    options(xdbi_connection=connection)

    ## Done, return the new connection:
    connection
}


##' Disconnects any existing connection saved globally.
##'
##' @param warn Indicates if warning messages to be raised.
##' @param ... Additional parameters to be passed to
##'     \code{DBI::dbDisconnect} function.
##' @return The status of the disconnect operation.
##'
##' @import DBI
##' @export
disconnect <- function (warn=TRUE, ...) {
    ## Get the connection:
    connection <- getConnection(validate=FALSE)

    ## Do we have a connection at all?
    if (is.null(connection)) {
        ## Check if we need to warn the user:
        if (warn) {
            ## Yep, warn the user:
            warning("No connection found to disconnect.")
        }

        ## Done, return with TRUE as there is nothing to disconnect:
        return(TRUE)
    }

    ## Disconnect:
    retval <- DBI::dbDisconnect(connection, ...)

    ## Clean options:
    options(xdbi_connection=NULL)

    ## Done, return the result of the disconnect operation:
    retval
}


##' Returns existing connection.
##'
##' If validation is enabled, the function will stop on invalid
##' connection encounter.
##'
##' @param validate Indicates if the connection should be validated.
##' @return The connection.
##'
##' @export
getConnection <- function (validate=TRUE) {
    ## Get the connection from options:
    connection <- getOption("xdbi_connection")

    ## Do we have a valid connection?
    if (validate && (is.null(connection) || !.isConnectionValid(connection))) {
        stop("No valid DBI connection found.")
    }

    ## Done, return the connection:
    connection
}


##' Reads a table from the database.
##'
##' @param .table The name of the database table.
##' @return The database table contents.
##'
##' @import DBI
##' @export
readTable <- function (.table) {
    DBI::dbReadTable(getConnection(), .table)
}


##' Creates and/or populates a database table.
##'
##' @param .table The name of the database table.
##' @param x The data to be populated. If contents are empty, it is
##'     used to introspect the database structure to create a table if
##'     it does not exist.
##' @param append If the values will be append. Otherwise will raise
##'     exception if the table is not new and \code{overwrite} is
##'     \code{FALSE}.
##' @param overwrite If the values will be overridden. Otherwise will
##'     raise exception if the table is not new and \code{append} is
##'     \code{FALSE}.
##' @param withRowNames Indicates if we are we inserting with row names.
##' @return The success indicator as a logical value.
##'
##' @import DBI
##' @export
writeTable <- function (.table, x, append=TRUE, overwrite=FALSE, withRowNames=FALSE) {
    DBI::dbWriteTable(getConnection(), .table, x, append=TRUE, row.names=withRowNames)
}


##' Reads table contents from database using a dynamic SQL statement.
##'
##' @param sql The SQL statement.
##' @param ... Additional parameters as \code{sprintf} function
##'     arguments along with the \code{sql} parameter.
##' @return The resulting table from the SQL query.
##'
##' @import DBI
##' @export
readData <- function (sql, ...) {
    ## Construct the statement:
    statement <- sprintf(sql, ...)

    ## Send the query and obtain a result set:
    resultSet <- DBI::dbSendQuery(getConnection(), statement)

    ## Fetch all rows:
    result <- DBI::dbFetch(resultSet, n=-1)

    ## Clear the resultset:
    DBI::dbClearResult(resultSet)

    ## Done, return the data:
    result
}


##' Write the data to the database table.
##'
##' This allows partial data insertion.
##'
##' @param name The database table name.
##' @param x The data
##' @return Nothing interesting
##'
##' @import DBI
##' @export
writeData <- function (.table, x) {
    ## Get the column names from the data frame as the fields:
    fields <- paste(sprintf("`%s`", colnames(x)), collapse=", ")

    ## Create the SQL insert statement template:
    sql <- sprintf("INSERT INTO %s (%s) VALUES %%s", .table, fields)

    ## Create the sql statement:
    statement <- sprintf(sql, paste(apply(x, MARGIN=1, FUN=function (row) {
        sprintf("(%s)", paste(.escapeValues(row), collapse=", "))
    }), collapse=", "))

    ## Run the statement:
    DBI::dbClearResult(DBI::dbSendQuery(getConnection(), statement))
}
