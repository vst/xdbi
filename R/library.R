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


##' Constructs a where clause.
##'
##' @param tuples key/value pairs for equality test.
##' @return The where clause
.clauseWhere <- function (tuples) {
    paste(lapply(names(tuples), function (key) {sprintf("`%s` = %s", key, .escapeValues(tuples[[key]]))}), collapse=" AND ")
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
##' @param .table The database table name.
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


##' Insert a given set of values as rows.
##'
##' @param .table The name of the database table.
##' @param ... Values
##' @return Nothing special.
##'
##' @export
insertRow <- function (.table, ...) {
    ## Get arguments:
    args <- list(...)

    ## Write the table and return:
    writeData(.table, as.data.frame(args))
}


##' Update a row
##'
##' @param .table The name of the table.
##' @param .where The where clause.
##' @param ... Values to be updated.
##' @return Nothing special.
##'
##' @export
updateRow <- function (.table, .where, ...) {
    ## Get arguments and make sure that we get only the first value:s
    args <- lapply(list(...), function (x) {x[1]})

    ## Construct the where clause:
    where <- .clauseWhere(.where)

    ## Create the update statement template:
    template <- "UPDATE `%s` SET %s WHERE %s"

    ## Prepare the vals.
    sets <- paste(lapply(names(args), function (key) {sprintf("`%s` = %s", key, .escapeValues(args[[key]]))}), collapse=", ")

    ## Create the statement:
    sql <- sprintf(template, .table, sets, where)

    ## Execute the statement:
    DBI::dbSendQuery(getConnection(), sql)
}


##' Selects rows.
##'
##' @param .table The name of the database table.
##' @param ... Where clause to be \code{AND}ed.
##' @param .ordering Ordering if any.
##' @return Result.
##'
##' @import DBI
##' @export
selectRow <- function (.table, ..., .ordering=NULL) {
    ## Construct the where clause:
    where <- .clauseWhere(list(...))

    ## Ordering:
    ordering <- ifelse (is.null(.ordering), "", paste(" ORDER BY ", paste(.ordering, collapse=", ")))

    ## Create the update statement template:
    template <- "SELECT * FROM `%s` WHERE %s%s"

    ## Create the statement:
    sql <- sprintf(template, .table, where, ordering)

    ## Send the query and obtain a result set:
    resultSet <- DBI::dbSendQuery(getConnection(), sql)

    ## Fetch all rows:
    result <- DBI::dbFetch(resultSet, n=-1)

    ## Clear the resultset:
    DBI::dbClearResult(resultSet)

    ## Done, return the data:
    result
}


##' Checks if any rows exist.
##'
##' @param .table The name of the database table.
##' @param ... Where clause to be \code{AND}ed.
##' @return Boolean indicating if any rows exist.
##'
##' @import DBI
##' @export
existsRow <- function (.table, ...) {
    ## Construct the where clause:
    where <- .clauseWhere(list(...))

    ## Create the update statement template:
    template <- "SELECT count(*) as `__count` FROM `%s` WHERE %s"

    ## Create the statement:
    sql <- sprintf(template, .table, where)

    ## Send the query and obtain a result set:
    resultSet <- DBI::dbSendQuery(getConnection(), sql)

    ## Fetch all rows:
    result <- DBI::dbFetch(resultSet, n=-1)

    ## Clear the resultset:
    DBI::dbClearResult(resultSet)

    ## Done, return the data:
    result[["__count"]] > 0
}


##' Upserts row.
##'
##' @param .table The name of the table.
##' @param .where The where clause.
##' @param ... Values to be updated.
##' @return Nothing special.
##'
##' @export
upsertRow <- function (.table, .where, ...) {
    ## Check if the tow exists:
    if (do.call(existsRow, c(list(.table=.table), .where))) {
        do.call(updateRow, c(list(.table=.table, .where=.where), list(...)))
    }
    else {
        do.call(insertRow, c(list(.table=.table), .where, list(...)))
    }
}
