#' Function to pull data from database using paramterized DBI odbc query
#' @param queryString string paramterized sql query where parameters are indicated by "?"
#' @param parameters tibble of parameter values
#' @param convertFactorsToStrings boolean whether to convert all factor variables to character strings default = TRUE
#' @param ... conn_args list of database connection arguments: driver, server, database name, userid, password, port
#' @return returns sql query result as a dataframe
#' @export
getDataframeFromDatabase <- function(queryString,parameters,convertFactorsToStrings = TRUE,...) {

  if(!is.null(parameters)){
    parameters |>
      dplyr::mutate_if(is.factor, as.character) -> parameters
  }

  dbhandle <- dbConnector(...)

  query <- DBI::dbSendQuery(dbhandle,queryString)

  DBI::dbBind(query, parameters)

  data <- DBI::dbFetch(query)

  DBI::dbClearResult(query)

  if(convertFactorsToStrings){
    data <- data |>
      dplyr::mutate_if(is.factor, as.character) -> data
  }

  return(data)

}

#' Function to create DBI odbc database connection object
#' @param conn_args list of database connection arguments: driver, server, database name, userid, password, port
#' @return returns DBI odbc database connection object
dbConnector <- function(conn_args) {

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = conn_args$driver,
                        Server   = conn_args$server,
                        Database = conn_args$database,
                        UID      = conn_args$uid,
                        PWD      = conn_args$pwd,
                        Port     = conn_args$port
  )

  return(con)

}
