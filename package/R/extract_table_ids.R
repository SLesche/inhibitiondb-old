#' Extract IDs from a database table
#'
#' This function extracts the IDs from a specified database table.
#'
#' @param conn The connection object or database connection string.
#' @param table The name of the database table from which to extract IDs.
#'
#' @return A data frame or tibble containing the extracted IDs.
#' @examples
#' # Extract IDs from a table
#' my_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbExecute(my_conn, "CREATE TABLE my_table (id INTEGER PRIMARY KEY, name TEXT)")
#' DBI::dbExecute(my_conn, "INSERT INTO my_table (name) VALUES ('John'), ('Jane')")
#' ids <- extract_table_ids(conn = my_conn, table = "my_table")
#' ids
extract_table_ids <- function(conn, table) {
  which_columns_exist_query <- paste0("PRAGMA table_info(", table, ")")
  table_columns <- DBI::dbGetQuery(conn, which_columns_exist_query)
  ends_in_id <- table_columns$name[grep("_id$", table_columns$name)]

  sql_statement <- paste0("SELECT ", paste(ends_in_id, collapse = ", "), " FROM ", table)
  # TODO: Add functionality to only return IDs based on some previous info
  # This is especially relevant in the 'observation_table'
  ids <- DBI::dbGetQuery(conn, sql_statement)
  return(ids)
}
