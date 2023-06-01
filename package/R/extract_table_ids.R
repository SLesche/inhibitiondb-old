extract_table_ids <- function(conn, table){
  which_columns_exist_query = paste0("PRAGMA table_info(", table, ")")
  table_columns = DBI::dbGetQuery(conn, which_columns_exist_query)
  ends_in_id =  table_columns$name[grep("_id$", table_columns$name)]

  sql_statement = paste0("SELECT ", paste(ends_in_id, collapse = ", "), " FROM ", table)

  ids = DBI::dbGetQuery(conn, sql_statement)
  return(ids)
}
