find_relevant_tables <- function(conn, column_name, info = NULL){
  # TODO: maybe save an info-data and use that, check speed of get_column_names
  if (is.null(info)){
    info = get_column_names(conn)
  }

  tables = info[info$column == column_name, "table"]

  if (length(tables) == 0){
    stop("This column does not exist in any table in this db")
  }

  return(tables)
}
