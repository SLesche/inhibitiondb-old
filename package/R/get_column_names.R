get_column_names <- function(conn){
  tables = DBI::dbListTables(conn)
  tables = tables[tables != "sqlite_sequence"]
  column_names = data.frame(
    column = c(),
    table = c()
  )
  for (table in tables){
    columns = DBI::dbListFields(conn, table)
    for (column in columns){
      add = data.frame(
        column = column,
        table = table
      )
      column_names = rbind(column_names, add)
    }
  }
  return(column_names)
}
