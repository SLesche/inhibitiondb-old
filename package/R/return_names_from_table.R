return_id_name_from_table <- function(table_name){
  name = stringr::str_replace(table_name, "table$", "id")
  return(name)
}

return_table_name_from_id <- function(id_name){
  name = stringr::str_replace(id_name, "id$", "table")
  return(name)
}
