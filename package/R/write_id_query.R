write_id_query <- function(table_name, variable_name, id_vector) {
  # Convert the ID vector to a comma-separated string
  id_str = paste0(id_vector, collapse = ",")

  # Construct the SQL query
  sql_query = sprintf("SELECT * FROM %s WHERE %s IN (%s)", table_name, variable_name, id_str)

  # Return the results
  return(sql_query)
}
