make_valid_sql <- function(conn, variable, operator, values){
  # First find out which table the variable is from
  table = find_relevant_tables(conn, variable)
  id_name = return_id_name_from_table(table)

  # Then turn this into a statement
  # Check validity of operator
  check_operator(operator, values)

  # if values is a char, surround with ''
  if (all(is.character(values))){
    values = paste0("'", values, "'")
  }

  if (operator == "greater"){
    sql_statement = paste(
      "SELECT",
      id_name,
      "FROM",
      table,
      " ",
      "WHERE",
      variable,
      ">",
      values[1]
    )
  }

  if (operator == "less"){
    sql_statement = paste(
      "SELECT",
      id_name,
      "FROM",
      table,
      "WHERE",
      variable,
      "<",
      values[1]
    )
  }

  if (operator == "equal"){
    equal_statement = ""
    for (i in seq_along(values)){
      added_statement = paste(
        variable,
        "=",
        values[i]
      )
      if (i == 1){
        equal_statement = added_statement
      } else {
        equal_statement = paste(equal_statement, "OR", added_statement)
      }
    }
    sql_statement = paste(
      "SELECT",
      id_name,
      "FROM",
      table,
      "WHERE",
      equal_statement
    )
  }

  if (operator == "between"){
    sql_statement = paste(
      "SELECT",
      id_name,
      "FROM",
      table,
      "WHERE",
      variable,
      ">",
      values[1],
      "AND",
      variable,
      "<",
      values[2]
    )
  }
  return(sql_statement)
}
