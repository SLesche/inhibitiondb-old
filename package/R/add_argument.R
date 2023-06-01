add_argument <- function(list, conn, variable, operator, values, manual = FALSE, statement = NULL){
  if (manual == FALSE){
    list[[length(list) + 1]] = make_valid_sql(conn, variable, operator, values)
  } else {
    if (is.null(statement)){
      stop("When using manual argument select, specify a statement variable")
    } else {
      list[[length(list) + 1]] = statement
    }
  }
  return(list)
}
