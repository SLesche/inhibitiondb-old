check_operator <- function(operator, values){
  valid_operators = c("less", "greater", "between", "equal")
  if (!operator %in% valid_operators){
    msg = paste0("Operator can only take the following values: ", valid_operators)
    stop(msg)
  }

  # # Commented out because of 'equal' operator functionalities
  # # TODO: Replace later
  # if (length(values) > 2){
  #   msg = "Please only provide a maximum of 2 values"
  #   stop(msg)
  # }
  #
  # if (length(values) == 2 & (values[1] >= values[2])){
  #   msg = "When providing two values, the first must be smaller than the second"
  #   stop(msg)
  # }

  if (operator == "between" & length(values) != 2){
    msg = "When using the 'between' operator, please provide two values"
    stop(msg)
  }

  if (operator != "between" & operator != "equal" & length(values) != 1){
    msg = "When suing 'less', 'greater' or 'equal', provide only one value"
    stop(msg)
  }

}
