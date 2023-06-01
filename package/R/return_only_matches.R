return_only_matches <- function(data1, data2, id = NULL, full = FALSE){
  if (!is.null(id)){
    matches = dplyr::inner_join(data1, data2) %>%
      dplyr::select({{id}})

    return(matches)
  }

  common_cols = intersect(
    colnames(data1),
    colnames(data2)
  )

  if (full == FALSE){
    matches = dplyr::inner_join(data1, data2) %>%
      dplyr::select({{common_cols}})
  } else {
    matches = dplyr::inner_join(data1, data2)
  }


  return(matches)
}
