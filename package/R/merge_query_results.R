merge_query_results <- function(query_results){
  # join the proper tables, return data frame
  data_raw = query_results$observation_table

  data_joined = data_raw %>%
    dplyr::left_join(., query_results$between_table) %>%
    dplyr::left_join(., query_results$within_table) %>%
    dplyr::left_join(., query_results$condition_table) %>%
    dplyr::left_join(., query_results$dataset_table) %>%
    dplyr::rename(comment_dataset = comment) %>%
    dplyr::left_join(., query_results$study_table, by = "study_id") %>%
    dplyr::rename(comment_study = comment) %>%
    dplyr::left_join(., query_results$publication_table, by = "publication_id") %>%
    dplyr::left_join(., query_results$task_table, by = "task_id") %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::everything())

  return(data_joined)
  # Maybe add functionality to compress this here
}
