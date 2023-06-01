return_entries_from_id <- function(conn, matches){
  tables = names(matches)

  data = list(
    publication_table = data.frame(),
    study_table = data.frame(),
    task_table = data.frame(),
    between_table = data.frame(),
    within_table = data.frame(),
    condition_table = data.frame(),
    dataset_table = data.frame(),
    observation_table = data.frame()
  )

  for (table in tables){
    id_name = return_id_name_from_table(table)

    query = write_id_query(table, id_name, matches[[table]])

    data[[table]] = DBI::dbGetQuery(conn, query)
  }

  return(data)
}
