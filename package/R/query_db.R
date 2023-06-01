query_db <- function(conn, arguments, target_level = "data", argument_relation = "and"){
  # Querying starts
  column_names = get_column_names(conn)
  argument_matches = list()

  argument_sequence = get_argument_sequence(arguments, argument_relation)

  for (i in seq_along(arguments)){
    # Loop through arguments, find matching primary keys
    # Then find keys connected to these matches
    # If its the first iteration (first argument), then the init ids is the matches
    # Otherwise it is only the matches that are already present in the arguments
    if (i == 1){
      # Find out which table is queried from
      table = which_table_is_queried(arguments[[i]])

      # Obtain the ids that match that query
      matching_ids = DBI::dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]

      # Return ids connected to this into the appropriate sublist in argument_matches
      argument_matches[[i]] = return_connected_ids(conn, table, matching_ids)

    } else {
      if (argument_sequence[i] != argument_sequence[i - 1]){
        # then it is an "and" connector, you should have starting ids
        table = which_table_is_queried(arguments[[i]])

        init_ids = argument_matches[[i - 1]][[table]]

        matching_ids = DBI::dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]

        relevant_ids = matching_ids[matching_ids %in% init_ids]

        argument_matches[[i]] = return_connected_ids(conn, table, relevant_ids)

      } else {
        # then it is an "or" connector, you should return all ids
        table = which_table_is_queried(arguments[[i]])

        matching_ids = DBI::dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]

        argument_matches[[i]] = return_connected_ids(conn, table, matching_ids)
      }
    }
  }

  # Now use the list of argument matches to return a proper list depending on structure
  proper_matches = return_proper_ids(argument_matches, argument_sequence)
  # Then use that list to query the db

  read_data = return_entries_from_id(conn, proper_matches)

  return(read_data)
}
