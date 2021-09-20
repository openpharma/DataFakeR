get_schema_constraints.RedshiftConnection <- function(source, schema) {
  constr_table <- DBI::dbGetQuery(source, glue::glue("
    SELECT tc.table_name, ccu.column_name, ccu.constraint_name, tc.constraint_type, cc.check_clause, kcu.unique_constraint_name
    FROM (SELECT * FROM information_schema.table_constraints WHERE table_schema = '{schema}') AS tc
    JOIN (SELECT * FROM information_schema.constraint_column_usage WHERE table_schema = '{schema}') AS ccu
    USING (constraint_name)
    FULL JOIN (SELECT * FROM information_schema.check_constraints WHERE constraint_schema = '{schema}') AS cc
    ON ccu.constraint_name = cc.constraint_name
    LEFT JOIN (SELECT * FROM information_schema.referential_constraints WHERE constraint_schema = '{schema}') AS kcu
    ON ccu.constraint_name = kcu.constraint_name;
  "))
  constraint_table <- dplyr::left_join(
    constr_table,
    constr_table %>% dplyr::select(unique_constraint_name, fk_table_name = table_name, fk_column_name = column_name),
    by = "unique_constraint_name",
    na_matches = "never"
  )
  if (nrow(constraint_table) == 0) {
    warning("No schema constraints pulled, make sure you have access rights to information_schema tables")
  }
  constraint_table
}

get_schema_constraints.PostgresConnection <- get_schema_constraints.RPostgreSQLConnection <- get_schema_constraints.RedshiftConnection

get_table_pk.RedshiftConnection <- function(source, schema, table_name) {
  # the way to source pk's without having permissions to information_schema
  sql_query <- glue::glue("
    SELECT a.attname column_name, format_type(a.atttypid, a.atttypmod) AS data_type
    FROM   pg_index i
    JOIN   pg_attribute a ON a.attrelid = i.indrelid
    AND a.attnum = ANY(string_to_array(textin(int2vectorout(i.indkey)), ' '))
    WHERE  i.indrelid = '{schema}.{table_name}'::regclass
    AND    i.indisprimary;
  ")
  DBI::dbGetQuery(source, sql_query)
}

get_table_pk.PostgresConnection <- get_table_pk.RPostgreSQLConnection <- get_table_pk.RedshiftConnection

pull_column_values.RedshiftConnection <- function(source, col_info, values, max_uniq_to_pull) {
  if (!identical(values, TRUE) || is.null(max_uniq_to_pull)) {
    return(NULL)
  }

  n_vals <- DBI::dbGetQuery(
    source, glue::glue("select count(distinct({col_info$column_name})) from {col_info$table_schema}.{col_info$table_name};")
  )[1, 1]
  if (n_vals > max_uniq_to_pull) {
    return(NULL)
  }
  result <- DBI::dbGetQuery(
    source, glue::glue("select distinct({col_info$column_name}) as vals from {col_info$table_schema}.{col_info$table_name};")
  ) %>% dplyr::pull(vals)

  result
}

pull_column_values.PostgresConnection <- pull_column_values.RPostgreSQLConnection <- pull_column_values.RedshiftConnection

pull_column_nchar.RedshiftConnection <- function(source, col_info, nchar) {
  if (!identical(nchar, TRUE)) {
    return(NULL)
  }

  nchar <- DBI::dbGetQuery(
    source, glue::glue("select max(char_length({col_info$column_name})) as nchars from {col_info$table_schema}.{col_info$table_name};")
  ) %>% dplyr::pull(nchars)

  nchar
}

pull_column_nchar.PostgresConnection <- pull_column_nchar.RPostgreSQLConnection <- pull_column_nchar.RedshiftConnection

pull_column_range.RedshiftConnection <- function(source, col_info, range) {

  if (!identical(range, TRUE)) {
    return(NULL)
  }

  result <- DBI::dbGetQuery(
    source, glue::glue(
      "select max({col_info$column_name}) as max_val, min({col_info$column_name}) as min_val from {col_info$table_schema}.{col_info$table_name};"
    )
  )

  c(result$min_val, result$max_val)
}

pull_column_range.PostgresConnection <- pull_column_range.RPostgreSQLConnection <- pull_column_range.RedshiftConnection

pull_column_na_ratio.RedshiftConnection <- function(source, col_info, na_ratio) {

  if (!identical(na_ratio, TRUE)) {
    return(NULL)
  }

  result <- DBI::dbGetQuery(
    source, glue::glue(
      "select (count(*) - count({col_info$column_name}))::float / count(*) as na_ratio from {col_info$table_schema}.{col_info$table_name};"
    )
  )

  result$na_ratio
}

pull_column_na_ratio.PostgresConnection <- pull_column_na_ratio.RPostgreSQLConnection <- pull_column_na_ratio.RedshiftConnection

pull_data_nrows.RedshiftConnection <- function(source, schema, nrows, ...) {

  if (nrows == "none") {
    return(NULL)
  }
  tables <- DBI::dbGetQuery(source, glue::glue("SELECT table_name FROM information_schema.tables WHERE table_schema='{schema}';"))
  tables_rows <- list()
  for (table in tables$table_name) {
    tbl_rows <- DBI::dbGetQuery(source, glue::glue("SELECT '{table}' as table_name, COUNT(1) as nrows FROM {schema}.{table};"))
    tables_rows <- append(tables_rows, list(tbl_rows))
  }
  tables_rows <- dplyr::bind_rows(tables_rows)
  if (nrows == "ratio") {
    tables_rows$nrows <- tables_rows$nrows / sum(tables_rows$nrows)
  }
  return(tables_rows)
}

pull_data_nrows.PostgresConnection <- pull_data_nrows.RPostgreSQLConnection <- pull_data_nrows.RedshiftConnection

pull_column_levels_ratio.RedshiftConnection <- function(source, col_info, levels_ratio) {

  if (!identical(levels_ratio, TRUE)) {
    return(NULL)
  }

  result <- DBI::dbGetQuery(
    source, glue::glue(
      "select count(distinct({col_info$column_name}))::float / count(*) as levels_ratio from {col_info$table_schema}.{col_info$table_name};"
    )
  )

  result$levels_ratio
}

pull_column_levels_ratio.PostgresConnection <- pull_column_levels_ratio.RPostgreSQLConnection <- pull_column_levels_ratio.RedshiftConnection

get_schema_info.RedshiftConnection <- function(source, schema) {
  DBI::dbGetQuery(
    source,
    glue::glue("select * from information_schema.columns WHERE table_schema = '{schema}'")
  )
}
get_schema_info.PostgresConnection <- get_schema_info.RPostgreSQLConnection <- get_schema_info.RedshiftConnection
