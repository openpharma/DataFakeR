get_table_info_df <- function(table, table_name) {
  if (length(table) == 1) {
    table <- list(table)
  }
  cbind(
    table_name = table_name,
    table %>%
      purrr::imap_dfr(
        ~ get_col_info_df(.x, column_name = .y)
      )
  )
}

get_col_info_df <- function(column, column_name) {
  data.frame(
    column_name = column_name,
    column_default = NA,
    is_nullable = if (any(is.na(column))) "YES" else "NO",
    data_type = class(column)
  )
}

get_schema_info.list <- function(source, schema) {

  if (is.null(names(source))) {
    stop("List of tables have to be named.")
  }

  cbind(
    table_schema = schema,
    source %>%
      purrr::imap_dfr(
        ~ get_table_info_df(.x, table_name = .y)
      )
  )
}

pull_data_nrows.list <- function(source, schema, nrows) {
  if (nrows == "none") {
    return(NULL)
  }
  tables_rows <- source %>%
    purrr::imap_dfr(
      ~ data.frame(table_name = .y, nrows = nrow(.x))
    )
  if (nrows == "ratio") {
    tables_rows$nrows <- tables_rows$nrows / sum(tables_rows$nrows)
  }

  return(tables_rows)
}

get_table_constraint_df <- function(table, table_name) {
  if (length(table) == 1) {
    table <- list(table)
  }
  cbind(
    table_name = table_name,
    table %>%
      purrr::imap_dfr(
        ~ get_col_constraint_df(.x, column_name = .y)
      )
  )
}

get_col_constraint_df <- function(column, column_name) {

  unique_col <- !any(duplicated(column))

  data.frame(
    column_name = column_name,
    constraint_name = paste0(column_name, "_constraint"),
    constraint_type = if (unique_col) "UNIQUE" else "none",
    check_clause = NA,
    unique_constraint_name = paste0(column_name, "_constraint"),
    fk_table_name = NA,
    fk_column_name = NA
  )
}

get_schema_constraints.list <- function(source, schema) {
  if (is.null(names(source))) {
    stop("List of tables have to be named.")
  }

  cbind(
    table_schema = schema,
    source %>%
      purrr::imap_dfr(
        ~ get_table_constraint_df(.x, table_name = .y)
      )
  ) %>%
    dplyr::filter(constraint_type %in% c("UNIQUE", "PRIMARY KEY", "FOREIGN KEY"))
}

get_table_pk.list <- function(source, schema, table_name) {
  data.frame(
    column_name = character(0),
    data_type = character(0)
  )
}

pull_column_values.list <- function(source, col_info, values, max_uniq_to_pull) {
  if (!identical(values, TRUE) || is.null(max_uniq_to_pull)) {
    return(NULL)
  }

  n_vals <- length(unique(source[[col_info$table_name]][[col_info$column_name]]))
  if (n_vals > max_uniq_to_pull) {
    return(NULL)
  }
  result <- unique(source[[col_info$table_name]][[col_info$column_name]])

  result
}

pull_column_nchar.list <- function(source, col_info, nchar) {
  if (!identical(nchar, TRUE)) {
    return(NULL)
  }

  nchar <- max(nchar(as.character(source[[col_info$table_name]][[col_info$column_name]])), na.rm = TRUE)

  nchar
}

pull_column_range.list <- function(source, col_info, range) {

  if (!identical(range, TRUE)) {
    return(NULL)
  }

  result <- range(source[[col_info$table_name]][[col_info$column_name]], na.rm = TRUE)

  c(result[1], result[2])
}

pull_column_na_ratio.list <- function(source, col_info, na_ratio) {

  if (!identical(na_ratio, TRUE)) {
    return(NULL)
  }

  col_val <- source[[col_info$table_name]][[col_info$column_name]]
  result <- sum(is.na(col_val)) / length(col_val)

  result
}

pull_column_levels_ratio.list <- function(source, col_info, levels_ratio) {

  if (!identical(levels_ratio, TRUE)) {
    return(NULL)
  }

  col_val <- source[[col_info$table_name]][[col_info$column_name]]
  result <- length(unique(col_val)) / length(col_val)

  result
}
