# todo whink if we need any meta infor for nodes and edges
deps_to_nodes <- function(deps_list) {
  deps_list %>%
    purrr::map_dfr(~ data.frame(name = .$name))
}

deps_to_edges <- function(deps_list) {
  deps_list %>%
    purrr::map_dfr(~ data.frame(from = if (is.null(.$source)) NA else .$source, to = .$name)) %>%
    dplyr::filter(!is.na(.data$from))
}

# todo robustness for no tables
get_fk_dep <- function(table_name, schema) {
  schema_name <- get_schema_name(schema)
  fk_list <- schema[[schema_name]]$tables[[table_name]]$foreign_keys
  dependency <- list(
    name = table_name,
    source = unname(purrr::map_chr(fk_list, ~ .$references$table))
  )
  if (length(dependency$source) == 0) {
    dependency$source <- NULL
  }
  return(dependency)
}

find_table_deps <- function(table_name, schema) {
  # Currently only based on foreign keys. Extend in the future when needed.
  fk_deps <- get_fk_dep(table_name, schema)
  schema_name <- get_schema_name(schema)
  valid_tables <- names(schema[[schema_name]]$tables)

  if (!is.null(fk_deps$source)) {
    invalid_deps <- fk_deps$source[!fk_deps$source %in% valid_tables]
    if (length(invalid_deps)) {
      stop(wrong_deps_message(table_name, invalid_deps))
    }
  }

  return(fk_deps)
}

detect_schema_deps <- function(schema) {
  schema_name <- get_schema_name(schema)
  schema[[schema_name]]$tables %>%
    names() %>%
    purrr::map(find_table_deps, schema = schema)
}

wrong_deps_message <- function(table_name, invalid_tables) {

  plur <- ""
  plur_do <- "doesn't"
  plur_them <- "it"
  if (length(invalid_tables) > 1) {
    plur <- "s"
    plur_do <- "don't"
    plur_them <- "them"
  }

  glue::glue(
    "Table{plur} {paste(invalid_tables, collapse = ', ')} {plur_do} exist{plur}, ",
    "but table {table_name} is dependent on {plur_them}"
  )
}

get_formula_col_dep <- function(col_def, schema) {
  if (is.null(col_def$formula)) {
    return(NULL)
  }
  table <- find_parent(col_def, schema)
  col_name <- attr(col_def, "name")
  table_cols <- names(table$columns)
  formula_vars <- all.names(col_def$formula)
  col_deps <- setdiff(
    formula_vars[formula_vars %in% table_cols],
    col_name
  )

  if (length(col_deps) == 0) {
    return(NULL)
  }

  return(as.character(col_deps))

}

operators <- c(
  "==",  "!=", "!",  ">", ">=", "<", "<=", "(", ")",  "&",
  "&&",  "|", "||", "+", "-", "*", "/", "%%", "%/%", "^", "**"
)

extract_dep_expr_col <- function(expression, col_name, table) {
  expression_vars <- all.names(expression)
  # todo instead of excluding operators, include only valid column names
  expression_vars <- expression_vars[!expression_vars %in% operators][-1] # lhs var name is the target column
  table_cols <- names(table$columns)
  dependent_vars <- setdiff(
    expression_vars[expression_vars %in% table_cols],
    col_name
  )

  if (length(dependent_vars) == 0) {
    return(NULL)
  }

  return(dependent_vars)

}

get_constraint_col_dep <- function(col_def, schema) {

  table <- find_parent(col_def, schema)
  check_constraints <- table$check_constraints
  if (is.null(check_constraints)) {
    return(NULL)
  }

  col_name <- attr(col_def, "name")
  col_constraints <- check_constraints %>%
    purrr::keep(~ constraint_col_related(col_name, .x)) %>%
    purrr::map("expression") %>%
    purrr::map(~ extract_dep_expr_col(.x, col_name, table)) %>%
    purrr::keep(~ !is.null(.x))

  if (length(col_constraints) == 0) {
    return(NULL)
  }

  return(as.character(col_constraints))
}

get_group_by_dep <- function(col_def, schema) {
  if (is.null(col_def$group_by)) {
    return(NULL)
  }

  table_cols <- names(find_parent(col_def, schema)$columns)
  if (!col_def$group_by %in% table_cols) {
    stop(glue::glue(
      "Column {sQuote(col_def$group_by)} defined in group_by parameter doesn't exist in table definition"
    ))
  }

  if (length(col_def$group_by) > 1) {
    stop("Only supported single value of group_by parameter")
  }

  col_def$group_by
}

get_inner_col_dependency <- function(col_def, schema) {

  col_deps <- list(
    name = attr(col_def, "name"),
    source = unique(c(
      get_formula_col_dep(col_def, schema),   # 1. formula
      get_constraint_col_dep(col_def, schema), # 1. column constraint 2. constraint (lhs vs rhs, rhs dependent)
      get_group_by_dep(col_def, schema)
    ))
  )
  if (length(col_deps$source) == 0) {
    col_deps$source <- NULL
  }

  col_deps
}

detect_table_deps <- function(table, schema) {

  table$columns %>%
    purrr::map(get_inner_col_dependency, schema = schema)
}

schema_to_graph <- function(schema) {
  schema_deps <- detect_schema_deps(schema)
  nodes <- deps_to_nodes(schema_deps)
  edges <- deps_to_edges(schema_deps)
  tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
    tidygraph::mutate(table = purrr::map(name, init_table, schema = schema))
}

table_to_graph <- function(table, schema) {
  table_deps <- detect_table_deps(table, schema)
  nodes <- deps_to_nodes(table_deps)
  edges <- deps_to_edges(table_deps)
  tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
}
