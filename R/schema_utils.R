add_table_column_attrs <- function(table, table_name, schema_name) {
  table$columns <- purrr::imodify(
    table$columns,
    ~ add_type(.x, "column", .y, extra = list(name = "parent", value = table_name))
  )
  table <- add_type(table, "table", table_name, extra = list(name = "parent", value = schema_name))
  table
}

mod_attr <- function(schema) {
  schema_name <- get_schema_name(schema)
  schema[[1]] <- add_type(schema[[1]], "schema", schema_name)

  schema[[schema_name]]$tables <- schema[[schema_name]]$tables %>%
    purrr::imodify(~ add_table_column_attrs(.x, .y, schema_name))

  if (length(schema[[schema_name]]$tables) == 1) {
    names(schema[[schema_name]]$tables) <- attr(schema[[schema_name]]$tables[[1]], "name")
  }

  schema
}

find_parent <- function(schema_obj, schema) {
  if (is.null(attr(schema_obj, "type"))) {
    stop("wrong object")
  }

  if (attr(schema_obj, "type") == "schema") {
    return(NULL)
  }

  if (attr(schema_obj, "type") == "table") {
    return(schema[[attr(schema_obj, "parent")]])
  }

  if (attr(schema_obj, "type") == "column") {
    return(schema[[get_schema_name(schema)]]$tables[[attr(schema_obj, "parent")]])
  }
}

verbmsg <- function(msg, ind = 0) {
  if (getOption("dfkr_verbose", FALSE)) {
    indent <- paste0(c(rep(" ", ind), rep("=", 5 - ind)), collapse = "")
    message(glue::glue("{indent}> {msg}"))
  }
  invisible(msg)
}


get_schema_name <- function(schema) {
  names(schema)
}

add_type <- function(obj, type, name, extra) {
  if (!missing(extra)) {
    attr(obj, extra$name) <- extra$value
  }
  attr(obj, "name") <- name
  attr(obj, "type") <- type
  obj
}

init_table <- function(table_name, schema) {
  empty_table <- tibble::tibble(.rows = attr(schema, "schema-nrows")[[table_name]])
  attr(empty_table, "name") <- table_name
  return(empty_table)
}

mod_table_attr <- function(table, name, value) {
  attr(table, name) <- value
  table
}

if_null_default <- function(param_name, col_def, faker_opts) {
  param <- col_def[[param_name]]
  if (is.null(param)) {
    param <- faker_opts[[param_name]]
  }
  return(param)
}
