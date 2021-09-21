Schema <- R6::R6Class("Schema",
  public = list(
    def = NULL,
    faker_opts = NULL,
    initialize = function(source, schema, file,
                          faker_opts = getOption("dfkr_options", default_faker_opts)) {
      if (!is.character(source)) {
        if (missing(schema)) stop("Parameter 'schema' required when sourcing config from database")
        get_schema_conf(source, schema, file, faker_opts)
      }
      if (is.character(source) && missing(file)) {
        file <- source
      }
      self$def <- read_schema(file, faker_opts)
      self$faker_opts <- faker_opts
    },
    get_opts = function() {
      self$faker_opts
    },
    get_schema = function(type = "def") {
      if (type == "def") {
        return(self$def)
      }
      if (type == "graph") {
        return(attr(self$def, "schema-graph"))
      }
    },
    #todo add get tables that returns list of all
    get_table = function(name, type = "def") {
      if (type == "def") {
        return(self$def[[get_schema_name(self$def)]]$tables[[name]])
      }
      if (type == "graph") {
        return(attr(self$def[[get_schema_name(self$def)]]$tables[[name]], "table-graph"))
      }
      if (type == "value") {
        get_simulated_tbl(name, self$get_schema())
      }
    },
    get_column = function(name, table) {
      if (is.character(table)) {
        return(self$def[[get_schema_name(self$def)]]$tables[[table]]$columns[[name]])
      }
    },
    update_schema = function(value, type = "def") {
      if (type == "def") {
        self$def <- value
        return(invisible(TRUE))
      }
      if (type == "graph") {
        attr(self$def, "schema-graph") <- value
        return(invisible(TRUE))
      }
    },
    update_table_col = function(col_name, table_name, value) {

      update_col <- function(table) {
        table[[col_name]] <- value
        return(table)
      }

      update_cols <- function(name, tables) {
        purrr::modify_if(tables, name == table_name, ~ update_col(.x))
      }

      new_node <- self$get_schema("graph") %>%
        tidygraph::mutate(table = update_cols(name, table))

      self$update_schema(new_node, "graph")

    },
    update_table = function(name, value, type = "def") {
      if (type == "def") {
        self$def[[get_schema_name(self$def)]]$tables[[name]] <- value
        return(invisible(TRUE))
      }
      if (type == "graph") {
        attr(self$def[[get_schema_name(self$def)]]$tables[[name]], "table-graph") <- value
        return(invisible(TRUE))
      }
    },
    update_source = function(file, faker_opts) {
      self$def <- read_schema(file, faker_opts)
      self$faker_opts <- faker_opts
      return(self)
    },
    simulate = function() {
      simulate_schema_obj(self)
      invisible(self)
    }
))

# todo add validation for types existance

read_schema_file <- function(file) {
  schema <- yaml::yaml.load_file(
    input = file,
    handlers = list(expr = function(x) parse(text = x))
  )

  if (length(schema) != 1) {
    stop("Multiple schemas not supported")
  }

  schema <- mod_attr(schema)
  return(schema)
}


schema_nrows <- function(schema, faker_opts) {
  attr(schema, "schema-nrows") <- faker_opts$opt_default_table$nrows(schema[[get_schema_name(schema)]]$tables)
  return(schema)
}

attach_table_deps <- function(schema) {
  schema[[get_schema_name(schema)]]$tables <- schema[[get_schema_name(schema)]]$tables %>%
    purrr::modify(~ mod_table_attr(.x, "table-graph", table_to_graph(.x, schema)))
  return(schema)
}

read_schema <- function(file, simpots = default_faker_opts) {
  schema <- read_schema_file(file)
  schema <- schema_nrows(schema, simpots)
  schema_graph <- schema_to_graph(schema)
  attr(schema, "schema-graph") <- schema_graph
  schema <- attach_table_deps(schema)
  return(schema)
}

#' Source schema file into dependency graph object
#'
#' The functions parses table schema (from database) and saves its structure yaml format.
#' The defined structure is then used to prepare schema dependency graph, that is:
#' \itemize{
#'   \item{dependencies between tables}{Based on foreign key definitions}
#'   \item{inner table column dependencies}{Based on defined dependencies by various methods. See \code{vignette('todo')}.}
#' }
#'
#' Detected dependencies are then saved in R6Class object that is returned and possible to pass for further methods.
#' See \link{schema_methods}.
#'
#' Keeping the schema as a graph allows to perform simulation process in proper order,
#' preserving table dependencies and constraints.
#'
#'
#' @param source Connection to Redshift or Postgres database or path to YAML configuration file
#' from which schema metadata should be sourced.
#' When missing \code{file} defined file will be sourced if existing.
#' @param schema Schema name from which the structure should be sourced.
#' @param file Path to yaml file describing database schema, or target file when schema should be saved
#' (when \code{db_conn} not mising). See \code{vignette('todo')}.
#' @param faker_opts Structure sourcing and columns simulation config.
#' @export
schema_source <- function(source, schema = "public",
                          file = if (is.character(source)) source else file.path(getwd(), "schema.yml"),
                          faker_opts = getOption("dfkr_options", default_faker_opts)) {
  Schema$new(source, schema, file, faker_opts)
}

#' Schema object methods
#'
#' The set of methods that can be used on schema object returned by \code{\link{schema_source}} function.
#'
#' The methods are:
#' \itemize{
#'   \item{schema_update_source}{ Update schema dependency graph based on provided file.}
#'   \item{schema_simulate}{ Run data simulation process.}
#'   \item{schema_get_table}{ Get simulated table value.}
#'   \item{schema_plot_deps}{ Plot inter or inner table dependecies.}
#' }
#'
#' @param schema Schema object keeping table dependency graph.
#' @param file Path to schema configuration yaml file.
#' @param faker_opts Structure sourcing and columns simulation config.
#' @param table_name Name of the table.
#'
#' @name schema_methods
NULL

#' @rdname schema_methods
#' @export
schema_update_source <- function(schema, file, faker_opts = getOption("dfkr_options", default_faker_opts)) {
  schema$update_source(file, faker_opts)
}

#' @rdname schema_methods
#' @export
schema_get_table <- function(schema, table_name) {
  schema$get_table(table_name, "value")
}

#' @rdname schema_methods
#' @export
schema_plot_deps <- function(schema, table_name) {
  if (missing(table_name)) {
    plot(schema$get_schema("graph"))
  } else {
    plot(schema$get_table(table_name, "graph"))
  }
}

#' @rdname schema_methods
#' @export
schema_simulate <- function(schema) {
  schema$simulate()
}

get_schema_constraints <- function(source, ...) {
  UseMethod("get_schema_constraints", source)
}

get_table_pk <- function(source, ...) {
  UseMethod("get_table_pk", source)
}

pull_column_values <- function(source, ...) {
  UseMethod("pull_column_values", source)
}

pull_column_nchar <- function(source, ...) {
  UseMethod("pull_column_nchar", source)
}

pull_column_range <- function(source, ...) {
  UseMethod("pull_column_range", source)
}

pull_column_na_ratio <- function(source, ...) {
  UseMethod("pull_column_na_ratio", source)
}

pull_column_levels_ratio <- function(source, ...) {
  UseMethod("pull_column_levels_ratio", source)
}

pull_data_nrows <- function(source, ...) {
  UseMethod("pull_data_nrows", source)
}

get_schema_info <- function(source, ...) {
  UseMethod("get_schema_info", source)
}

pull_column_data_info <- function(source, column_name, column_info, options) {
  column_type <- get_column_type(list(type = column_info$data_type), options)
  pull_opts <- switch(column_type,
                      "character" = options$opt_pull_character,
                      "numeric" = options$opt_pull_numeric,
                      "integer" = options$opt_pull_integer,
                      "logical" = options$opt_pull_logical,
                      "date" = options$opt_pull_date
  )
  # todo think how to make it more customizable
  data_info <- list(
    values = pull_column_values(source, column_info, pull_opts$values, pull_opts$max_uniq_to_pull),
    nchar = pull_column_nchar(source, column_info, pull_opts$nchar),
    range = pull_column_range(source, column_info, pull_opts$range),
    na_ratio = pull_column_na_ratio(source, column_info, pull_opts$na_ratio),
    levels_ratio = pull_column_levels_ratio(source, column_info, pull_opts$levels_ratio)
  )
  data_info %>% purrr::keep(~ !is.null(.x))
}

get_column_conf <- function(column_name, source, pkeys, table_info, table_constraints, options) {

  verbmsg(glue::glue("Pulling {sQuote(column_name)} column metadata"), 2)

  column_info <- table_info %>%
    dplyr::filter(column_name == !!column_name)
  column_constraint <- table_constraints %>%
    dplyr::filter(column_name == !!column_name, constraint_type == "UNIQUE")
  column_conf <- list(
    type = column_info$data_type,
    default = column_info$column_default,
    not_null = if (column_info$is_nullable == "YES") FALSE else TRUE,
    unique = if (nrow(column_constraint) == 0) FALSE else TRUE
  )

  if (column_name %in% pkeys) {
    column_conf$unique <- TRUE
    column_conf$not_null <- TRUE
  }

  column_data_info <- pull_column_data_info(source, column_name, column_info, options)

  return(c(column_conf, column_data_info))
}

get_fk_list <- function(constraint_name, fk_constraints) {
  constraint_meta <- fk_constraints %>%
    dplyr::filter(constraint_name == !!constraint_name)
  list(
    columns = list(constraint_meta$column_name),
    references = list(
      columns = list(constraint_meta$fk_column_name),
      table = constraint_meta$fk_table_name
    )
  )
}

get_constraint_list <- function(constraint_name, check_constraints) {
  constraint_meta <- check_constraints %>%
    dplyr::filter(constraint_name == !!constraint_name)
  check_clause <- constraint_meta$check_clause
  attr(check_clause, "tag") <- "!expr"
  list(
    column = constraint_meta$column_name,
    expression = check_clause
  )
}

get_table_conf <- function(table_name, schema_info, schema_constraints, schema_nrows, options, source) {

  verbmsg(glue::glue("Preparing schema dump for table {sQuote(table_name)}"), 0)

  table_info <- schema_info %>%
    dplyr::filter(table_name == !!table_name)
  table_constraints <- schema_constraints %>%
    dplyr::filter(table_name == !!table_name)
  pkeys <- unique(
    get_table_pk(source, schema_info$table_schema[1], table_name)$column_name,
    table_constraints %>% dplyr::filter(constraint_type == "PRIMARY KEY") %>% dplyr::pull(column_name)
  )
  check_constraints <- table_constraints %>%
    dplyr::filter(constraint_type == "CHECK")
  fk_constraints <- table_constraints %>%
    dplyr::filter(constraint_type == "FOREIGN KEY")
  table_conf <- list(
    check_constraints = unique(check_constraints$constraint_name) %>%
      stats::setNames(unique(check_constraints$constraint_name)) %>%
      purrr::map(get_constraint_list, check_constraints = check_constraints),
    foreign_keys = unique(fk_constraints$constraint_name) %>%
      stats::setNames(unique(fk_constraints$constraint_name)) %>%
      purrr::map(get_fk_list, fk_constraints = fk_constraints),
    # todo handle multiple pkeys
    primary_key = list(
      list(
        columns = list(pkeys)
      )
    ) %>% stats::setNames(glue::glue("{table_name}_pkey")),
    nrows = if (is.null(schema_nrows)) NULL else dplyr::filter(schema_nrows, table_name == !!table_name)$nrows,
    columns = unique(table_info$column_name) %>%
      stats::setNames(unique(table_info$column_name)) %>%
      purrr::map(
        get_column_conf, source = source, pkeys = pkeys,
        table_info = table_info, table_constraints = table_constraints, options = options
      )
  )
  if (length(pkeys) == 0) {
    table_conf$primary_key <- NULL
  }

  if (length(check_constraints$constraint_name) == 0) {
    table_conf$check_constraints <- NULL
  }

  if (length(fk_constraints$constraint_name) == 0) {
    table_conf$foreign_keys <- NULL
  }

  if (is.null(table_conf$nrows)) {
    table_conf$nrows <- NULL
  }

  table_conf
}

get_schema_conf <- function(source, schema, file = file.path(getwd(), "schema.yml"),
                            options = default_faker_opts, tables = NULL) {

  schema_info <- get_schema_info(source, schema = schema)
  schema_nrows <- pull_data_nrows(source, schema = schema, nrows = options$opt_pull_table$nrows)

  # access to table_constraints may be limited, so the table can be empty
  schema_constraints <- get_schema_constraints(source, schema = schema)

  if (!is.null(tables)) {
    schema_info <- schema_info %>%
      dplyr::filter(table_name %in% !!tables)
    schema_constraints <- schema_constraints %>%
      dplyr::filter(table_name %in% !!tables)
    if (!is.null(schema_nrows)) {
      schema_nrows <- schema_nrows %>%
        dplyr::filter(table_name %in% !!tables)
    }
  }
  schema_conf <- list(
    list(
      tables = unique(schema_info$table_name) %>%
        stats::setNames(unique(schema_info$table_name)) %>%
        purrr::map(
          get_table_conf,
          schema_info = schema_info, schema_constraints = schema_constraints,
          schema_nrows = schema_nrows, options = options, source = source
        )
    )
  ) %>% stats::setNames(schema)

  yaml::write_yaml(
    schema_conf, file = file,
    handlers = list(
      Date = function(x) as.character(x),
      logical = function(x) {
        result <- ifelse(x, "true", "false")
        class(result) <- "verbatim"
        result
      }
    )
  )

  return(invisible(TRUE))
}
