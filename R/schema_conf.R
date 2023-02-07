#' Setup default column type parameters
#'
#' All the parameters (excluding \code{regexp}) are attached to column definition
#' when the ones are not specified in configuration YAML file.
#' All the functions are used to specify default configuration
#' (see: \code{\link{default_faker_opts}}).
#'
#' @param regexp Regular expression that allows mapping YAML configuration column
#'   type to desired R class.
#' @param nchar Maximum number of characters when simulating character values.
#'   When source column is of type \code{char(n)} the parameter is ignored.
#' @param not_null Should the column allow to simulate NA values?
#' @param unique Should column values be unique?
#' @param default Default column value. Ignored during simulation.
#' @param na_ratio Ratio of NA values returned in simulated sample.
#' @param levels_ratio Ratio of unique values (in terms of sample length) simulated in the sample.
#' @param precision Precision of numeric column value when simulating numeric values.
#'   When source column is of type e.g. \code{numeric(precision)} the parameter is ignored.
#' @param scale Precision of numeric column value when simulating numeric values.
#'   When source column is of type e.g. \code{numeric(precision, scale)} the parameter is ignored.
#' @param min_date,max_date Minimum and maximum date used when simulating Date columns.
#' @param format Format of date used when simulating Date columns.
#' @param ... Other default parameters attached to the column definition.
#'
#' @name default_simulation_params
NULL

#' @rdname default_simulation_params
#' @export
opt_default_character <- function(regexp = "text|char|factor", nchar = 10, na_ratio = 0.05,
                                  not_null = FALSE, unique = FALSE, default = "", levels_ratio = 1, ...) {
  list(
    regexp = regexp,
    nchar = nchar,
    not_null = not_null,
    unique = unique,
    default = default,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname default_simulation_params
#' @export
opt_default_numeric <- function(regexp = "^decimal|^numeric|real|double precision",
                                na_ratio = 0.05, not_null = FALSE, unique = FALSE,
                                default = 0, precision = 7, scale = 2, levels_ratio = 1, ...) {
  list(
    regexp = regexp,
    not_null = not_null,
    unique = unique,
    default = default,
    na_ratio = na_ratio,
    precision = precision,
    scale = scale,
    levels_ratio = levels_ratio,
    ...
  )
}

integer_ranges <- list(
  smallint = c(-32768, 32767),
  integer = c(-2147483648, 2147483647),
  bigint = c(-9223372036854775808, 9223372036854775807),
  smallserial = c(1, 32767),
  serial = c(1, 2147483647),
  bigserial = c(1, 9223372036854775807)
)

#' @rdname default_simulation_params
#' @export
opt_default_integer <- function(regexp = "smallint|integer|bigint|smallserial|serial|bigserial",
                                na_ratio = 0.05, not_null = FALSE, unique = FALSE, default = "",
                                levels_ratio = 1, ...) {
  list(
    regexp = regexp,
    not_null = not_null,
    unique = unique,
    default = default,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname default_simulation_params
#' @export
opt_default_logical <- function(regexp = "boolean|logical", na_ratio = 0.05, not_null = FALSE,
                                unique = FALSE, default = FALSE, levels_ratio = 1, ...) {
  list(
    regexp = regexp,
    not_null = not_null,
    unique = unique,
    default = default,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname default_simulation_params
#' @export
opt_default_date <- function(regexp = "date|Date", na_ratio = 0.05, not_null = FALSE, unique = FALSE,
                             default = Sys.Date(), format = "%Y-%m-%d",
                             min_date = as.Date("1970-01-01"), max_date = Sys.Date(),
                             levels_ratio = 1, ...) {
  list(
    regexp = regexp,
    not_null = not_null,
    unique = unique,
    default = default,
    format = format,
    min_date = min_date,
    max_date = max_date,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' Configure data simulation options
#'
#' The parameters affect high level (not column type related) simulation settings
#' such as target number of rows for each table.
#' Currently only number of simulated rows is supported.
#'
#' @param nrows Integer or function. When \code{nrows} is precised as an integer value,
#' all the tables will have the same number of rows.
#' In case of function, the should take tables configuration (list of tables section
#' from configuration YSML file) and return named list of table with rows values.
#' See \code{\link{nrows_simul_constant}} and \code{\link{nrows_simul_ratio}} for more details.
#'
#' @export
opt_default_table <- function(nrows = nrows_simul_constant(10)) {
  list(
    nrows = nrows # function or number
  )
}

#' Methods for extracting number of target rows in simulation
#'
#' Each method returns function of list of tables.
#' The value of such function is named list being mapping between tables
#' (names of list) and target number of rows (values of list).
#' Such methods can be passed as \code{nrows} parameter of \link{opt_default_table}.
#'
#' Currently supported methods are:
#' \itemize{
#'   \item{nrows_simul_constant}{
#'     Returns \code{n} rows for each table when not defined in YAML parameter \code{nrows}
#'   }
#'   \item{nrows_simul_ratio}{
#'     Returns \code{nrows * ratio} when \code{nrows} defined as YAML parameter and is integer.
#'     Returns \code{nrows} when \code{nrows} defined as YAML parameter and id fraction,
#'     Returns \code{n * ratio} otherwise.
#'   }
#' }
#'
#' @param n Default number of rows for each table when not defined in configuration file.
#' @param ratio,total The parameters multiplications results with defining target
#'   number of rows for simulated table. See details section.
#' @param force Should specified parameters overwrite related configuration parameters?
#' @name number_of_rows
NULL

#' @rdname number_of_rows
#' @export
nrows_simul_constant <- function(n, force = FALSE) {

  n_if_null <- function(table, n) {
    if (!is.null(table$nrows) && !force) {
      n <- table$nrows
    }
    return(n)
  }

  function(tables) {
    purrr::map(tables, n_if_null, n = n)
  }

}

#' @rdname number_of_rows
#' @export
nrows_simul_ratio <- function(ratio, total, force = FALSE) {
  n_if_null <- function(table, ratio) {
    if (!is.null(table$nrows) && !force) {
      if (table$nrows <= 0 && table$nrows >= 1) {
        stop("Provided table configuration nrows is not a ratio")
      }
      n <- round(table$nrows * total)
    } else {
      n <- round(ratio * total)
    }
    return(n)
  }

  function(tables) {
    purrr::map(tables, n_if_null, ratio = ratio)
  }
}

#' Set of functions defining special simulation methods for column and its type
#'
#' Whenever there's a need to simulate column using specific function (as a \code{spec}
#' parameter in YAML configuration file), such method should be defined in one of
#' \code{opt_simul_spec_<column_type>} functions.
#'
#' @details
#'
#' Currently defined special methods are:
#' \itemize{
#'   \item{name}{
#'     For character column, that allows to simulate character reflecting real names and surnames
#'   }
#'   \item{distr}{
#'     For all the remaining column types. The method allows to simulate data with specified
#'     distribution generator, such as \code{rnorm}, \code{rbinom} etc.
#'   }
#' }
#'
#' Each 'spec' method receives \code{n} parameter (the desired number of rows to simulate),
#' all the default column-based parameters (type, unique, not_null, etc.) but also a special
#' one named \code{spec_params} that are applied to selected distribution simulation method.
#'
#' See for example \code{\link{simul_spec_character_name}} definition.
#'
#' @param name Function for simulating personal names.
#' @param distr Function for simulating data from desired distribution.
#' @param ... Other custom special methods.
#' @name special_simulation
NULL

#' @rdname special_simulation
#' @export
opt_simul_spec_character <- function(name = simul_spec_character_name, ...) {
  list(
    name = name,
    ...
  )
}

#' @rdname special_simulation
#' @export
opt_simul_spec_numeric <- function(distr = simul_spec_numeric_distr, ...) {
  list(
    distr = distr,
    ...
  )
}

#' @rdname special_simulation
#' @export
opt_simul_spec_integer <- function(distr = simul_spec_integer_distr, ...) {
  list(
    distr = distr,
    ...
  )
}

#' @rdname special_simulation
#' @export
opt_simul_spec_logical <- function(distr = simul_spec_logical_distr, ...) {
  list(
    distr = distr,
    ...
  )
}

#' @rdname special_simulation
#' @export
opt_simul_spec_date <- function(distr = simul_spec_date_distr, ...) {
  list(
    distr = distr,
    ...
  )
}

#' Simulate data restricted by extra column parameters
#'
#' The functions allow to define a set of methods for simulating data using additional
#' column-based parameters such as range or values.
#'
#' @details
#'
#' Except for the standard column parameters, that are now:
#' \itemize{
#'   \item{type}
#'   \item{unique}
#'   \item{not_null}
#'   \item{default}
#'   \item{nchar}
#'   \item{min_date}
#'   \item{max_date}
#'   \item{precision}
#'   \item{scale}
#' }
#'
#' it is also allowed to add custom ones (either directly in YAML configuration file,
#' or in \code{opt_default_<column_type>} functions).
#'
#' In order to respect simulation using such parameters, we may want to define our custom simulation
#' functions.
#'
#' Such functions should be defined as a parameters of \code{opt_simul_restricted_<column_type>} functions,
#' and each of them should take special parameter as its own one.
#'
#' When the parameter condition is not met (for example the parameter is missing) such function should
#' return NULL value. This allows the simulation workflow to move to the next defined method.
#' The order of methods execution is followed by the order of defined parameters in the below methods.
#'
#' That means, the highest priority always have \code{f_key} - a special method that is used for foreign key
#' columns, and simulates only from values received from parent primary key.
#'
#' The second priority method for character type columns is in_set, that seeks for \code{values} column
#' parameter, and when such exists it simulates the data from defined set of values.
#' See \code{\link{simul_restricted_character_in_set}} definition to check details.
#'
#' @param f_key Method for simulating foreign key columns. The \code{values} parameter of the function,
#' receives all the unique values from parent primary key column.
#' @param in_set Method for simulating columns from defined set of values. The \code{values}
#' parameter of the function, take all the values defined in YAML column definition as \code{values}
#' parameter.
#' @param range Method for simulating columns fitting inside defined range. It takes special parameter
#' \code{range} 2-length vector minimum and maximum value for simulated data.
#' @param ... Other methods that can be defined to handle extra parameters.
#'
#' @name restricted_simulation
NULL

#' @rdname restricted_simulation
#' @export
opt_simul_restricted_character <- function(f_key = simul_restricted_character_fkey,
                                           ...,
                                           in_set = simul_restricted_character_in_set) {
  list(
    f_key = f_key,
    ...,
    in_set = in_set
  )
}

#' @rdname restricted_simulation
#' @export
opt_simul_restricted_numeric <- function(f_key = simul_restricted_numeric_fkey,
                                         ...,
                                         in_set = simul_restricted_numeric_in_set,
                                         range = simul_restricted_numeric_range) {
  list(
    f_key = f_key,
    ...,
    in_set = in_set,
    range = range
  )
}

#' @rdname restricted_simulation
#' @export
opt_simul_restricted_integer <- function(f_key = simul_restricted_integer_fkey,
                                         ...,
                                         in_set = simul_restricted_integer_in_set,
                                         range = simul_restricted_integer_range) {
  list(
    f_key = f_key,
    ...,
    in_set = in_set,
    range = range
  )
}

#' @rdname restricted_simulation
#' @export
opt_simul_restricted_logical <- function(f_key = simul_restricted_integer_fkey, ...) {
  list(
    f_key = f_key,
    ...
  )
}

#' @rdname restricted_simulation
#' @export
opt_simul_restricted_date <- function(f_key = simul_restricted_integer_fkey,
                                      ...,
                                      range = simul_restricted_date_range) {
  list(
    f_key = f_key,
    ...,
    range = range
  )
}

#' Specify YAML configuration options while pulling the schema from DB
#'
#' The set of function allows to configure which data information should be saved
#' to configuration YAML file when such configuration is sourced directly from
#' database schema.
#'
#' @param values Should column unique values be sourced? If so the ones are stored as
#' an array withing \code{values} parameter.
#' @param max_uniq_to_pull Pull unique values only when the distinct number of them is less than
#' provided value. The parameter prevents for sourcing large amount of values to configuration file
#' for example when dealing with ids column.
#' @param nchar Should maximum number of characters in column be pulled? Is so stored as
#' \code{nchar} parameter in configuration YAML file.
#' @param na_ratio Should ratio of NA values existing in column be sourced?
#' @param levels_ratio Should ratio of unique column values be sourced?
#' @param range Should column range be sourced? Is so stored as \code{range} parameter in
#' configuration YAML file.
#' @param precision Currently unused.
#' @param scale Currently unused.
#' @param nrows Should number of original columns be sourced? When 'exact' stored as a \code{nrows}
#' parameter for each table in YAML configuration file. When 'ratio' stored as a fraction of original columns
#' (based on all tables) and saved as \code{nrows} configuration parameter. When 'none'
#' tables rows information will not be saved.
#' @param ... Other parameters defining column metadata source. Currently unsupported.
#'
#' @name sourcing_metadata
NULL

#' @rdname sourcing_metadata
#' @export
opt_pull_character = function(values = TRUE, max_uniq_to_pull = 10, nchar = TRUE,
                              na_ratio = TRUE, levels_ratio = TRUE, ...) {
  list(
    values = values,
    max_uniq_to_pull = max_uniq_to_pull,
    nchar = nchar,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname sourcing_metadata
#' @export
opt_pull_numeric = function(values = TRUE, max_uniq_to_pull = 10, range = TRUE, precision = TRUE,
                            scale = TRUE, na_ratio = TRUE, levels_ratio = FALSE, ...) {
  list(
    values = values,
    max_uniq_to_pull = max_uniq_to_pull,
    range = range,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    # todo mean, median, sd?
    ...
  )
}

#' @rdname sourcing_metadata
#' @export
opt_pull_integer = function(values = TRUE, max_uniq_to_pull = 10, range = TRUE,
                            na_ratio = TRUE, levels_ratio = FALSE, ...) {
  list(
    values = values,
    max_uniq_to_pull = max_uniq_to_pull,
    range = range,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname sourcing_metadata
#' @export
opt_pull_date = function(range = TRUE, na_ratio = TRUE, levels_ratio = FALSE, ...) {
  list(
    range = range,
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname sourcing_metadata
#' @export
opt_pull_logical <- function(na_ratio = TRUE, levels_ratio = FALSE, ...) {
  list(
    na_ratio = na_ratio,
    levels_ratio = levels_ratio,
    ...
  )
}

#' @rdname sourcing_metadata
#' @export
opt_pull_table <- function(nrows = "exact", ...) {
  if (!nrows %in% c("exact", "ratio", "none")) {
    warning("You cannot pull data exact nrows and ratio. Using ratio by default.")
    exact <- FALSE
  }
  list(
    nrows = nrows,
    ...
  )
}

#' Default options for pulling metadata and data simulation
#'
#' Generated with the set of configuration functions:
#' \link{default_simulation_params}, \link{opt_default_table},
#' \link{special_simulation}, \link{restricted_simulation},
#' \link{sourcing_metadata}.
#'
#' \code{set_faker_opts} allows to overwrite selected options.
#' \code{get_faker_opts} lists the current options configuration.
#'
#' @param opt_pull_character,opt_pull_numeric,opt_pull_integer,opt_pull_logical,opt_pull_date,opt_pull_table,opt_default_character,opt_simul_spec_character,opt_simul_restricted_character,opt_simul_default_fun_character,opt_default_numeric,opt_simul_spec_numeric,opt_simul_restricted_numeric,opt_simul_default_fun_numeric,opt_default_integer,opt_simul_spec_integer,opt_simul_restricted_integer,opt_simul_default_fun_integer,opt_default_logical,opt_simul_spec_logical,opt_simul_restricted_logical,opt_simul_default_fun_logical,opt_default_date,opt_simul_spec_date,opt_simul_restricted_date,opt_simul_default_fun_date,opt_default_table
#' Parameters defined in default configuration that can be modified by using \code{set_faker_opts} function.
#' Please make sure each parameter is specified by method designed to it.
#' @param global If TRUE, default the configuration will be set up globally
#' (no need to pass it as a \code{faker_opts} parameter for \link{schema_source} and \link{schema_methods}).
#'
#' @name faker_configuration
NULL

#' @rdname faker_configuration
#' @export
default_faker_opts <- list(
  opt_pull_character = opt_pull_character(),
  opt_pull_numeric = opt_pull_numeric(),
  opt_pull_integer = opt_pull_integer(),
  opt_pull_logical = opt_pull_logical(),
  opt_pull_date = opt_pull_date(),
  opt_pull_table = opt_pull_table(),
  opt_default_character = opt_default_character(),
  opt_simul_spec_character = opt_simul_spec_character(),
  opt_simul_restricted_character = opt_simul_restricted_character(),
  opt_simul_default_fun_character = simul_default_character,
  opt_default_numeric = opt_default_numeric(),
  opt_simul_spec_numeric = opt_simul_spec_numeric(),
  opt_simul_restricted_numeric = opt_simul_restricted_numeric(),
  opt_simul_default_fun_numeric = simul_default_numeric,
  opt_default_integer = opt_default_integer(),
  opt_simul_spec_integer = opt_simul_spec_integer(),
  opt_simul_restricted_integer = opt_simul_restricted_integer(),
  opt_simul_default_fun_integer = simul_default_integer,
  opt_default_logical = opt_default_logical(),
  opt_simul_spec_logical = opt_simul_spec_logical(),
  opt_simul_restricted_logical = opt_simul_restricted_logical(),
  opt_simul_default_fun_logical = simul_default_logical,
  opt_default_date = opt_default_date(),
  opt_simul_spec_date = opt_simul_spec_date(),
  opt_simul_restricted_date = opt_simul_restricted_date(),
  opt_simul_default_fun_date = simul_default_date,
  opt_default_table = opt_default_table()
)


#' @rdname faker_configuration
#' @export
set_faker_opts <- function(
  opt_pull_character, opt_pull_numeric, opt_pull_integer, opt_pull_logical, opt_pull_date, opt_pull_table,
  opt_default_character, opt_simul_spec_character, opt_simul_restricted_character, opt_simul_default_fun_character,
  opt_default_numeric, opt_simul_spec_numeric, opt_simul_restricted_numeric, opt_simul_default_fun_numeric,
  opt_default_integer, opt_simul_spec_integer, opt_simul_restricted_integer, opt_simul_default_fun_integer,
  opt_default_logical, opt_simul_spec_logical, opt_simul_restricted_logical, opt_simul_default_fun_logical,
  opt_default_date, opt_simul_spec_date, opt_simul_restricted_date, opt_simul_default_fun_date, opt_default_table,
  global = TRUE
  ) {

  current_opts <- default_faker_opts
  env_opts <- environment() %>%
    as.list() %>%
    purrr::keep(~ !is.symbol(.x))

  env_opts_names <- names(env_opts)

  for (opt_name in env_opts_names) {
    current_opts[[opt_name]] <- env_opts[[opt_name]]
  }

  if (global) {
    options("dfkr_options" = current_opts)
    return(invisible(current_opts))
  }
  current_opts
}

#' @rdname faker_configuration
#' @export
get_faker_opts <- function() {
  getOption("dfkr_options")
}
