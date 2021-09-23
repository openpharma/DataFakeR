#' Logical type simulation methods
#'
#' @param n Number of values to simulate.
#' @param not_null Should NA values be forbidden?
#' @param unique Should duplicated values be allowed?
#' @param default Default column value.
#' @param spec_params Set of parameters passed to special method.
#' @param type Column raw type (sourced from configuration file).
#' @param values Possible values from which to perform simulation.
#' @param na_ratio Ratio of NA values (in terms of sample length) the sample should have.
#' @param levels_ratio Fraction of levels (in terms of sample length) the sample should have.
#' @param ... Other parameters passed to column configuration in YAML file.
#'
#' @name simulation_methods_logical
NULL

#' @rdname simulation_methods_logical
#' @export
simul_spec_logical_distr <- function(n, not_null, unique, default, spec_params, na_ratio, levels_ratio, ...) {
  call_args <- names(sys.call())
  if (!"spec_params" %in% call_args) {
    stop(glue::glue(
      "{sQuote('distr')} spec method for numerical columns requires {sQuote('spec_params')} defined"
    ))
  }

  if (isTRUE(unique)) {
    warning("Cannot specify unique = true for boolean. The parameter will be ignored.")
  }

  method <- spec_params$distr
  spec_params$distr <- NULL
  if (method == "sample") {
    spec_params$size <- n
  } else {
    spec_params$n <- n
  }
  do.call(
    method,
    spec_params
  ) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

#' @rdname simulation_methods_logical
#' @export
simul_default_logical <- function(n, not_null, unique, default, type, na_ratio, levels_ratio, ...) {

  if (isTRUE(unique)) {
    warning("Cannot specify unique = true for boolean. The parameter will be ignored.")
  }

  values <- c(TRUE, FALSE)
  # todo handle more params (not_null, unique, default)
  return(
    sample(values, n, replace = TRUE) %>%
      levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
  )
}

#' @rdname simulation_methods_logical
#' @export
simul_restricted_logical_fkey <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {

  if (isTRUE(not_null)) {
    values <- values[!is.na(values)]
  }

  if (isTRUE(unique)) {
    warning("Cannot specify unique = true for boolean. The parameter will be ignored.")
  }
  # todo consider other options if related like distribution in the future
  sample(values, n, replace = TRUE) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

lgl_generate_spec <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  if (!params$spec %in% names(faker_opts$opt_simul_spec_logical)) {
    stop(glue::glue(
      "Special method {sQuote(params$spec)} attached for column {sQuote(attr(col_def, 'name'))} is not defined. ",
      "Please check {sQuote('opt_simul_spec_*')} options."
    ))
  }

  do.call(
    faker_opts$opt_simul_spec_logical[[col_def$spec]],
    params
  )
}

lgl_generate_restricted <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n
  result <- NULL

  if (is_col_fk(col_def, schema)) {
    params$values <- get_fkey_vals(col_def, schema, faker_opts)
    result <- do.call(faker_opts$opt_simul_restricted_logical$f_key, params)
    return(result)
  }
  rules <- faker_opts$opt_simul_restricted_logical
  rules$f_key <- NULL

  for (rule in rules) {
    if (!is.null(result)) {
      return(result)
    } else {
      result <- do.call(
        rule,
        params
      )
    }
  }
  return(result)
}

lgl_generate_unrestricted <- function(n, col_def, schema, faker_opts) {
  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  do.call(
    faker_opts$opt_simul_default_fun_logical,
    params
  )
}

lgl_generate <- function(n, col_def, schema, faker_opts) {

  if (!is.null(col_def$spec)) {
    # check if types consistent
    lgl_generate_spec(n, col_def, schema, faker_opts)
  } else if (col_restricted(col_def, schema)) { # same as col dependent
    lgl_generate_restricted(n, col_def, schema, faker_opts)
  } else {
    lgl_generate_unrestricted(n, col_def, schema, faker_opts)
  }

}
