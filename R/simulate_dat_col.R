#' Date type simulation methods
#'
#' @param n Number of values to simulate.
#' @param not_null Should NA values be forbidden?
#' @param unique Should duplicated values be allowed?
#' @param default Default column value.
#' @param spec_params Set of parameters passed to special method.
#' @param type Column raw type (sourced from configuration file).
#' @param values Possible values from which to perform simulation.
#' @param range,min_date,max_date Date range or minimum and maximum date from which to simulate data.
#' @param format Date format used to store dates.
#' @param na_ratio Ratio of NA values (in terms of sample length) the sample should have.
#' @param levels_ratio Fraction of levels (in terms of sample length) the sample should have.
#' @param ... Other parameters passed to column configuration in YAML file.
#'
#' @name simulation_methods_date
NULL

#' @rdname simulation_methods_date
simul_spec_date_distr <- function(n, not_null, unique, default, spec_params, na_ratio, levels_ratio, ...) {
  call_args <- names(sys.call())
  if (!"spec_params" %in% call_args) {
    stop(glue::glue(
      "{sQuote('distr')} spec method for numerical columns requires {sQuote('spec_params')} defined"
    ))
  }

  method <- spec_params$distr
  spec_params$distr <- NULL
  if (method == "sample") {
    spec_params$size <- n
    n_name <- "size"
  } else {
    spec_params$n <- n
    n_name <- "n"
  }
  unique_sample(
    do.call(method, spec_params),
    method = method, spec_params = spec_params, unique = unique, n_name = n_name
  ) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

#' @rdname simulation_methods_date
#' @export
simul_default_date <- function(n, not_null, unique, default, type, min_date, max_date, format, na_ratio, levels_ratio, ...) {

  # todo handle more params (not_null, unique, default)
  date_seq <- seq(as.Date(min_date, format = format), as.Date(max_date, format = format), by = "day")
  return(
    sample(date_seq, n, replace = !unique) %>%
      levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
  )
}

#' @rdname simulation_methods_date
#' @export
simul_restricted_date_range <- function(n, not_null, unique, default, type, range, format, na_ratio, levels_ratio, ...) {

  if (!missing(range)) {
    date_seq <- seq(as.Date(range[1], format = format), as.Date(range[2], format = format), by = "day")
    sampled_date <- sample(date_seq, n, replace = !unique) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
    return(sampled_date)
  }

  return(NULL)
}

#' @rdname simulation_methods_date
#' @export
simul_restricted_date_fkey <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {
  # todo consider other options if related like distribution in the future
  if (isTRUE(not_null)) {
    values <- values[!is.na(values)]
  }
  if (isTRUE(unique)) {
    warning("Requested to simulate foreign key having unique values. Make sure config is correctly defined.")
  }
  sample(values, n, replace = !unique) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

dat_generate_spec <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  if (!params$spec %in% names(faker_opts$opt_simul_spec_date)) {
    stop(glue::glue(
      "Special method {sQuote(params$spec)} attached for column {sQuote(attr(col_def, 'name'))} is not defined. ",
      "Please check {sQuote('opt_simul_spec_*')} options."
    ))
  }

  do.call(
    faker_opts$opt_simul_spec_date[[col_def$spec]],
    params
  )

}

dat_generate_restricted <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n
  result <- NULL

  if (is_col_fk(col_def, schema)) {
    params$values <- get_fkey_vals(col_def, schema, faker_opts)
    result <- do.call(faker_opts$opt_simul_restricted_date$f_key, params)
    return(result)
  }
  rules <- faker_opts$opt_simul_restricted_date
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

dat_generate_unrestricted <- function(n, col_def, schema, faker_opts) {
  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  do.call(
    faker_opts$opt_simul_default_fun_date,
    params
  )
}

dat_generate <- function(n, col_def, schema, faker_opts) {

  if (!is.null(col_def$spec)) {
    # check if types consistent
    dat_generate_spec(n, col_def, schema, faker_opts)
  } else if (col_restricted(col_def, schema)) { # same as col dependent
    dat_generate_restricted(n, col_def, schema, faker_opts)
  } else {
    dat_generate_unrestricted(n, col_def, schema, faker_opts)
  }

}
