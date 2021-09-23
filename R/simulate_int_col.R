#' Integer type simulation methods
#'
#' @param n Number of values to simulate.
#' @param not_null Should NA values be forbidden?
#' @param unique Should duplicated values be allowed?
#' @param default Default column value.
#' @param spec_params Set of parameters passed to special method.
#' @param type Column raw type (sourced from configuration file).
#' @param values Possible values from which to perform simulation.
#' @param range Possible range of values from which to perform simulation.
#' @param na_ratio Ratio of NA values (in terms of sample length) the sample should have.
#' @param levels_ratio Fraction of levels (in terms of sample length) the sample should have.
#' @param ... Other parameters passed to column configuration in YAML file.
#'
#' @name simulation_methods_integer
NULL

#' @rdname simulation_methods_integer
#' @export
simul_spec_integer_distr <- function(n, not_null, unique, default, spec_params, na_ratio, levels_ratio, ...) {
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

#' @rdname simulation_methods_integer
#' @export
simul_default_integer <- function(n, not_null, unique, default, type, na_ratio, levels_ratio, ...) {

  # todo handle more params (not_null, unique, default)
  if (grepl("serial", type)) {
    # todo handle unique
    return(
      1:n
    )
  }

  return(
    unique_sample(
      round(stats::runif(n, integer_ranges[[type]][1], integer_ranges[[type]][2])),
      n = n, type = type, unique = unique
    ) %>%
      levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
  )

}

#' @rdname simulation_methods_integer
#' @export
simul_restricted_integer_range <- function(n, not_null, unique, default, type, range, na_ratio, levels_ratio, ...) {

  if (!missing(range)) {
    return(
      unique_sample(
        round(stats::runif(n, range[1], range[2])),
        range = range, n = n, unique = unique
      ) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  return(NULL)
}

#' @rdname simulation_methods_integer
#' @export
simul_restricted_integer_in_set <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {

  if (!missing(values)) {
    if (isTRUE(not_null)) {
      values <- values[!is.na(values)]
    }
    return(
      sample(values, n, replace = !unique) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  return(NULL)
}

#' @rdname simulation_methods_integer
#' @export
simul_restricted_integer_fkey <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {
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

int_generate_spec <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  if (!params$spec %in% names(faker_opts$opt_simul_spec_integer)) {
    stop(glue::glue(
      "Special method {sQuote(params$spec)} attached for column {sQuote(attr(col_def, 'name'))} is not defined. ",
      "Please check {sQuote('opt_simul_spec_*')} options."
    ))
  }

  do.call(
    faker_opts$opt_simul_spec_integer[[col_def$spec]],
    params
  )

}

int_generate_restricted <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n
  result <- NULL

  if (is_col_fk(col_def, schema)) {
    params$values <- get_fkey_vals(col_def, schema, faker_opts)
    result <- do.call(faker_opts$opt_simul_restricted_integer$f_key, params)
    return(result)
  }
  rules <- faker_opts$opt_simul_restricted_integer
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

int_generate_unrestricted <- function(n, col_def, schema, faker_opts) {
  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  do.call(
    faker_opts$opt_simul_default_fun_integer,
    params
  )
}

int_generate <- function(n, col_def, schema, faker_opts) {

  if (!is.null(col_def$spec)) {
    # check if types consistent
    int_generate_spec(n, col_def, schema, faker_opts)
  } else if (col_restricted(col_def, schema)) { # same as col dependent
    int_generate_restricted(n, col_def, schema, faker_opts)
  } else {
    int_generate_unrestricted(n, col_def, schema, faker_opts)
  }

}
