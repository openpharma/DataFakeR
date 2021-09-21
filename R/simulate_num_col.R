#' Numeric type simulation methods
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
#' @name simulation_methods_numeric
NULL

#' @rdname simulation_methods_numeric
#' @export
simul_spec_numeric_distr <- function(n, not_null, unique, default, spec_params, na_ratio, levels_ratio, ...) {
  call_args <- names(sys.call())
  if (!"spec_params" %in% call_args) {
    stop(glue::glue(
      "{sQuote('distr')} spec method for numerical columns requires {sQuote('spec_params')} defined"
    ))
  }

  method <- spec_params$distr
  spec_params$distr <- NULL
  spec_params$n <- n
  unique_sample(
    do.call(method, spec_params),
    method = method, spec_params = spec_params, unique = unique
  ) %>%
    levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

#' @rdname simulation_methods_numeric
#' @export
simul_default_numeric <- function(n, not_null, unique, default, type, na_ratio, levels_ratio, ...) {

  # todo handle more params (not_null, unique, default)
  if (grepl("decimal|numeric", type) && grepl("[0-9]", type)) {
    num_spec <- stringr::str_extract_all(type, "[0-9]+")[[1]] %>% as.integer()
    precision <- num_spec[1]
    scale <- if (is.na(num_spec[2])) 0 else num_spec[2]
    # todo handle unique
    return(
      unique_sample(
        round(10^(precision - scale) * stats::runif(n), scale),
        precision = precision, scale = scale, n = n, unique = unique
      ) %>%
        levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  params <- list(...)
  precision <- params$precision
  scale <- params$scale
  return(
    unique_sample(
      round(10^(precision - scale) * stats::runif(n), scale),
      precision = precision, scale = scale, n = n, unique = unique
    ) %>%
      levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
  )

}

#' @rdname simulation_methods_numeric
#' @export
simul_restricted_numeric_range <- function(n, not_null, unique, default, type, range, na_ratio, levels_ratio, ...) {

  if (!missing(range)) {
    return(
      unique_sample(
        stats::runif(n, range[1], range[2]),
        range = range, n = n, unique = unique
      ) %>%
        levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  return(NULL)
}

#' @rdname simulation_methods_numeric
#' @export
simul_restricted_numeric_in_set <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {

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

#' @rdname simulation_methods_numeric
#' @export
simul_restricted_numeric_fkey <- function(n, not_null, unique, default, type, values, na_ratio, levels_ratio, ...) {
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

num_generate_spec <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  if (!params$spec %in% names(faker_opts$opt_simul_spec_numeric)) {
    stop(glue::glue(
      "Special method {sQuote(params$spec)} attached for column {sQuote(attr(col_def, 'name'))} is not defined. ",
      "Please check {sQuote('opt_simul_spec_*')} options."
    ))
  }

  do.call(
    faker_opts$opt_simul_spec_numeric[[col_def$spec]],
    params
  )

}

num_generate_restricted <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n
  result <- NULL

  if (is_col_fk(col_def, schema)) {
    params$values <- get_fkey_vals(col_def, schema, faker_opts)
    result <- do.call(faker_opts$opt_simul_restricted_numeric$f_key, params)
    return(result)
  }
  rules <- faker_opts$opt_simul_restricted_numeric
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

num_generate_unrestricted <- function(n, col_def, schema, faker_opts) {
  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  do.call(
    faker_opts$opt_simul_default_fun_numeric,
    params
  )
}

num_generate <- function(n, col_def, schema, faker_opts) {

  if (!is.null(col_def$spec)) {
    # check if types consistent
    num_generate_spec(n, col_def, schema, faker_opts)
  } else if (col_restricted(col_def, schema)) { # same as col dependent
    num_generate_restricted(n, col_def, schema, faker_opts)
  } else {
    num_generate_unrestricted(n, col_def, schema, faker_opts)
  }

}
