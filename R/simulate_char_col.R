#' Character type simulation methods
#'
#' @param n Number of values to simulate.
#' @param not_null Should NA values be forbidden?
#' @param unique Should duplicated values be allowed?
#' @param default Default column value.
#' @param spec_params Set of parameters passed to special method.
#' @param nchar Maximum number of characters for each value.
#' @param type Column raw type (sourced from configuration file).
#' @param values Possible values from which to perform simulation.
#' @param na_ratio Ratio of NA values (in terms of sample length) the sample should have.
#' @param levels_ratio Fraction of levels (in terms of sample length) the sample should have.
#' @param ... Other parameters passed to column configuration in YAML file.
#'
#' @name simulation_methods_character
NULL

#' @name simulation_methods_character
#' @export
simul_spec_character_name <- function(n, not_null, unique, default, spec_params, na_ratio, levels_ratio, ...) {
  call_args <- names(sys.call())
  if (!"spec_params" %in% call_args) {
    spec_params <- list()
  }
  spec_params$n <- n
  unique_sample(do.call(charlatan::ch_name, spec_params), spec_params = spec_params, unique = unique) %>%
    levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
    na_rand(not_null = not_null, na_ratio = na_ratio)
}

#' @name simulation_methods_character
#' @export
simul_default_character <- function(n, not_null, unique, default, nchar, type, na_ratio, levels_ratio, ...) {

  # todo handle more params (not_null, unique, default)
  if (grepl("var", type) && grepl("[0-9]", type)) {
    nchar <- stringr::str_extract(type, "[0-9]+") %>% as.integer()
    return(
      unique_sample(
        sim_expr = stringi::stri_rand_strings(n, sample(1:nchar, n, replace = TRUE), "[[a-z][A-Z]]"),
        n = n, nchar = nchar, unique = unique
      ) %>%
        levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  if (grepl("char", type) && grepl("[0-9]", type)) {
    nchar <- stringr::str_extract(type, "[0-9]+") %>% as.integer()
    return(
      unique_sample(
        sim_expr = stringi::stri_rand_strings(n, nchar, "[[a-z][A-Z]]"),
        n = n, nchar = nchar, unique = unique
      ) %>%
        levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
        na_rand(not_null = not_null, na_ratio = na_ratio)
    )
  }

  return(
    unique_sample(
      sim_expr = stringi::stri_rand_strings(n, sample(1:nchar, n, replace = TRUE), "[[a-z][A-Z]]"),
      n = n, nchar = nchar, unique = unique
    ) %>%
      levels_rand(unique = unique, levels_ratio = levels_ratio) %>%
      na_rand(not_null = not_null, na_ratio = na_ratio)
  )

}

#' @name simulation_methods_character
#' @export
simul_restricted_character_in_set <- function(n, not_null, unique, default, nchar, type, values, na_ratio, levels_ratio, ...) {

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

#' @name simulation_methods_character
#' @export
simul_restricted_character_fkey <- function(n, not_null, unique, default, nchar, type, values, na_ratio, levels_ratio, ...) {
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

char_generate_spec <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  if (!params$spec %in% names(faker_opts$opt_simul_spec_character)) {
    stop(glue::glue(
      "Special method {sQuote(params$spec)} attached for column {sQuote(attr(col_def, 'name'))} is not defined. ",
      "Please check {sQuote('opt_simul_spec_*')} options."
    ))
  }

  return(
    do.call(
      faker_opts$opt_simul_spec_character[[col_def$spec]],
      params
    )
  )
}

char_generate_restricted <- function(n, col_def, schema, faker_opts) {

  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n
  result <- NULL

  if (is_col_fk(col_def, schema)) {
    params$values <- get_fkey_vals(col_def, schema, faker_opts)
    result <- do.call(faker_opts$opt_simul_restricted_character$f_key, params)
    return(result)
  }
  rules <- faker_opts$opt_simul_restricted_character
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

char_generate_unrestricted <- function(n, col_def, schema, faker_opts) {
  params <- get_col_params(col_def, schema, faker_opts)
  params$n <- n

  do.call(
    faker_opts$opt_simul_default_fun_character,
    params
  )
}

char_generate <- function(n, col_def, schema, faker_opts) {

  if (!is.null(col_def$spec)) {
    # check if types consistent
    char_generate_spec(n, col_def, schema, faker_opts)
  } else if (col_restricted(col_def, schema)) { # same as col dependent
    char_generate_restricted(n, col_def, schema, faker_opts)
  } else {
    char_generate_unrestricted(n, col_def, schema, faker_opts)
  }

}
