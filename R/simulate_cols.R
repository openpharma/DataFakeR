constraint_col_related <- function(col_name, constraint) {

  if (!is.null(constraint$column) && col_name %in% constraint$column) {
    return(TRUE)
  }
  col_name %in% all.vars(constraint$expression)[1]
}


is_deterministic <- function(col_def, schema) {

  if (!is.null(col_def$formula)) {
    return(TRUE)
  }

  check_constraints <- find_parent(col_def, schema)$check_constraints
  if (is.null(check_constraints)) {
    return(FALSE)
  }

  col_name <- attr(col_def, "name")
  col_constraints <- check_constraints %>%
    purrr::keep(~ constraint_col_related(col_name, .x)) %>%
    purrr::map("expression")

  # check if column follow equal constraints
  col_constraints %>% purrr::map_lgl(~ "==" %in% all.names(.x)) %>% any()
}

get_column_type <- function(col_def, faker_opts) {

  if (is.null(col_def$type)) {
    stop(glue::glue("Column {attr(col_def, 'name')} doesn't have type defined."))
  }

  # character
  if (grepl(faker_opts$opt_default_character$regexp, col_def$type)) {
    return("character")
  }

  # numeric
  if (grepl(faker_opts$opt_default_numeric$regexp, col_def$type)) {
    return("numeric")
  }

  # integer
  if (grepl(faker_opts$opt_default_integer$regexp, col_def$type)) {
    return("integer")
  }

  # logical
  if (grepl(faker_opts$opt_default_logical$regexp, col_def$type)) {
    return("logical")
  }

  # date
  if (grepl(faker_opts$opt_default_date$regexp, col_def$type)) {
    return("date")
  }

  stop(glue::glue("Column type {col_def$type} of column {attr(col_def, 'name')} was not recognized."))
}

get_type_generator <- function(column_type) {
  switch(
    column_type,
    "character" = char_generate, #nolint
    "numeric" = num_generate,#nolint
    "integer" = int_generate,#nolint
    "logical" = lgl_generate,#nolint
    "date" = dat_generate#,#nolint
    # "time" = time_generator#nolint
  )
}

get_type_converter <- function(column_type) {
  switch(
    column_type,
    "character" = as.character, #nolint
    "numeric" = as.numeric,#nolint
    "integer" = as.integer,#nolint
    "logical" = as.logical,#nolint
    "date" = as.Date#,#nolint
    # "time" = time_generator#nolint
  )
}

is_col_fk <- function(col_def, schema) {
  table <- find_parent(col_def, schema)
  col_name <- attr(col_def, "name")
  fk_cols <- purrr::map_chr(table$foreign_keys, ~ .$columns)

  return(col_name %in% fk_cols)
}

get_fkey_vals <- function(col_def, schema, faker_opts) {
  col_name <- attr(col_def, "name")
  parent_col <- find_parent(col_def, schema)$foreign_keys %>%
    purrr::keep(~ .x$columns == col_name) %>%
    purrr::map(~ .x$references) %>%
    dplyr::first()
  key_values <- get_simulated_tbl(parent_col$table, schema) %>%
    dplyr::pull(!!parent_col$columns) %>%
    unique()

  key_values
}

col_restricted <- function(col_def, schema) {

  # is limited by values or range?
  if ("values" %in% names(col_def) || "range" %in% names(col_def)) {
    return(TRUE)
  }

  # is limited as foreign key?
  if (is_col_fk(col_def, schema)) {
    return(TRUE)
  }

  # is restricted by constraint and not deterministic (no '==' in constraint)?
  check_constraints <- find_parent(col_def, schema)$check_constraints
  if (is.null(check_constraints)) {
    return(FALSE)
  }

  col_name <- attr(col_def, "name")
  col_constraints <- check_constraints %>%
    purrr::keep(~ constraint_col_related(col_name, .x)) %>%
    purrr::map("expression") %>%
    purrr::map_lgl(~ any(c("<", ">", "in", "between") %in% all.names(.x)))

  if (any(col_constraints)) {
    return(TRUE)
  }

  return(FALSE)

}

prepare_expression <- function(expr) {

  # make sure can be evaluated
  # for example c2 == 1 -> c2 = 1, 10 == c2 -> c2 = 10
  # todo extend to left side
  # todo handle multiple

  if (identical(expr, as.name("=="))) {
    expr <- as.name("=")
    return(expr)
  }

  if (length(expr) == 1) {
    return(expr)
  }

  expr[] <- lapply(expr, prepare_expression)
  expr
}

extract_col_expression <- function(col_def, schema) {
  if (!is.null(col_def$formula)) {
    return(col_def$formula)
  }

  check_constraints <- find_parent(col_def, schema)$check_constraints
  if (is.null(check_constraints)) {
    return(NULL)
  }

  col_name <- attr(col_def, "name")
  col_constraints <- check_constraints %>%
    purrr::keep(~ constraint_col_related(col_name, .x)) %>%
    purrr::map("expression")

  # check if column follow equal constraints
  col_constraints <- col_constraints %>% purrr::keep(~ "==" %in% all.names(.x))

  if (length(col_constraints) == 1 && is.list(col_constraints)) {
    return(col_constraints[[1]])
  }

  col_constraints
}

generate_deterministic <- function(n, col_def, schema, faker_opts) {

  col_expr <- extract_col_expression(col_def, schema)

  # todo handle more than one
  col_expr <- prepare_expression(col_expr[[1]]) # expression[[1]] is quote that we need
  simul_table <- get_simulated_tbl(attr(col_def, "parent"), schema)

  if (!is.null(col_def$group_by)) {
    simul_table <- simul_table %>%
      dplyr::group_by(!!dplyr::sym(col_def$group_by))
  }

  simul_table %>%
    dplyr::mutate(val = !!col_expr) %>%
    dplyr::pull(val)
}

is_col_pk <- function(col_def, schema) {
  attr(col_def, "name") %in% find_parent(col_def, schema)$primary_key[[1]]$columns
}

get_col_params <- function(col_def, schema, faker_opts) {

  type <- get_column_type(col_def, faker_opts)
  default_opts <- switch(
    type,
    "character" = faker_opts$opt_default_character,
    "numeric" = faker_opts$opt_default_numeric,
    "integer" = faker_opts$opt_default_integer,
    "logical" = faker_opts$opt_default_logical,
    "date" = faker_opts$opt_default_date
  )

  if (is_col_pk(col_def, schema)) {
    if (!isTRUE(col_def$unique)) {
      warning(glue::glue(
        "Column {attr(col_def, 'name')} is primary key but have unique = false specified.",
        "The parameter will be overwritten."
      ))
      col_def$unique <- TRUE
    }
    if (!isTRUE(col_def$unique)) {
      warning(glue::glue(
        "Column {attr(col_def, 'name')} is primary key but have not_null = false specified.",
        "The parameter will be overwritten."
      ))
      col_def$not_null <- TRUE
    }
  }

  utils::modifyList(default_opts, col_def)
}

#' Schema graph_tbl with already faked data (if performed)
#'
#' @param n Number of values to simulate
#' @param col_def Column definition.
#' @param schema Schema object.
#' @param faker_opts Column simulation faker_opts.
#' @keywords internal
fake_column <- function(n, col_def, schema, faker_opts) {
  if (is_deterministic(col_def, schema)) {
    # Col deterministic, no other rules can be applied
    return(generate_deterministic(n, col_def, schema, faker_opts))
  }

  column_type <- get_column_type(col_def, faker_opts)
  type_converter <- get_type_converter(column_type)
  type_generator <- get_type_generator(column_type)

  if (is.null(col_def$group_by)) {
    return(type_converter(type_generator(n, col_def, schema, faker_opts)))
  } else {
    call_fun <- function(n, group_val, schema, options) {
      col_def$group_val <- group_val[1, 1, drop = TRUE]
      type_generator(n, col_def, schema, faker_opts)
    }

    return(
      get_simulated_tbl(attr(col_def, "parent"), schema) %>%
        dplyr::group_by(!!dplyr::sym(col_def$group_by)) %>%
        dplyr::mutate(val = call_fun(dplyr::n(), dplyr::cur_group(), schema, faker_opts)) %>%
        dplyr::pull(val) %>%
        type_converter()
    )
  }
}

#' Modify sample with desired condition
#'
#' The set of function that allows to perform most common operations ion data sample.
#'
#' \code{unique_sample} - takes simulation expression and assures the expression will be executed as many times as needed to return unique result sample.
#' \code{na_rand} - attaches NA values to the sample according to provided NA's ratio.
#' \code{levels_rand} - takes provided number of sample levels, and assures the returned sample have as many levels as requested.
#'
#' @param sim_expr Expression to be evaluated in order to get column sample.
#' @param ... Parameters and their values that are used in \code{sim_expr}.
#' @param unique If TRUE the function will try to simulate unique values.
#' @param n_name Name of the parameter providing sample length (for example 'n' for \code{rnorm} and 'size' for \code{sample}).
#' @param n_iter Number of iteration to make to assure the returned values are unique.
#' @param na_ratio Ratio (in terms of column length) of NA values to attach to the sample.
#' @param not_null Information whether NA's are allowed.
#' @param levels_ratio Ratio of unique levels in terms of whole sample length.
#' @param sample_vec Vector to which NA values should be injected.
#'
#' @examples
#'
#' unique_sample(rnorm(n, mean = my_mean), n = 10, my_mean = 2)
#' unique_sample(sample(values, size, replace = TRUE), size = 10, values = 1:10, n_name = "size")
#'
#' \dontrun{
#'   ## In 10 iterations it was not possible to simulate 6 unique values from the vector 1:5
#'   unique_sample(sample(values, size, replace = TRUE), size = 6, values = 1:5, n_name = "size")
#' }
#'
#' na_rand(1:10, na_ratio = 0.5)
#'
#' @name sample_modifiers

#' @rdname sample_modifiers
#' @export
unique_sample <- function(sim_expr, ..., unique = TRUE, n_name = "n", n_iter = 10) {
  params <- list(...)
  result <- eval(substitute(sim_expr), list(...))
  if (unique) {
    iter <- 1
    while(any(duplicated(result)) && iter < n_iter) {
      iter <- iter + 1
      params[[n_name]] <- length(which(duplicated(result)))
      result[which(duplicated(result))] <- eval(substitute(sim_expr), params)
    }
    if (any(duplicated(result))) {
      stop("It wasn't possible to generate the unique sample. Please make sure sampling configuration is correct.")
    }
  }
  result
}

#' @rdname sample_modifiers
#' @export
na_rand <- function(sample_vec, na_ratio, not_null = FALSE) {
  if (not_null) {
    return(sample_vec)
  }
  na_idxs <- sample(1:length(sample_vec), round(na_ratio * length(sample_vec)))
  sample_vec[na_idxs] <- NA

  return(sample_vec)
}

#' @rdname sample_modifiers
#' @export
levels_rand <- function(sample_vec, levels_ratio, unique) {
  if (unique || levels_ratio == 1) {
    return(sample_vec)
  }
  n_levels <- round(levels_ratio * length(sample_vec))
  chosen_levels <- sample(unique(sample_vec), min(n_levels, length(unique(sample_vec))), replace = FALSE)

  sample(chosen_levels, length(sample_vec), replace = TRUE)
}
