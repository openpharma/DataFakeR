test_that("Linting package", {

  config <- lintr::with_defaults(
    default = lintr:: default_linters,
    line_length_linter = lintr::line_length_linter(120),
    object_usage_linter = NULL,
    camel_case_linter = NULL,
    object_name_linter = NULL
  )

  #Sys.setenv(NOT_CRAN = "true") # nolint
  lintr::expect_lint_free(
    path = "../../R",
    relative_path = FALSE,
    linters = config
  )
})
