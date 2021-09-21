test_that("lgl_generate_restricted properly generates foreign key values", {
  schema <- read_schema("schema-logical.yml")
  col_def <- schema$public$tables$from_mocked$columns$mock
  options <- default_faker_opts

  mockery::stub(lgl_generate_restricted, "get_fkey_vals", function(col_def, schema, faker_opts) TRUE)
  faked_col <- lgl_generate_restricted(2, col_def, schema, options)

  expect_true(all(faked_col))
})

test_that("unrestricted boolean types returns logical values", {
  schema <- read_schema("schema-logical.yml")
  col_def <- schema$public$tables$patients$columns$recovered
  options <- default_faker_opts

  faked_col <- lgl_generate_unrestricted(10, col_def, schema, options)

  expect_true(all(faked_col %in% c(TRUE, FALSE)))
})
