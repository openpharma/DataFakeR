test_that("int_generate_spec fakes special columns correctly", {
  schema <- read_schema("schema-integer.yml")
  options <- default_faker_opts

  # stub(base::sample, "sample.int", function(vec, n) { LETTERS[1:n] }) #nolint
  # set seed temporary until stub bug is fixed
  set.seed(1)
  vals <- rbinom(3, 120, 0.5)
  set.seed(1)
  expect_equal(
    int_generate_spec(3, schema$public$tables$patients$columns$height, schema, options),
    vals
  )
})

test_that("int_generate_restricted generates values from inset option", {
  schema <- read_schema("schema-integer.yml")
  options <- default_faker_opts

  # params are unified
  vals <- int_generate_restricted(2, schema$public$tables$patients$columns$weight, schema, options)
  possible_vals <- schema$public$tables$patients$columns$weight$values
  expect_true(all(
    vals %in% possible_vals
  ))

})

test_that("int_generate_restricted properly generates foreign key values", {
  schema <- read_schema("schema-integer.yml")
  col_def <- schema$public$tables$blood_rank$columns$blood
  options <- default_faker_opts

  mockery::stub(int_generate_restricted, "get_fkey_vals", function(col_def, schema, faker_opts) 1:3)
  faked_col <- int_generate_restricted(2, col_def, schema, options)

  expect_true(all(faked_col %in% 1:3))
})

test_that("unrestricted serial type return incremented sequence", {
  schema <- read_schema("schema-integer.yml")
  col_def <- schema$public$tables$patients$columns$id
  options <- default_faker_opts

  faked_col <- int_generate_unrestricted(10, col_def, schema, options)

  expect_equal(faked_col, 1:10)
})
