test_that("num_generate_spec fakes special columns correctly", {
  schema <- read_schema("schema-numeric.yml")
  options <- default_faker_opts

  # stub(base::sample, "sample.int", function(vec, n) { LETTERS[1:n] }) #nolint
  # set seed temporary until stub bug is fixed
  set.seed(1)
  vals <- rnorm(3, 65, 5)
  set.seed(1)
  expect_equal(
    num_generate_spec(3, schema$public$tables$patients$columns$weight, schema, options),
    vals
  )
})

test_that("num_generate_restricted generates values from inset option", {
  schema <- read_schema("schema-numeric.yml")
  options <- default_faker_opts

  # params are unified
  vals <- char_generate_restricted(2, schema$public$tables$patients$columns$weight, schema, options)
  range <- schema$public$tables$patients$columns$blood$range
  expect_true(all(
    vals < range[2] & vals > range[1]
  ))

})

test_that("num_generate_restricted properly generates foreign key values", {
  schema <- read_schema("schema-numeric.yml")
  col_def <- schema$public$tables$blood_rank$columns$blood
  options <- default_faker_opts

  mockery::stub(num_generate_restricted, "get_fkey_vals", function(col_def, schema, faker_opts) 1:3)
  faked_col <- num_generate_restricted(2, col_def, schema, options)

  expect_true(all(faked_col %in% 1:3))
})

test_that("unrestricted numeric type uses scale and precision from config", {
  schema <- read_schema("schema-numeric.yml")
  col_def <- schema$public$tables$patients$columns$marker
  options <- default_faker_opts

  set.seed(1)
  faked_col <- num_generate_unrestricted(2, col_def, schema, options)
  precision <- nchar(sub(".", "", faked_col, fixed=TRUE))
  scale <- nchar(sub("\\d+\\.?(.*)$", "\\1", faked_col))

  expect_true(all(precision == options$opt_default_numeric$precision))
  expect_true(all(scale == options$opt_default_numeric$scale))
})
