test_that("char_generate_spec fakes special columns correctly", {
  schema <- read_schema("schema-nonlinear.yml")
  options <- default_faker_opts

  # stub(base::sample, "sample.int", function(vec, n) { LETTERS[1:n] }) #nolint
  # set seed temporary until stub bug is fixed
  set.seed(1)
  expect_equal(
    char_generate_spec(3, schema$public$tables$t2$columns$c22, schema, options),
    c("Channing Glover", "Heather Ziemann", "Ballard Wyman")
  )
})

test_that("char_generate_restricted generates values from inset option", {
  schema <- read_schema("schema-simple.yml")
  options <- default_faker_opts

  # params are unified
  expect_true(all(
    char_generate_restricted(2, schema$public$tables$t2$columns$c22, schema, options) %in%
      schema$public$tables$t2$columns$c22$values
  ))

})

test_that("get_fkey_vals properly extracts foreign key values", {
  schema <- read_schema("schema-simple.yml")
  col_def <- schema$public$tables$t1$columns$c2
  options <- default_faker_opts

  mockery::stub(get_fkey_vals, "get_simulated_tbl", function(table_name, schema) data.frame(c21 = letters[1:2]))
  expect_equal(get_fkey_vals(col_def, schema, options), letters[1:2])
})

test_that("char_generate_restricted properly generates foreign key values", {
  schema <- read_schema("schema-simple.yml")
  col_def <- schema$public$tables$t1$columns$c2
  options <- default_faker_opts

  mockery::stub(char_generate_restricted, "get_fkey_vals", function(col_def, schema, faker_opts) letters[1:10])
  faked_col <- char_generate_restricted(2, col_def, schema, options)

  expect_true(all(faked_col %in% letters[1:10]))
})
