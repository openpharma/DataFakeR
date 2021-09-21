test_that("read_schema_file sources file as list", {
  schema <- read_schema_file("schema-simple.yml")
  expect_equal(class(schema), "list")
})

test_that("read_schema_file attached 'schema' atrtibute", {
  schema <- read_schema_file("schema-simple.yml")
  expect_false(is.null(attr(schema, "name")))
  expect_equal(attr(schema, "name"), "public")
})

test_that("read_schema_file doesn't evaluate expressions", {
  schema <- read_schema_file("schema-simple.yml")
  expect_true(
    is.expression(schema$public$tables$t1$check_constraints$t1_c2_check$expression)
  )
})

test_that("find_parent returns schema parents properly", {
  schema <- read_schema_file("schema-simple.yml")

  expect_equal(find_parent(schema$public, schema), NULL)
  expect_equal(find_parent(schema$public$tables$t1, schema[[1]]), NULL)
  expect_equal(find_parent(schema$public$tables$t1$columns$c1, schema), schema[[1]]$tables$t1)
})

test_that("read_schema returns schema obejct properly", {
  schema <- read_schema("schema-simple.yml")

  expect_true(!is.null(attr(schema, "schema-graph")))
})
