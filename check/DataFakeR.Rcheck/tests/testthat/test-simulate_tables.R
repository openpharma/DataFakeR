## R6 approach

test_that("nrows_simul_constant scans list of tables and returns passed value when not existing", {
  schema <- read_schema("schema-simple.yml")
  expect_equal(
    nrows_simul_constant(n = 10)(schema$public$tables),
    list(t1 = 10, t2 = 10)
  )
  schema <- read_schema("schema-nonlinear.yml")
  expect_equal(
    nrows_simul_constant(n = 10)(schema$public$tables),
    list(t1 = 10, t2 = 20, t3 = 10)
  )
})

test_that("simulate_schema preserved defined rules properly", {
  schema <- Schema$new("schema-character.yml")

  simulate_schema_obj(schema)
  generated_tbls <- tibble::as_tibble(schema$get_schema("graph"))
  expect_equal(dim(generated_tbls$table[[1]]), c(10, 6))
  expect_equal(dim(generated_tbls$table[[2]]), c(10, 3))
  expect_equal(generated_tbls$table[[1]]$author, generated_tbls$table[[1]]$author_dupl)
})

test_that("get_simulated_tbl generates values from inset option", {
  schema <- read_schema("schema-simple.yml")

  expect_true(is.data.frame(get_simulated_tbl("t1", schema)))
  expect_equal(attr(get_simulated_tbl("t1", schema), "name"), "t1")
})
