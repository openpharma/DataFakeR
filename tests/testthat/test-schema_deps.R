test_that("get_fk_dep correctly detects foreign key dependency", {
  schema <- read_schema_file("schema-simple.yml")
  expect_equal(get_fk_dep("t1", schema), list(name = "t1", source = "t2"))
  # no dependency
  expect_equal(get_fk_dep("t2", schema), list(name = "t2"))

  schema <- read_schema_file("schema-nonlinear.yml")
  expect_equal(get_fk_dep("t1", schema), list(name = "t1", source = c("t2", "t3")))
})

test_that("find_table_deps detects incorrect dependencies", {
  schema <- read_schema_file("schema-incorrect_deps.yml")
  expect_error(find_table_deps("t1", schema))
})

test_that("detect_schema_deps correctly detects dependencies", {
  schema <- read_schema_file("schema-simple.yml")
  expect_equal(
    detect_schema_deps(schema),
    list(list(name = "t1", source = "t2"), list(name = "t2"))
  )
})

schema <- read_schema_file("schema-simple.yml")
schema_deps <- detect_schema_deps(schema)

test_that("schema nodes are extracted properly from dependencies", {
  expect_equal(
    deps_to_nodes(schema_deps),
    data.frame(name = c("t1", "t2"))
  )
})

test_that("schema edges are extracted properly from dependencies", {
  expect_equal(
    deps_to_edges(schema_deps),
    data.frame(from = c("t2"), to = c("t1"))
  )
})

schema <- read_schema_file("schema-nonlinear.yml")
schema_deps <- detect_schema_deps(schema)

test_that("schema nodes are extracted properly from non-linear dependencies", {
  expect_equal(
    deps_to_nodes(schema_deps),
    data.frame(name = c("t1", "t2", "t3"))
  )
})

test_that("schema edges are extracted properly from non-linear dependencies", {
  expect_equal(
    deps_to_edges(schema_deps),
    data.frame(from = c("t2", "t3"), to = c("t1", "t1"))
  )
})

test_that("schema_to_graph converts schema to graph", {
  schema <- read_schema_file("schema-nonlinear.yml")

  expect_true("tbl_graph" %in% class(schema_to_graph(schema)))
})
