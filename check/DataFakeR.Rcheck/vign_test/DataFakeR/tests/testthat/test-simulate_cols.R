test_that("is_deterministic detects whether column should be generated in deterministic way", {
  schema <- read_schema("schema-simple.yml")
  determ_col_formula <- schema$public$tables$t2$columns$c23
  determ_col_constraint <- schema$public$tables$t2$columns$c24
  undeterm_col <- schema$public$tables$t1$columns$c1
  expect_true(is_deterministic(determ_col_formula, schema))
  expect_true(is_deterministic(determ_col_constraint, schema))
  expect_false(is_deterministic(undeterm_col, schema))
})

test_that("generate_deterministic creates values based on defined constraints and formulas", {
  schema <- read_schema("schema-deterministic.yml")
  c1_col <- 1:3
  attr(schema, "schema-graph") <- attr(schema, "schema-graph") %>%
    tidygraph::mutate(table = list(data.frame(c1 = 1:3)))
  options <- list()

  expect_equal(
    generate_deterministic(3, schema$public$tables$t1$columns$c2, schema, options),
    c1_col + 1.1
  )

  expect_equal(
    generate_deterministic(3, schema$public$tables$t1$columns$c3, schema, options),
    c(20, 20, 20)
  )
})

test_that("fake_column fakes deterministic columns correctly", {
  schema <- read_schema("schema-deterministic.yml")
  c1_col <- 1:3
  attr(schema, "schema-graph") <- attr(schema, "schema-graph") %>%
    tidygraph::mutate(table = list(data.frame(c1 = 1:3)))
  options <- list(opt_default_table = list(nrows = 3))

  expect_equal(
    fake_column(3, schema$public$tables$t1$columns$c2, schema, options),
    c1_col + 1.1
  )
})

test_that("get_col_params correctly rewrites default options", {
  schema <- read_schema("schema-simple.yml")
  options <- default_faker_opts

  # params are unified
  expect_equal(
    names(get_col_params(schema$public$tables$t2$columns$c22, schema, options)),
    c("regexp", "nchar", "not_null", "unique", "default", "na_ratio", "levels_ratio", "type", "values")
  )

  # default not_null == FALSE is rewritten with schema option
  expect_true(
    get_col_params(schema$public$tables$t2$columns$c22, schema, options)$not_null
  )
})

test_that("get_col_params correctly rewrites default options", {
  schema <- read_schema("schema-simple.yml")

  fk_col <- schema$public$tables$t1$columns$c2
  not_fk_col <- schema$public$tables$t1$columns$c1
  # params are unified
  expect_true(
    is_col_fk(fk_col, schema)
  )
  expect_false(
    is_col_fk(not_fk_col, schema)
  )
})

test_that("group_by parameter is supported by all methods", {
  names <- c("Kamil", "Adam", "Krystian", "Pawel", "Adam")
  surnames <- c("Wais", "Forys", "Igras", "Kawski", "Lesniewski")
  faker_opts <- set_faker_opts(
    opt_simul_spec_character = opt_simul_spec_character(nsgroup = function(n, group_val, ...) {
      if (group_val == "Name") {
        sample(names, n, replace = TRUE)
      } else {
        sample(surnames, n, replace = TRUE)
      }
    })
  )
  schema <- Schema$new("schema-grouped.yml", faker_opts = faker_opts)

  from_idx <- schema$get_table("person", "graph") %>% tidygraph::activate(edges) %>% as.data.frame() %>% unlist()
  from_val <- schema$get_table("person", "graph") %>% tidygraph::activate(nodes) %>% as.data.frame()
  # value is dependent on personal_name
  expect_equal(from_val[from_idx, ], c("personal_name", "value"))

  simulate_schema_obj(schema)

  # c4 = paste0(c2, 1:nrow)
  expect_equal(
    schema$get_table("meta", "value")$c4,
    paste(schema$get_table("meta", "value")$c2, 1:10)
  )

  # c3 = paste0(c2, 1:nrow) per c2 group
  a_group <- schema$get_table("meta", "value") %>% dplyr::filter(c2 == "a")
  expect_equal(
    a_group$c3,
    paste(a_group$c2, 1:nrow(a_group))
  )

  # c3 = paste0(c2, 1:nrow) per c2 group
  b_group <- schema$get_table("meta", "value") %>% dplyr::filter(c2 == "b")
  expect_equal(
    b_group$c3,
    paste(b_group$c2, 1:nrow(b_group))
  )

  name_group <- schema$get_table("person", "value") %>% dplyr::filter(personal_name == "Name")
  expect_true(all(
    name_group$value %in% names
  ))

  surname_group <- schema$get_table("person", "value") %>% dplyr::filter(personal_name == "Surname")
  expect_true(all(
    surname_group$value %in% surnames
  ))
})

