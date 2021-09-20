test_that("dat_generate_unrestricted properly generates date in desired format", {
  schema <- read_schema("schema-date.yml")
  col_def <- schema$public$tables$patients$columns$birth
  options <- default_faker_opts

  faked_col <- dat_generate_unrestricted(2, col_def, schema, options)

  expect_true(all(!is.na(as.Date(faked_col, format = col_def$format))))
})

test_that("dat_generate_restricted properly generates date in desired range", {
  schema <- read_schema("schema-date.yml")
  col_def <- schema$public$tables$patients$columns$treatment
  options <- default_faker_opts

  faked_col <- dat_generate_restricted(2, col_def, schema, options)
  sim_dates <- as.Date(faked_col)
  min_date <- as.Date(col_def$range[1])
  max_date <- as.Date(col_def$range[2])
  expect_true(all(min_date <= sim_dates & sim_dates < max_date))
})
