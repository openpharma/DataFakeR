## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE,        
  echo = TRUE,         # echo code?
  message = TRUE,     # Show messages
  warning = TRUE,     # Show warnings
  fig.width = 8,       # Default plot width
  fig.height = 6,      # .... height
  dpi = 200,           # Plot resolution
  fig.align = "center"
)
knitr::opts_chunk$set()  # Figure alignment   
library(DataFakeR)
set.seed(123)
options(tibble.width = Inf)

## -----------------------------------------------------------------------------
set.seed(123)
sch <- schema_source(system.file("extdata", "schema-books.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, file = system.file("extdata", "schema-books_2.yml", package = "DataFakeR"))
schema_plot_deps(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, file = system.file("extdata", "schema-books_3.yml", package = "DataFakeR"))
schema_plot_deps(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, file = system.file("extdata", "schema-books_4.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
default_faker_opts$opt_simul_spec_character

## -----------------------------------------------------------------------------
books <- function(n) {
  first <- c("Learning", "Amusing", "Hiding", "Symbols", "Hunting", "Smile")
  second <- c("Of", "On", "With", "From", "In", "Before")
  third <- c("My", "Your", "The", "Common", "Mysterious", "A")
  fourth <- c("Future", "South", "Technology", "Forest", "Storm", "Dreams")
  paste(sample(first, n), sample(second, n), sample(third, n), sample(fourth, n))
}

## -----------------------------------------------------------------------------
books(3)

## -----------------------------------------------------------------------------
books <- function(n, add_second = FALSE) {
  first <- c("Learning", "Amusing", "Hiding", "Symbols", "Hunting", "Smile")
  second <- c("Of", "On", "With", "From", "In", "Before")
  third <- c("My", "Your", "The", "Common", "Mysterious", "A")
  fourth <- c("Future", "South", "Technology", "Forest", "Storm", "Dreams")
  second_res <- NULL
  if (add_second) {
    second_res <- sample(second, n, replace = TRUE)
  }
  paste(
    sample(first, n, replace = TRUE), second_res, 
    sample(third, n, replace = TRUE), sample(fourth, n, replace = TRUE)
  )
}

## -----------------------------------------------------------------------------
simul_spec_character_book <- function(n, unique, spec_params, ...) {
  spec_params$n <- n
  
  DataFakeR::unique_sample(
    do.call(books, spec_params), 
    spec_params = spec_params, unique = unique
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  spec_params$n <- n

## ----eval=FALSE---------------------------------------------------------------
#  DataFakeR::unique_sample(
#    sim_expr = do.call(books, spec_params),
#    spec_params = spec_params, unique = unique
#  )

## -----------------------------------------------------------------------------
my_opts <- set_faker_opts(
  opt_simul_spec_character = opt_simul_spec_character(book = simul_spec_character_book)
)
sch <- schema_source(
  system.file("extdata", "schema-books_5.yml", package = "DataFakeR"), 
  faker_opts = my_opts
)
sch <- schema_simulate(sch)
schema_get_table(sch, "books")


## -----------------------------------------------------------------------------
default_faker_opts$opt_simul_restricted_integer

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(
#    opt_simul_restricted_<column-type> = opt_simul_restricted_<column-type>(my_method = method, ...)
#  )

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, system.file("extdata", "schema-books_6.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
default_faker_opts$opt_simul_restricted_character$in_set

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, system.file("extdata", "schema-books_7.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, system.file("extdata", "schema-books_8.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, system.file("extdata", "schema-books_9.yml", package = "DataFakeR"))
schema_plot_deps(sch)

## -----------------------------------------------------------------------------
sch <- schema_simulate(sch)
schema_get_table(sch, "books")

## -----------------------------------------------------------------------------
unique(schema_get_table(sch, "borrowed")$book_id)

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(
#    opt_simul_default_fun_<column-type> = my_custom_method
#  )

