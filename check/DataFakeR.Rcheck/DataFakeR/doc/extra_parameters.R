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
sch <- schema_source(system.file("extdata", "schema-patient.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "patient")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, file = system.file("extdata", "schema-patient_2.yml", package = "DataFakeR"))
sch <- schema_simulate(sch)
schema_get_table(sch, "patient")

## -----------------------------------------------------------------------------
sch <- schema_update_source(sch, file = system.file("extdata", "schema-patient_3.yml", package = "DataFakeR"))
schema_plot_deps(sch, "patient")

## -----------------------------------------------------------------------------
sch <- schema_simulate(sch)
schema_get_table(sch, "patient")

## -----------------------------------------------------------------------------
singular_vals <- function(n, values, singular, ...) {

  if (!missing(singular) && isTRUE(singular)) {
    val <- sample(values, 1)
    return(rep(val, n))
  }

  return(NULL)
}

## -----------------------------------------------------------------------------
my_opts = set_faker_opts(
  opt_simul_restricted_character = opt_simul_restricted_character(
    singular = singular_vals
  )
)

## ----patient_deps-------------------------------------------------------------
sch <- schema_update_source(
  sch, 
  file = system.file("extdata", "schema-patient_4.yml", package = "DataFakeR"),
  my_opts
)
sch <- schema_simulate(sch)
schema_get_table(sch, "patient")

## ----ptntwo_deps--------------------------------------------------------------
dep_sampl <- function(n, group_val, range, ...) {
  print(group_val)
  if (group_val == "M") {
    pmax(pmin(rnorm(n, 0.6, 0.01), range[2]), range[1])
  } else {
    pmax(pmin(rnorm(n, 0.5, 0.01), range[2]), range[1])
  }
}

my_opts = set_faker_opts(
  opt_simul_spec_numeric = opt_simul_spec_numeric(
    dep_sampl = dep_sampl
  ),
  # don't forget option from previous case
  opt_simul_restricted_character = opt_simul_restricted_character(
    singular = singular_vals
  )
)

sch <- schema_update_source(
  sch, 
  file = system.file("extdata", "schema-patient_5.yml", package = "DataFakeR"), 
  my_opts
)
sch <- schema_simulate(sch)
schema_get_table(sch, "patient")


