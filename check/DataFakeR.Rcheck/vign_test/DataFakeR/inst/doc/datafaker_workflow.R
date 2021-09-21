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

## ----eval=FALSE---------------------------------------------------------------
#  schema <- schema_source(
#    source = <db-connection-object>,
#    schema_name = <target-schema-name>,
#    file = <target-schema-configuration-file>
#  )

## ----eval = FALSE-------------------------------------------------------------
#  schema <- schema_source(
#    source = <named-list-of-tables>,
#    schema_name = <target-schema-name>,
#    file = <target-schema-configuration-file>
#  )

## ----eval=FALSE---------------------------------------------------------------
#  schema <- schema_source(
#    source = <source-schema-configuration-file>
#  )

## ----eval=FALSE---------------------------------------------------------------
#  schema <- schema_simulate(schema)

## ----eval=FALSE---------------------------------------------------------------
#  table_a <- schema_get_table(schema, <table-name>)

## ----eval=FALSE---------------------------------------------------------------
#  schema_plot_deps(schema)

## ----eval=FALSE---------------------------------------------------------------
#  schema_plot_deps(schema, table_name = <table-name>)

## ----eval=FALSE---------------------------------------------------------------
#  schema <- schema_update_source(
#    schema,
#    file = <new-version-source-file>,
#    faker_opts = <new-simulation-options>
#  )

