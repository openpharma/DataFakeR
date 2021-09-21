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
#  schema <- source_schema(
#    source = conn,
#    schema = "public",
#    faker_opts = set_faker_opts(...)
#  )

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(opt_pull_<type> = opt_pull_<type>(...))

## -----------------------------------------------------------------------------
default_faker_opts$opt_pull_character

## -----------------------------------------------------------------------------
default_faker_opts$opt_pull_integer

## ----eval=FALSE---------------------------------------------------------------
#  my_opts <- set_faker_opts(
#    opt_pull_integer = opt_pull_integer(range = FALSE),
#    opt_pull_character = opt_pull_character(nchar = FALSE)
#  )

