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
str(default_faker_opts, max.level = 1)

## -----------------------------------------------------------------------------
default_faker_opts$opt_default_character

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(opt_default_table = opt_default_table(nrows = 10))

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(opt_default_table = opt_default_table(nrows = nrows_simul_constant(10)))

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(
#    opt_simul_spec_<column-type> = opt_simul_spec_<column-type>(
#      <spec-method-name> = <spec-function>
#    )
#  )

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(
#    opt_simul_restricted_<column-type> = opt_simul_restricted_<column-type>(
#      <restricted-method-name> = <restricted-function>
#    )
#  )

## ----eval=FALSE---------------------------------------------------------------
#  set_faker_opts(
#    opt_simul_default_fun_<column-type> = <default-function>
#  )

