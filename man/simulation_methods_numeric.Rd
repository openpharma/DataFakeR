% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_num_col.R
\name{simulation_methods_numeric}
\alias{simulation_methods_numeric}
\alias{simul_spec_numeric_distr}
\alias{simul_default_numeric}
\alias{simul_restricted_numeric_range}
\alias{simul_restricted_numeric_in_set}
\alias{simul_restricted_numeric_fkey}
\title{Numeric type simulation methods}
\usage{
simul_spec_numeric_distr(
  n,
  not_null,
  unique,
  default,
  spec_params,
  na_ratio,
  levels_ratio,
  ...
)

simul_default_numeric(
  n,
  not_null,
  unique,
  default,
  type,
  na_ratio,
  levels_ratio,
  ...
)

simul_restricted_numeric_range(
  n,
  not_null,
  unique,
  default,
  type,
  range,
  na_ratio,
  levels_ratio,
  ...
)

simul_restricted_numeric_in_set(
  n,
  not_null,
  unique,
  default,
  type,
  values,
  na_ratio,
  levels_ratio,
  ...
)

simul_restricted_numeric_fkey(
  n,
  not_null,
  unique,
  default,
  type,
  values,
  na_ratio,
  levels_ratio,
  ...
)
}
\arguments{
\item{n}{Number of values to simulate.}

\item{not_null}{Should NA values be forbidden?}

\item{unique}{Should duplicated values be allowed?}

\item{default}{Default column value.}

\item{spec_params}{Set of parameters passed to special method.}

\item{na_ratio}{Ratio of NA values (in terms of sample length) the sample should have.}

\item{levels_ratio}{Fraction of levels (in terms of sample length) the sample should have.}

\item{...}{Other parameters passed to column configuration in YAML file.}

\item{type}{Column raw type (sourced from configuration file).}

\item{range}{Possible range of values from which to perform simulation.}

\item{values}{Possible values from which to perform simulation.}
}
\description{
Numeric type simulation methods
}
