% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_causens.R
\name{create_jags_model}
\alias{create_jags_model}
\title{Create an JAGS model for Bayesian sensitivity analysis}
\usage{
create_jags_model()
}
\description{
Creates a JAGS model available as a string, or .txt file, where
priors are initialized to be uninformative by default.

No inputs are given to this function (for now) since data-related information
is provided in jags.model() during model initialization.
}
