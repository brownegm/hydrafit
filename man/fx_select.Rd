% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_select.R
\name{fx_select}
\alias{fx_select}
\title{Select the best fitting function for each species based on the lowest AICc score.}
\usage{
fx_select(
  linear_fits = NULL,
  logistic_fits = NULL,
  sigmoidal_fits = NULL,
  exp1_fits = NULL,
  exp2_fits = NULL,
  silent = TRUE
)
}
\arguments{
\item{linear_fits}{List of linear fits}

\item{logistic_fits}{List of logistic fits}

\item{sigmoidal_fits}{List of sigmoidal fits}

\item{exp1_fits}{List of first exponential fits}

\item{exp2_fits}{List of second exponential fits}

\item{silent}{Logical. If TRUE, suppresses alternative model output.}
}
\value{
Returns a list with the best fitting (i.e., lowest AICc) models.
}
\description{
Requires that you input all of the fitting parameter data frames from `hydrafit::fit_vuln_curve()`.
}
