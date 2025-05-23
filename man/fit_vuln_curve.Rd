% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitvulncurve.R
\name{fit_vuln_curve}
\alias{fit_vuln_curve}
\title{Compute the best fit likelihood parameters for each species for each model type}
\usage{
fit_vuln_curve(
  formula,
  input_df,
  model_type,
  max_cond_at = 0.1,
  pdf = stats::dnorm,
  bootstrap = F,
  plot = F
)
}
\arguments{
\item{formula}{Formula indicating the}

\item{input_df}{input folder with kl and psi values}

\item{model_type}{select appropriate model type here i.e., "Linear" for linear, "sig" for sigmoidal, "exp" and "exp2" for Exponentials and "log" for Logistic. See R/fitfunctions.R for functional types}

\item{max_cond_at}{Water potential which pX should be based upon.}

\item{pdf}{probability density function. Default is dnorm.}

\item{bootstrap}{True or false for bootstrapping of P50 at your chosen max_cond_at}

\item{plot}{True or false for plotting model parameters}
}
\value{
Returns best fitting model parameters for each species for nonlinear fits
}
\description{
Compute the best fit likelihood parameters for each species for each model type
}
\details{
See r/fitfunctions.R for functional types and see R/defineparams.R for parameter definitions.

This function utilizes the `anneal` function of the likelihood package \code{citation('likelihood')}. This function will always return the hydraulic parameters assuming that maximum conductance is at 0 MPa. However, it may be more physiologically relevant for parameters to be estimated at 0.1MPa due to the presence of a driving force for hydraulic conductance. Within this function there are few assumptions about how we expect the annealing and fitting process is run:

\itemize{
\item It looks weird that kl is the x variable here, but anneal calculates the slope and R2 of the fit using the predicted kl values as the y and the observed kl values as the x
\item The parameters are set to change slowly in the fitting procedure (the temp_red variable) helped a lot. You can watch the fitting proceed with show_display. \emph{This option needs to be changed within the function itself. There is no parameter in the function at the moment to do this.}
\item AIC formula: -2LL + 2 x parameters (incl nuisance, i.e.,sd)
\item AICcorr formula: -2LL + (2n x parameters (incl nuisance, i.e.,sd)/(n-parameters-1))
}
}
