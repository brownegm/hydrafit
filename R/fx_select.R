#' Select the best fitting function for each species based on the lowest AICc score.
#'
#' @description Requires that you input all of the fitting parameter data frames from `hydrafit::fit_vuln_curve()`.
#'
#' @param linear_fits List of linear fits
#' @param logistic_fits List of logistic fits
#' @param sigmoidal_fits List of sigmoidal fits
#' @param exp1_fits List of first exponential fits
#' @param exp2_fits List of second exponential fits
#' @param quiet Logical. If TRUE, suppresses output.
#' @return Returns a list with the best fitting (i.e., lowest AICc) models.
#'
#' @export fx_select
#'

fx_select <- function(linear_fits=NULL,
                      logistic_fits=NULL,
                      sigmoidal_fits=NULL,
                      exp1_fits=NULL,
                      exp2_fits=NULL,
                      quiet = TRUE) {

  #establish objects
  ## create empty dataframe to store output with names matching input dataframes.
  # Combine all conditions into a list
  all_models <- list(linear_fits,
                             logistic_fits,
                             sigmoidal_fits,
                             exp1_fits,
                             exp2_fits)

  # Find the minimum AIC for each species across all conditions
 lowest_elements <- do.call(mapply, c(function(...) {
    values <- list(...)  # Combine corresponding species across conditions
    # Return element with lowest AIC
    values[[which.min(sapply(values, function(x) x$AICcorr))]]
  }, all_models, SIMPLIFY = FALSE))

 attr(lowest_elements, "fit.list") <- TRUE

  return(lowest_elements)
}

