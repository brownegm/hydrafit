#' Select the best fitting function for each species based on the lowest AICc score.
#'
#' @description Requires that you input all of the fitting parameter data frames from `hydrafit::fit_vuln_curve()`.
#'
#' @param linear_fits dataframe of linear fits
#' @param logistic_fits dataframe of logistic fits
#' @param sigmoidal_fits dataframe of sigmoidal fits
#' @param exp1_fits dataframe of first exponential fits
#' @param exp2_fits dataframe of second exponential fits
#'
#' @return Returns a dataframe with the best fit models. Note: D here is the third parameter for Exp2 or Sigmoidal models.
#' @export fx_select
#'

fx_select <- function(linear_fits,
                      logistic_fits,
                      sigmoidal_fits,
                      exp1_fits,
                      exp2_fits,
                      quiet = TRUE) {

  #establish objects
  ## create empty dataframe to store output with names matching input dataframes.
  # output <- .df_lin|> apply( MARGIN = c(1,2), FUN = \(x) x<-NA)
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

  return(lowest_elements)
}

