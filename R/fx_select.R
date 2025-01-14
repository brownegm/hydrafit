#' Select the best fitting function for each species based on the lowest AICc score.
#'
#' @description Requires that you input all of the fitting parameter data frames from `hydrafit::fit_vuln_curve()`.
#'
#' @param linear_fits List of linear fits
#' @param logistic_fits List of logistic fits
#' @param sigmoidal_fits List of sigmoidal fits
#' @param exp1_fits List of first exponential fits
#' @param exp2_fits List of second exponential fits
#' @param silent Logical. If TRUE, suppresses alternative model output.
#' @return Returns a list with the best fitting (i.e., lowest AICc) models.
#'
#' @export fx_select
#'

fx_select <- function(linear_fits=NULL,
                      logistic_fits=NULL,
                      sigmoidal_fits=NULL,
                      exp1_fits=NULL,
                      exp2_fits=NULL,
                      silent = TRUE) {

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
    #models <- names(values[['data.type']])
    models <- sapply(values, function(x) x$data.type)# Get model names
    # Extract AICcorr values
    (aic_values <- sapply(values, function(x) x$AICcorr))
    min_index <- which.min(aic_values)  # Identify the model with the lowest AIC
    best_model <- values[[min_index]]

      # idenfity the indices of the conditions that are within 2 AIC units of the lowest
    close_indices <- which(abs(aic_values - aic_values[min_index]) <= 2 & seq_along(aic_values) != min_index)

    close_conditions <- models[close_indices]

    # Return element with lowest AIC
    best_model[["models_within_2AIC"]] <- if (length(close_conditions) > 0) close_conditions else NULL
    best_model
    #values[[which.min(sapply(values, function(x) x$AICcorr))]]
  }, all_models, SIMPLIFY = FALSE))

 attr(lowest_elements, "fit.list") <- TRUE


 # Extract models_within_2AIC for each species
 models_with_alternatives <- sapply(seq_along(lowest_elements), function(x) {
   # Get species name
   species_name <- lowest_elements[[x]]$species

   # Get alternative models within 2 AIC
   alternative_models <- lowest_elements[[x]]$models_within_2AIC

   # Return species name and alternatives if any exist
   if (!is.null(alternative_models) && length(alternative_models) > 0) {
     list(species = species_name, alternatives = alternative_models)
   } else {
     NULL  # Skip species with no alternatives
   }
 })

 # Remove NULL entries (species with no alternatives)
 models_with_alternatives <- models_with_alternatives[!sapply(models_with_alternatives, is.null)]

 # Print report for user
if(silent==F){
 if (length(models_with_alternatives) > 0) {
   cat("Note: The following species have alternative models within 2 AICc units:\n")
   for (entry in models_with_alternatives) {
     cat(sprintf("Species: %s, Alternatives: %s\n", entry$species, paste(entry$alternatives, collapse = ", ")))
   }
 } else {
   cat("No models have viable alternatives within 2 AIC units.\n")
 }
}else{

}
  return(lowest_elements)
}

