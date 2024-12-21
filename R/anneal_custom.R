#' anneal_custom
#'
#' @param input_pdf The name of the model to be passed to the likelihood::anneal function
#' @param ... All arguments to be passed to the likelihood::anneal function
#'
#' @return Resulting annealing model


anneal_custom <- function(input_pdf,...){

    # Use deparse(substitute()) to capture the model name
    #pdf <- paste0(".temp_", deparse(substitute(input_pdf)))

    # Temporarily assign the model to .GlobalEnv with the captured name
    assign("pdf", input_pdf, envir = .GlobalEnv)

    # Ensure cleanup of the temporary object, even if errors occur
    on.exit({
      if (exists("pdf", envir = .GlobalEnv)) {
        rm(list = "pdf", envir = .GlobalEnv)
      }
    }, add = TRUE)

    # Call the original anneal function
    result <- likelihood::anneal(...)

    return(result)
  }

# # Define a dummy model
# dummy_model <- function(x) x^2
#
# # Call the custom anneal function
# anneal_custom(dummy_model)
#
# # Check if the temporary object still exists
# exists(".temp_dummy_model", envir = .GlobalEnv)  # Should return FALS
