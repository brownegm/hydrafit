#' save_plots
#'
#' Saves the plots from a list of model fits to a file
#'
#' @param fit.list A list of model fits
#' @param file The file string to save the plots to
#' @param force Logical to force the input to be a list of model fits
#' @returns A file containing the plots
#' @export save_plots
#'
#' @importFrom ggplot2 ggsave
#' @importFrom patchwork wrap_plots

save_plots <- function(fit.list, file="output.pdf", force = T) {
  # Check if the input is a list of model fits
  if(force == T){
    if(!is.list(fit.list)){
      stop("The input must be a list of model fits")
    }
    }else if(attr(fit.list, "fit.list")==FALSE){
    stop("The input must be a list of model fits")
  }

  # Load the fitted models
figure_list <- lapply(seq_along(fit.list), function(fit) fit.list[[fit]]$plot)

# Save the plots to a PDF
ggplot2::ggsave(file, patchwork::wrap_plots(figure_list, guides = "collect"))

}
