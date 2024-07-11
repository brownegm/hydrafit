# function for performing the fitting done in the use example.

fitpx <- function(data, plot=T)

data$to_split <- paste(data$species, data$data.type)

species_list <- unique(data$to_split)

 output.df <-
    data.frame(
      species = as.character(),
      data.type = as.character(),
      A = numeric(),
      B = numeric(),
      C = numeric(),
      D = numeric(),
      loglikeli = numeric(),
      rsq = numeric(),
      slope = numeric(),
      AIC = numeric(),
      AICcorr = numeric(),
      sterror1 = numeric(),
      sterror2 = numeric(),
      sterror3 = numeric(),
      sterror4 = numeric(),
      N = numeric()
    )

for (ii in 1:length(species_list)){
  (subset(data, data$to_split == species_list[ii], select =c(1:dim(data)[2])) -> data_by_sp)
  Linear_fits = fit_linear(data_by_sp, Linear, plot=T, xlab="Water Potential (MPa)", ylab="Leaf Hydraulic Conductance (mmol m^-2 s-1)")
  Linear_fits$D.NA<- NA # placeholder to keep all of the data frames the same size
  Linear_fits$sterror.NA <- NA
  rbind(modelfitting_results_linear, as.data.frame(Linear_fits))-> modelfitting_results_linear
  #cli::cli_progress_update()
}

}
