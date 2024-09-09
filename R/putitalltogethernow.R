


run_fit <- function (data, mod.type){
# declare the data frame to store the results
modelfitting_results <-
  structure(data.frame(
    species = character(),
    mod.type = character(),
    A = numeric(),
    B = numeric(),
    C = numeric(),
    D = numeric(),
    loglikeli = numeric(),
    rsq = numeric(),
    slope = numeric(),
    AIC = numeric(),
    AICcorr = numeric(),
    sterrorA = numeric(),
    sterrorB = numeric(),
    sterrorC = numeric(),
    sterrorD = numeric(),
    N = numeric()
  ),
  class = "modfit",
  model = mod.type)

 species_list <- unique(data$species)
 run.fit.fx <- ifelse(model_type=="log", Logistic,
                               ifelse(model_type=="exp", Exponential,
                                      ifelse(model_type=="exp2",Exponential2,
                                             ifelse(model_type=="sig", Sigmoidal))))
 for (ii in 1:length(species_list)) {

   (data_by_sp <- subset(data,
                      data$to_split == species_list[ii],
                      select = c(1:dim(data)[2]))
    )

   linear_fits = fit_linear(data_by_sp,
                            Linear,
                            plot = T,
                            xlab = "Water Potential (MPa)",
                            ylab = "Leaf Hydraulic Conductance (mmol m^-2 s-1)")
   linear_fits$D.NA <- NA # placeholder to keep all of the data frames the same size
   linear_fits$sterror.NA <- NA

   modelfitting_results_linear -> rbind(modelfitting_results_linear,
                                     as.data.frame(Linear_fits))
   #cli::cli_progress_update()
 }
}
