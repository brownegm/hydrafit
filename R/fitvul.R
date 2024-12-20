#
#
# #data <- hydrafit::scof2012
#
# fitvul <- function (data, mod_type = c("log", "sig", "exp", "exp2"), plot = F, bootstrap = F, silent=T){
#
#   species_list <- unique(data$species)
#   if (length(species_list) == 0) {
#     stop("No species names found in input data.")
#   }else if(silent==F){
#      cat("There are ", length(species_list), "unique species in this dataset.")
#   }
#
#   # Loop through species list and store each subset
#   data_by_species <- lapply(seq_along(species_list),
#                             function(sp) subset(data,
#                                                 species==species_list[sp]))
#
#   run.fit.fx <- ifelse(model_type == "log", Logistic,
#   ifelse(model_type == "exp", Exponential,
#     ifelse(model_type == "exp2", Exponential2,
#       ifelse(model_type == "sig", Sigmoidal, Linear)
#     )
#   )
# )
#
#   par_estimates <- list()
# modelled_fits <- list()
# for (ii in seq_along(species_list)) {
#   define_pars(data_by_species[[ii]], model_type = model_type) -> par_estimates
#   pars <- par_estimates[[1]]
#   par_lo <- par_estimates[[2]]
#   par_high <- par_estimates[[3]]
#
# if(mod_type=="Linear"){
# modelled_fits[ii] = fit_linear(data_by_sp,
#                             plot = plot,
#                             xlab = "Water Potential (MPa)",
#                             ylab = "Leaf Hydraulic Conductance (mmol m^-2 s-1)")
#    linear_fits$D.NA <- NA # placeholder to keep all of the data frames the same size
#    linear_fits$sterror.NA <- NA
# }else{
#      fittedParameters = fit_nonlinear(data_by_sp, parsL, par_loL, par_highL, model_type = "log", plot=T,xlab="Water Potential (MPa)", ylab="Leaf Hydraulic Conductance (mmol m^-2 s-1)")
#      rbind(modelfitting_results_logistic, as.data.frame(Logistic_fits))-> modelfitting_results_logistic
#      }# cli::cli_progress_update()
#    }
# }
#    # if(bootstrap){
#    #   set.seed(1994)
#    #
#    #   #standard error function
#    #   se<-function(x){
#    #     sd(x, na.rm = T)/sqrt(length(x))# specifying na.rm=T because sometimes the predicted parameters estimate negative values and log(negative #)=NA
#    #   }
#    #
#    #   output_boot <-
#    #     data.frame(
#    #       species = character(),
#    #       pred_mean = numeric(),
#    #       pred_median = numeric(),
#    #       pred_se = numeric(),
#    #       conf_low = numeric(),
#    #       conf_high = numeric()
#    #     )
#    #
#    #   alpha=0.05
#    #
#    #   #  For reproducibility
#    #
#    #
#    #   boot_vals_temp<-bootPX(mod_bestfit,
#    #                          fx_type=mod_bestfit[1,"data.type"],
#    #                          px=0.5, psi_max = 0.1)# suppressing warnings for the NAs that are produced
#    #
#    #   boot_vals <- unlist(boot_vals_temp)
#    #
#    #   boot_mean<-mean(boot_vals, na.rm=T)# specifying na.rm=T because sometimes the predicted parameters estimate negative values and log(negative #)=NA
#    #
#    #   boot_se<-se(boot_vals)
#    #
#    #   boot_median<-median(boot_vals,na.rm = T)# specifying na.rm=T because sometimes the predicted parameters estimate negative values and log(negative #)=NA
#    #   #determine confidence intervals
#    #   deg_of_freedom= length(boot_vals)
#    #   t_score=qt(p=alpha/2, df=deg_of_freedom, lower.tail = F)
#    #
#    #   margin_error<-t_score*boot_se
#    #
#    #   conf.low<-mod_bestfit[1,"psi_k50"]-margin_error # using the predicted pX value to make the error make sense
#    #   conf.high<-mod_bestfit[1,"psi_k50"]+margin_error
#    #
#    #   output_boot[1,1]<-mod_bestfit[1,"species"]
#    #   output_boot[1,2]<-boot_mean
#    #   output_boot[1,3]<-boot_median
#    #   output_boot[1,4]<-boot_se
#    #   output_boot[1,5]<-conf.low
#    #   output_boot[1,6]<-conf.high
#    #
#    #
#    #   output_boot
#    #}
#
#
#
