# test_that("bootstrap check ", {
#
#   #standard error function
#   se<-function(x){
#     sd(x, na.rm = T)/sqrt(length(x))# specifying na.rm=T because sometimes the predicted parameters estimate negative values and log(negative #)=NA
#   }
#
#   output_boot <-
#     data.frame(
#       species = character(),
#       pred_mean = numeric(),
#       pred_median = numeric(),
#       pred_se = numeric(),
#       margin_error=numeric(),
#       conf_low = numeric(),
#       conf_high = numeric()
#     )
#
#   alpha=0.05
#
#   set.seed(1994) #  For reproducibility
#
#   test_df <- hydrafit::fit_vuln_curve(input_df = cebe_efm, model_type = "exp2", max_cond_at = 0.1, plot=T)|>as.data.frame()
#
#   for(i in seq_along(test_df)[1]){
#
#     boot_vals_temp <- bootPX(test_df,
#                            model_type=test_df[i,"data.type"],
#                            px=0.5, psi_max = 0.1)# suppressing warnings here can help if errors resulting from NAs stop you from moving forward.
#     boot_vals <- unlist(boot_vals_temp)
#
#     boot_mean<-mean(boot_vals, na.rm=T) # this values *is not * the same are the actual psi at X% loss in hydraulic conductance
#
#     boot_se<-se(boot_vals)
#
#     boot_median<-median(boot_vals,na.rm = T)
#
#     #determine confidence intervals
#     deg_of_freedom= length(boot_vals)
#     t_score=qt(p=alpha/2, df=deg_of_freedom, lower.tail = F)
#
#     margin_error<-t_score*boot_se
#
#     conf.low<-mod_bestfit[i,"psi_k50_at0.1"]-margin_error # using the predicted pX value to make the error make sense
#     conf.high<-mod_bestfit[i,"psi_k50_at0.1"]+margin_error
#
#     output_boot[i,1]<-mod_bestfit[i,"species"]
#     output_boot[i,2]<-boot_mean
#     output_boot[i,3]<-boot_median
#     output_boot[i,4]<-boot_se
#     output_boot[i,5]<-margin_error
#     output_boot[i,6]<-conf.low
#     output_boot[i,7]<-conf.high
#   }
#
#   output_boot
# })
#
