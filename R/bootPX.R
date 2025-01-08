#' Tool for estimating confidence intervals for percent loss in conductance.
#'
#' @details This function takes the best fit model chosen by `fx_select()` and estimates confidence intervals
#'  by re-sampling model parameters from a normal distribution based on mean and standard deviation. Note that
#'  the standard deviation used here is estimated as the square root of a hessian matrix created when the anneal
#'  functions tests for the best fit model.
#'
#'\itemize{
#'    \item Annealing function info: \url{https://tinyurl.com/annealingsim}
#'    \item Github page: \url{https://github.com/cran/likelihood/blob/master/R/anneal.R}. See lines 742-797 specifically within the code.
#' }

#' @param fit Fit object(s) from `fit_vuln_curve()`
#' @param model_type Best fitting model
#' @param px Percent loss in conductance. Default is 0.5.
#' @param psi_max Value of psi at which to estimate maximum conductance.
#' @param seed Value for reproducibility
#' @param sims Number of simulations to run. Default is 1000.
#'
#' @export bootPX
#'
bootPX <- function(fit, px = 0.5, psi_max, seed = 123, sims = 1000){

  if(attr(fit, "fit.list")==F){
    n_fit <- 1
  }else{
    n_fit <- length(fit)
  }
  px_char <- as.character(px)
  px_est <- switch(px_char,
                   "0.5"=fit$psi_k50,
                   "0.8"=fit$psi_k80)

  #check if the model type is valid
  if(!fit$data.type%in%c("exp2", "log", "sig", "Linear", "exp")){
    stop("Model type must be one of the following: exp2, log, sig, Linear, exp")
  }

  #check if the percent loss in conductance is valid
  if(px>1|px<0){
    stop("Percent loss in conductance must be between 0 and 1")
  }

  #check if the psi_max value is valid
  if(psi_max<0){
    stop("Value for psi_max must be greater than 0")
  }

  # #check if the fit object is a data frame
  # if(!is.data.frame(fit)){
  #   stop("Fit object must be a data frame")
  # }

  # #check if the fit object has the correct columns
  # if(!all(c("species", "A", "B", "C", "sterrorA", "sterrorB", "sterrorC", "data.type", "psi_k50", "psi_k80")%in%names(fit))){
  # missing_names <- which(!names(fit)%in%c("species", "A", "B", "C", "sterrorA", "sterrorB", "sterrorC", "data.type", "psi_k50", "psi_k80"))
  #   stop("Column(s) missing. Fit object must have the following columns: species, A, B, C, sterrorA, sterrorB, sterrorC, data.type, psi_k50.")
  # }

  #check if the psi_k50 value is valid
  if(any(px_est<0)){
    stop("Values for psi_k50 must be greater than 0")
  }

alpha = 0.05

set.seed(seed) #  For reproducibility

if(attr(fit, "fit.list")==F){
  output <- vector(mode = "list", length = n_fit)
}else{
  output <- vector(mode = "list", length = n_fit)
}


for(i in seq_along(n_fit)){

  fit_resample <- resamplePX(fit,
                        px = px, psi_max = psi_max)
  #suppressing warnings here can help if errors resulting from NAs stop you from moving forward.
  finite_values <- sapply(fit_resample, function(x) is.finite(x[[1]]))
  boot_vals <- fit_resample[finite_values]|>unlist()

  boot_mean <- mean(boot_vals, na.rm=T) # this values *is not * the same are the actual psi at X% loss in hydraulic conductance

  boot_se <- se(boot_vals)

  boot_median <- median(boot_vals,na.rm = T)

  #determine confidence intervals
  deg_of_freedom = length(boot_vals)
  t_score = qt(p=alpha/2, df=deg_of_freedom, lower.tail = F)

  margin_error <- t_score*boot_se

  conf.low <- px_est-margin_error # using the predicted pX value to make the error make sense
  conf.high <- px_est+margin_error

# save out of the results
  output[[i]] <- structure(list(species=fit$species,
                                psi_PX = ifelse(px_char=="0.5", "PLC@50%", "PLC@80%" ),
                                boot_mean=boot_mean,
                                boot_median=boot_median,
                                boot_se=boot_se,
                                deg_of_freedom = deg_of_freedom,
                                margin_error=margin_error,
                                conf.low=conf.low,
                                conf.high=conf.high))# class="data.frame"

}

return(output)

}

#' Resample PX
#' @details Resampling is done by sampling from a normal distribution based on the mean and standard deviation of the parameter
#' @param df input data frame containing best fit models, parameter estimates and their SDs
#' @param model_type Best fitting model
#' @param px Choose what percent loss in K/gs you are solving for PX (e.g., P50, P80 etc)
#' @param sims Number of simulated values to generate
#' @param psi_max estimate px based on what value of psi
#' @param seed Seed for reproducibility.
#'
#' @return Returns a list of simulations predicted PX values the length of the number `sims`.
#'
#' @importFrom stats rnorm


resamplePX <- function(fit,
                 #model_type = character(),
                 px=0.5, seed,
                 sims=1000,
                 psi_max = numeric()){

  if(length(psi_max)<1){stop("Value for psi_max must be provided.")}

  if(!fit$data.type%in%c("Linear","exp1", "exp2", "log", "sig")){stop("Model type must be one of the following: Linear, exp1, exp2, log, sig")}

  psi_px <- vector("list", length = sims) #initialize list to store results
  model_type <- fit$data.type

  #check conditions
  fx_with_param3 <- model_type%in%c("exp2", "log", "sig")

  #define model parameters
  A<-fit$A
  B<-fit$B

  #sd of parameter estimates
  A.sd <- fit$sterrorA
  B.sd <- fit$sterrorB

  if(fx_with_param3==F){# linear and exponential

    param_samples <- lapply(c(1:sims), #create X samples of paired values

                            function(x) {
                              lapply(1, function(y)
                                c(
                                  sample(rnorm(1000, A, A.sd), size = 1, replace = T),
                                  #sample for A
                                  sample(rnorm(1000, B, B.sd), size =
                                           1, replace = T) #sample for B
                                ))

                            })

  }else{#exponential2, logistic, and sigmoidal

 # define the third parameter for models with 3 parameters...C or Xo
    param_3<-fit$C
    param_3.sd<-fit$sterrorC

    param_samples <- lapply(c(1:sims), #create X samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, A, A.sd), size=1,replace=T),#sample for A
                                                      sample(rnorm(1000, B, B.sd), size=1,replace=T), #sample for B
                                                      sample(rnorm(1000, param_3, param_3.sd), size=1, replace=T)))#sample for Xo or C

                            })

  }

  psi_px_boot <- psiPx(model_type = model_type)

  if (fx_with_param3==T){

    for(i in 1:sims){# this is a lot to look at!!! Only way to index this list of lists since unlist makes this unusable

        psi_px[[i]] <- psi_px_boot(A=param_samples[[i]][[1]][1],
                              B=param_samples[[i]][[1]][2],
                              C=param_samples[[i]][[1]][3],
                              px = px, max_cond_at = psi_max)$psi.px

    }
      }else{

    for(i in 1:sims){

      psi_px[[i]] <- psi_px_boot(A=param_samples[[i]][[1]][1],
                              B=param_samples[[i]][[1]][2],
                              px = px, max_cond_at= psi_max)$psi.px

      }

    }#end for loop
return(psi_px)
  }


#' Standard Error
#'
#' @param x Input value(s)
#' @param na.rm Remove NA values. Default is TRUE.
#' @returns The standard error of the input value(s).
#' @details na.rm=T by default because the predicted parameters estimate negative values and log(negative number)=NA
#'
se <- function(x, na.rm = T){
  sd(x, na.rm = na.rm)/sqrt(length(x))
}


