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

#' @param fit Fit object from `fit_vuln_curve()`
#' @param model_type Best fitting model
#' @param px Percent loss in conductance. Default is 0.5.
#' @param psi_max Value of psi at which to estimate maximum conductance.
#' @param seed Value for reproducibility
#' @param sims Number of simulations to run. Default is 1000.
#'
#' @export bootPX
#'
bootPX <- function(fit, model_type, px = 0.5, psi_max, seed = 123, sims = 1000){

  #check if the model type is valid
  if(!model_type%in%c("exp2", "log", "sig", "lin", "exp")){
    stop("Model type must be one of the following: exp2, log, sig, lin, exp")
  }

  #check if the percent loss in conductance is valid
  if(px>1|px<0){
    stop("Percent loss in conductance must be between 0 and 1")
  }

  #check if the psi_max value is valid
  if(psi_max<0){
    stop("Value for psi_max must be greater than 0")
  }

  #check if the fit object is a data frame
  if(!is.data.frame(fit)){
    stop("Fit object must be a data frame")
  }

  #check if the fit object has the correct columns
  if(!all(c("species", "A", "B", "C", "sterrorA", "sterrorB", "sterrorC", "data.type", "psi_k50")%in%colnames(fit))){
    stop("Fit object must have the following columns: species, A, B, C, sterrorA, sterrorB, sterrorC, data.type, psi_k50")
  }

  #check if the psi_k50 value is valid
  if(any(fit$psi_k50<0)){
    stop("Values for psi_k50 must be greater than 0")
  }

alpha = 0.05

set.seed(seed) #  For reproducibility

for(i in 1:dim(fit)[1]){

  boot_vals_tmp<-resamplePX(mod_bestfit,
                        fx_type=mod_bestfit[i,"data.type"],
                        px=px, psi_max = psi_max)%>%unlist()# suppressing warnings here can help if errors resulting from NAs stop you from moving forward.
  boot_vals <- is.finite(boot_vals_tmp)

  boot_mean <- mean(boot_vals, na.rm=T) # this values *is not * the same are the actual psi at X% loss in hydraulic conductance

  boot_se <- se(boot_vals)

  boot_median <- median(boot_vals,na.rm = T)

  #determine confidence intervals
  deg_of_freedom = length(boot_vals)
  t_score = qt(p=alpha/2, df=deg_of_freedom, lower.tail = F)

  margin_error <- t_score*boot_se

  conf.low<-mod_bestfit[i,"psi_k50"]-margin_error # using the predicted pX value to make the error make sense
  conf.high<-mod_bestfit[i,"psi_k50"]+margin_error

# save out of the results
  output_boot <- structure(list(species=fit$species,
                                psi_PX = numeric(),
                                boot_mean=boot_mean,
                                boot_median=boot_median,
                                boot_se=boot_se,
                                margin_error=margin_error,
                                conf.low=conf.low,
                                conf.high=conf.high), class="data.frame")

}

output_boot

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
                 model_type = character(),
                 px=0.5, seed,
                 sims=1000,
                 psi_max = numeric()){

  if(length(psi_max)<1){stop("Value for psi_max must be provided.")}

  psi_px <- vector()

  #check conditions
  fx_with_param3 <- model_type%in%c("exp2", "log", "sig")

  #define model parameters
  A<-df[,"A"]; B<-df[,"B"] #parameter estimates

  A.sd<-df[,"sterrorA"];B.sd<-df[,"sterrorB"]#sd of parameter estimates

  if(fx_with_param3==F){# linear and exponential

    param_samples <- lapply(c(1:sims), #create X samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, A, A.sd), size=1,replace=T),#sample for A
                                                      sample(rnorm(1000, B, B.sd), size=1,replace=T) #sample for B
                                                      ))

                            })

  }else{#exponential2, logistic, and sigmoidal
 # define the third parameter for models with 3 parameters...C or Xo
    param_3<-df[,"C"]
    param_3.sd<-df[,"sterrorC"]

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


        psi_px[i] <- psi_px_boot(A=param_samples[[i]][[1]][1],
                              B=param_samples[[i]][[1]][2],
                              C=param_samples[[i]][[1]][3],
                              px = px, max_cond_at = psi_max)

    }
      }else{

    for(i in 1:sims){

      psi_px[i] <- psi_px_boot(A=param_samples[[i]][[1]][1],
                              B=param_samples[[i]][[1]][2],
                              px = px, max_cond_at= psi_max)

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


