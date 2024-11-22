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
#'
#'
#' @param df input data frame containing best fit models, parameter estimates and their SDs
#' @param mod_type Best fitting model
#' @param px Choose what percent loss in K/gs you are solving for PX (e.g., P50, P80 etc)
#' @param sims Number of simulated values to generate
#' @param psi_max estimate px based on what value of psi
#'
#' @return Returns a list of simulations predicted PX values the length of the number `sims`.
#'
#' @importFrom stats rnorm
#' @export bootPX


bootPX<-function(df,
                 mod_type=character(),
                 px=0.5,
                 sims=1000,
                 psi_max=numeric()){

  if(length(psi_max)<1){ stop("Value for psi_max must be provided.")}

  psi_px <- vector()

  #check conditions
  fx_with_param3<-model_type%in%c("exp2", "log", "sig")
  #fx_with_A<-fx_type%in%c("exp2","Linear")

  #define model parameters

  #define A, B and param_3(C or Xo); named 'param_3' because it can be either C or Xo but for the sake of boot not important until output

  A<-df[,"A"]; B<-df[,"B"] #parameter estimates

  A.sd<-df[,"sterrorA"];B.sd<-df[,"sterrorB"]#sd of parameter estimates

  if(fx_with_param3==T){#only create the third parameter if fx_typein fx_with_param3

    param_3<-df[,"C"]
    param_3.sd<-df[,"sterrorC"]

    }

  if(fx_with_param3==F){# linear and exponential

    param_samples <- lapply(c(1:sims), #create 1000 samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, A, A.sd), size=1,replace=T),#sample for A
                                                      sample(rnorm(1000, B, B.sd), size=1,replace=T) #sample for B
                                                      ))

                            })


  }else{#exponential2, logistic, and sigmoidal

    param_samples <- lapply(c(1:sims), #create 1000 samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, A, A.sd), size=1,replace=T),#sample for A
                                                      sample(rnorm(1000, B, B.sd), size=1,replace=T), #sample for B
                                                      sample(rnorm(1000, param_3, param_3.sd), size=1, replace=T)))#sample for Xo or C

                            })

  }

  psi_px_boot <- psiPx(model_type=model_type)

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


