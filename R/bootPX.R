#' Bootstrap percent loss in conductance
#'
#' @param df input data frame containing best fit models, parameter estimates and their SDs
#' @param fx_type what model type fit the best
#' @param px choose what percent loss you are solving for PX e.g., P50, P80 etc
#' @param sims #how many samples to generate?
#'
#' @return returns a list of sims length predicted PX values
#' @export
#'
bootPX<-function(df,
                 fx_type=character(),
                 px=0.5,
                 sims=1000){

  psi_px<-list()

  #check conditions
  fx_with_param3<-fx_type%in%c("Exponential2", "Logistic", "Sigmoidal")
  fx_with_A<-fx_type%in%c("Exponential2","Linear")

  #define model parameters

  #define A, B and param_3(C or Xo); named 'param_3' because it can be either but for the sake of boot not important until output maybe

  A<-df[,"A"]; B<-df[,"B"] #parameter estimates

  A.sd<-df[,"sterror1"];B.sd<-df[,"sterror2"]#sd of parameter estimates

  if(fx_with_param3==T){#only create the third parameter if test_fx_type==TRUE

    param_3<-df[,"C"]
    param_3.sd<-df[,"sterror3"]

    }

  if(fx_with_param3==T & fx_with_A==T){#exponential2

    param_samples <- lapply(c(1:sims), #create 1000 samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, A, A.sd), size=1,replace=T),#sample for A
                                                      sample(rnorm(1000, B, B.sd), size=1,replace=T), #sample for B
                                                      sample(rnorm(1000, param_3, param_3.sd), size=1,replace=T)))#sample for Xo

                            })

  }else if( fx_with_param3==T & fx_with_A==F){#logistic, sigmoidal

    param_samples <- lapply(c(1:sims), #create 1000 samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, B, B.sd), size=1,replace=T), #sample for B
                                                      sample(rnorm(1000, param_3, param_3.sd), size=1,replace=T)))#sample for Xo

                            })
  }else if(fx_with_param3==F & fx_with_A==F){#this is the case for exponential 1

    param_samples <- lapply(c(1:sims), #create 1000 samples of paired values

                            function(x){

                              lapply(1, function(y) c(sample(rnorm(1000, B, B.sd), size=1,replace=T))) #sample for B

                            })
  }

    for(i in 1:sims){# this is a lot to look at!!! Only way to index this list of lists since unlist makes this unusable

      #Sample parameters and bootstrap PX(P50 here)
      if(fx_type=="Linear"){

        cat("Check data. Linear is rarely the best fit.")
        ## df$psi_k50<- (px-1)*(-df$A.A)*-1/(df$B.B)

      }else if(fx_type=="Exponential"){

        psi_px[i]<- log(px)/(-param_samples[[i]][[1]][1])

      }else if (fx_type=="Exponential2"){

        psi_px[i]<- log(((px)*(-param_samples[[i]][[1]][3]+param_samples[[i]][[1]][1]))/param_samples[[i]][[1]][1])/(-param_samples[[i]][[1]][2])

      }else if (fx_type=="Logistic"){

        psi_px[i]<- param_samples[[i]][[1]][2]*(1/px-1)^(1/param_samples[[i]][[1]][1])

      }else{#if sigmoidal

        psi_px[i]<- param_samples[[i]][[1]][2] - param_samples[[i]][[1]][1]*log(1/px -1)
      }#end else

    }#end for loop
return(psi_px)
  }


