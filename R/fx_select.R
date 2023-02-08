#' Select the best fitting function for each species based on the lowest AICc score.
#' Requires that you input all of the fitting parameter dataframes from [fitlinear](R/fitlinear.R) and [fit_nonlinear](R/fitnonlinear.R)
#'
#' @param .df_lin dataframe of linear fits
#' @param .df_log dataframe of logistic fits
#' @param .df_sig dataframe of sigmoidal fits
#' @param .df_exp1 dataframe of first exponential fits
#' @param .df_exp2 dataframe of second exponential fits
#'
#' @return returns a dataframe with the best fit models
#' @export fx_select
#'

fx_select<-function(.df_lin=df_lin, .df_log=df_log, .df_sig=df_sig, .df_exp1=df_exp1, .df_exp2=df_exp2){
  #establish objects
  output<-data.frame(Species= as.character(), data.type=as.character(),  A = numeric(), B = numeric(), C = numeric(), D = numeric(),
                     loglikeli = numeric(), rsq = numeric(), slope = numeric(), AIC = numeric(), AICcorr = numeric(),
                     sterror1 = numeric(), sterror2 = numeric(), sterror3 = numeric(), sterror4 = numeric(),
                     N=numeric(), gmax=numeric(),
                     psi_k20=numeric(), psi_k50=numeric(), psi_k80=numeric(), psi_k95=numeric())

  rows<-dim(df_lin)[1]#how many species rows are there
  dfs<-list(.df_lin, .df_log, .df_sig, .df_exp1, .df_exp2)#list of the function objects containing modeled outputs

  for(i in 1:rows){

    minAIC=min(c(.df_lin[i,"AICcorr"], #what's the lowest AICc for each species row for each model
                 .df_log[i,"AICcorr"],
                 .df_sig[i,"AICcorr"],
                 .df_exp1[i,"AICcorr"],
                 .df_exp2[i,"AICcorr"]))

    lowestAICc<-lapply(dfs, function(df) #select the row from the appropriate df to add to output

      if(df[[i,"AICcorr"]]==minAIC){

        return(df[i,])

        }else{

        }
    )
    lowestAICc[sapply(lowestAICc, is.null)] <- NULL#lapply creates NULLs in the list; remove here

    output[i,]<-unlist(lowestAICc)#save row for given species from appropriate df in the output
  }
  return(output)
}
