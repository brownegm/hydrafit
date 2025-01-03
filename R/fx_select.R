#' Select the best fitting function for each species based on the lowest AICc score.
#'
#' @description Requires that you input all of the fitting parameter data frames from `hydrafit::fitlinear()` and `hydrafit::fit_nonlinear()`.
#'
#' @param .df_lin dataframe of linear fits
#' @param .df_log dataframe of logistic fits
#' @param .df_sig dataframe of sigmoidal fits
#' @param .df_exp1 dataframe of first exponential fits
#' @param .df_exp2 dataframe of second exponential fits
#'
#' @return Returns a dataframe with the best fit models. Note: D here is the third parameter for Exp2 or Sigmoidal models.
#' @export fx_select
#'

fx_select <- function(.df_lin,
                      .df_log,
                      .df_sig,
                      .df_exp1,
                      .df_exp2) {

  #establish objects
  ## create empty dataframe to store output with names matching input dataframes.
  output <- .df_lin|> apply( MARGIN = c(1,2), FUN = \(x) x<-NA)

  ## determine number of rows in input dataframes
  rows <- dim(.df_lin)[1]

  ## create list of input dataframes
  dfs <-
    list(.df_lin,
         .df_log,
         .df_sig,
         .df_exp1,
         .df_exp2)

  for(i in 1:rows){
## determine the lowest AICc for each species across each model
    minAIC=min(c(.df_lin[i,"AICcorr"],
                 .df_log[i,"AICcorr"],
                 .df_sig[i,"AICcorr"],
                 .df_exp1[i,"AICcorr"],
                 .df_exp2[i,"AICcorr"]))

    ## select the row from the appropriate df to add to output
    lowestAICc<-lapply(dfs, function(df)

      if(df[[i,"AICcorr"]]==minAIC){

        return(df[i,])

        }else{

        }
    )
    # clean output
    ## lapply creates NULLs in the list; remove here
    lowestAICc[sapply(lowestAICc, is.null)] <- NULL
   ## save row for given species from appropriate df in the output
    output[i,]<-unlist(lowestAICc)#
  }

  return(output)
}
