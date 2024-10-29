#' Define parameter values
#'
#' @param input_df Input data frame that contains paired conductance (e.g., "kl") and leaf water potential observations
#' @param model_type
#' @return Parameter values to be used in setting predicting the best fit parameters
#'
#' @description Functions used here set initial, lower and upper limits to parameter estimates
#'
#' @details Functions described here define parameters for the models named below:
#'
#' \bold{Model types:}
#'
#' \itemize{
#' \item Exponential
#' \item Exponential2
#' \item Logistic
#' \item Sigmoidal
#' \item Linear
#' }
#'
#' @family internal

#' @rdname define_pars
#' @export define_pars
define_pars <- function(input_df, model_type){

  par_fx <- ifelse(model_type=="log", define_parsL,
                   ifelse(model_type=="exp", define_parsE,
                          ifelse(model_type=="exp2", define_parsE2,
                                 ifelse(model_type=="sig", define_parsS, define_parslin))))

  parameters <- par_fx(input_df)

  return(parameters)

}

#' @rdname define_pars
#' @export define_parsE

define_parsE <- function(input_df) {

  parsE = list(A = max(input_df$kl),
                B = 1,
                sd = 2)

  par_loE = list(A = 0,
                 B = 0.1,
                 sd = 0.0005)

  par_highE = list(A = max(input_df$kl) * 2,
                   B = 10,
                   sd = 20)
  return(list(
    parsE,
    par_loE,
    par_highE
  ))
}


#' @rdname define_pars
#' @export define_parsL
define_parsL <- function(input_df) {

  parsL = list(
    A = max(input_df$kl),
    B = 5,
    Xo = 2,
    sd = 2
  )

  par_loL = list(A = 0,
                 B = 0.1,
                 Xo = 0.2,
                 sd = 0.0005)

  par_highL = list(

    A = max(input_df$kl) * 2,
    B = 25,
    Xo = 5,
    sd = 20

  )
  return(list(
    parsL,
    par_loL,
    par_highL
  ))
}


#' @rdname define_pars
#' @export define_parsS
define_parsS <- function(input_df) {
  parsS <- list(
    A = max(input_df$kl),
    B = -0.5,
    Xo = 2,
    sd = 2
  )
  par_loS = list(
    A = 0,
    B = -1.25,
    Xo = 0.2,
    sd = 0.0005
  )
  par_highS = list(
    A = max(input_df$kl) * 1.5,
    B = 0,
    Xo = 6,
    sd = 20
  )
  return(list(
    parsS,
    par_loS,
    par_highS
  ))
}

#' @rdname define_pars
#' @export define_parsE2
define_parsE2 <- function(input_df) {

  parsE2 <- list(
    A = max(input_df$kl),
    B = 1,
    C = 0.1,
    sd = 2
  )
  par_loE2 = list(A = 0,
                  B = 0.1,
                  C = 0.001,
                  sd = 0.0005)

  par_highE2 = list(
    A = max(input_df$kl) * 2,
    B = 10,
    C = 2,
    sd = 20
  )

  return(list(
    parsE2,
    par_loE2,
    par_highE2
  ))
}

#' @rdname define_pars
#' @export define_parslin
define_parslin<-function(input_df){

lm(input_df$kl ~ input_df$psi)-> fita #a normal linear regression gave the best starting parameters

  pars = list(
    A = summary(fita)$coeff[1, 1],
    B = summary(fita)$coeff[2, 1],
    sd = 1
    )
  par_lo1 = list(
    A = summary(fita)$coeff[1, 1] * 0.1,
    B = summary(fita)$coeff[2, 1] * 2,
    sd = 0.005
  )
  par_hi1 = list(
    A = summary(fita)$coeff[1, 1] * 2,
    B = summary(fita)$coeff[2, 1] * 0.1,
    sd = 20
  )

return(list(pars, par_lo1, par_hi1))

}
