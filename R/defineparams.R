#' Define parameter values
#'
#' @param input_df input d with kl and psi values
#'
#' @return Parameters used in setting predicting the best fit parameters
#'
#' @export define_parsE
#' @export define_parsL
#' @export define_parsS
#' @export define_parsE2



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
