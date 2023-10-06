#' Function for working hydraulic vulnerability curve data. Tests for
#' outliers via the dixon Q test function.
#'
#' @param data Input data frame
#' @param breaks_by Values to group leaf water potential into bins by. Default breaks values into bins of 0.5 MPa
#' @param test_var Character string. Name of column with the conductance measurement where outlier may exist
#' @param ... Additional parameters to pass to dixon.test function. See ?outliers::dixon.test for more information
#'
#' @return Data frame with the bins of data, the Dixon test alternative hypothesis, test statistic and p-value.
#'
#' @importFrom outliers dixon.test
#' @import dplyr
#'
#'
#'

vul.cur_dixon <- function(data,
                          breaks_by = 0.5,
                          # values to break of psi bins by
                          test_var = character(),
                          # column name of conductance measurement
                          ...) {
  # additional options for dixon test function. See ?dixon.test for options

  df_wbins <- data %>%
    mutate(
      k_bins_numeric = cut(
        psi,
        breaks = seq(
          from = 0,
          to = max(psi) + breaks_by,
          by = breaks_by
        ),
        # break from psi=0 to the max value + 0.5 to cover the driest points.
        include.lowest = TRUE,
        labels = F
      ),
      # instead of factor output vector on bin numbers
      k_bins_factor = cut(
        psi,
        breaks = seq(
          from = 0,
          to = max(psi) + breaks_by,
          by = breaks_by
        ),
        # break from psi=0 to the max value + 0.5 to cover the driest points.
        include.lowest = TRUE,
        ordered = TRUE
      )
    ) %>% # bins as factor and ordered%>%

    group_by(k_bins_numeric) %>%

    mutate(k_bins_new = ifelse(n() < 3, k_bins_numeric + 1, k_bins_numeric)) %>%

    group_by(k_bins_new) %>% # group by the new bins

    mutate(k_bins_new = ifelse(n() < 3, k_bins_new + 1, k_bins_new)) # values themselves are just to categorize the psi values

  stopifnot(df_wbins %>% group_by(k_bins_new) %>% n() < 3)

  dixon_test.list <-
    lapply(unique(df_wbins$k_bins_new), function(bin) {
      test <-
        outliers::dixon.test(x = df_wbins[[test_var]][df_wbins$k_bins_new ==
                                                        bin], ...)#if you want to check the opposite as the chosen value, opposite = T

      bin_asfactor <- df_wbins[df_wbins$k_bins_new == bin, ]$k_bins_factor

      output.df <- data.frame(
        bin_fac = ifelse(
          min(bin_asfactor) == max(bin_asfactor),
          substring(as.character(unique(bin_asfactor)), 2, 6),
          paste(substring(min(bin_asfactor), 2, 2), substring(max(bin_asfactor), 4, 6), sep = ",")
        ),
        # may have a problem if bin is on one that got changed?
        bin_num = bin,
        dqr_hyp = test$alternative,
        dqr_stat = test$statistic,
        dqr_pval = test$p.value,
        row.names = NULL
      ) # end output.df

      return(output.df)
    } # end function
    )

dixon_test.df <- do.call(rbind, dixon_test.list)

return(dixon_test.df)
}

