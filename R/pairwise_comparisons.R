#' Pairwise Bootstrap Comparisons of Species Predictions
#'
#' @param df A data frame containing bootstrap predictions for different species.
#' @param species_col The name of the column containing species names.
#' @param boot_col The name of the column containing bootstrap identifiers.
#' @param pred_col The name of the column containing prediction values.
#' @param conf_level Confidence level for the confidence intervals (default is 0.95).
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select mutate summarise
#' @importFrom purrr map_dfr
#' @import stats
#' @return A data frame with pairwise comparisons between species, including the proportion of times one species has a higher prediction than another, confidence intervals for the differences, and p-values from a binomial test.
#'
#'
pairwise_bootstrap_comparisons <- function(df,
                                           species_col = "species",
                                           boot_col = "boot_id",
                                           pred_col = "prediction",
                                           conf_level = 0.95) {
  # Reshape to wide format: one row per bootstrap, columns for each species
  df_wide <- df %>%
    select(all_of(c(boot_col, species_col, pred_col))) %>%
    pivot_wider(names_from = all_of(species_col), values_from = all_of(pred_col)) %>%
    drop_na()

  # Identify all species combinations
  species <- setdiff(names(df_wide), boot_col)
  combos <- combn(species, 2, simplify = FALSE)

  # Loop through species pairs and compute comparison statistics
  results <- map_dfr(combos, function(pair) {
    x <- df_wide[[pair[1]]]
    y <- df_wide[[pair[2]]]
    diffs <- x - y
    prop_gt <- mean(diffs > 0)
    ci <- quantile(diffs, probs = c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)
    binom_res <- binom.test(sum(diffs > 0), length(diffs), p = 0.5, alternative = "two.sided")

    tibble(
      species1 = pair[1],
      species2 = pair[2],
      prop_species1_gt_species2 = prop_gt,
      ci_lower = ci[1],
      ci_upper = ci[2],
      p_value = binom_res$p.value,
      diffs = list(diffs)  # raw differences stored as a list-column
    )
  })

  return(results)
}
