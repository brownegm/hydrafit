#' Pairwise Bootstrap Comparisons of Species Predictions
#'
#' @param bootstraps A list containing bootstrap predictions for different species based on best fitting model.
#' @param conf_level Confidence level for the confidence intervals (default is 0.95).
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select mutate summarise
#' @importFrom purrr map_dfr
#' @import stats
#' @return A data frame with pairwise comparisons between species, including the proportion of times one species has a higher prediction than another, confidence intervals for the differences, and p-values from a binomial test.
#'
#'
pairwise_bootstrap_comparison <- function(bootstraps,
                                           conf_level = 0.95) {

  # Check if the input is a fit.list
  stopifnot(class(bootstraps) == "list")

  # Get species names
  species_names <- sapply(seq_along(bootstraps),\(bb) bootstraps[[bb]]$species)

  # Create a data frame from the bootstraps
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
