


run_fit <- function (data){
# declare the data frame to store the results
modelfitting_results <-
  data.frame(
    Species = as.character(),
    data.type = as.character(),
    A = numeric(),
    B = numeric(),
    C = numeric(),
    D = numeric(),
    loglikeli = numeric(),
    rsq = numeric(),
    slope = numeric(),
    AIC = numeric(),
    AICcorr = numeric(),
    sterror1 = numeric(),
    sterror2 = numeric(),
    sterror3 = numeric(),
    sterror4 = numeric(),
    N = numeric()
  )|>structure()

}
