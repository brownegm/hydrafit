test_that("Data, Species, errors", {

  data <- data.frame(name = LETTERS[1:10], kl = rnorm(10))

  expect_error(fitvul(data),"No species names found in input data.")
})
