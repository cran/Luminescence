test_that("Basic test", {
  testthat::skip_on_cran()

  ##cause error
  expect_error(plot_OSLAgeSummary("error"),
               "\\[plot_OSLAgeSummary\\(\\)\\] class character not supported as input for object!")

  ##simple run with example data
  set.seed(1234)
  object <- rnorm(1000, 100, 10)

  ##run as numeric
  SW({
  results <- expect_s4_class(plot_OSLAgeSummary(object), "RLum.Results")
  })

  ##run from S4-class
  object1 <- set_RLum("RLum.Results",
                     data = list(A = object), originator = ".calc_BayesianCentralAgeModel")
  object2 <- set_RLum("RLum.Results",
                      data = list(A = object), originator = ".calc_IndividualAgeModel")

  SW({
  expect_s4_class(plot_OSLAgeSummary(object1), "RLum.Results")
  expect_s4_class(plot_OSLAgeSummary(object2), "RLum.Results")
  })

  ##run with no output
  expect_silent(plot_OSLAgeSummary(object, verbose = FALSE))

  ##run with rug option
  expect_silent(plot_OSLAgeSummary(object, verbose = FALSE, rug = TRUE))

  ##check the results
  expect_length(results, 3)


})
