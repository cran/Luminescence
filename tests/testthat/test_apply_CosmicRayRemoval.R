test_that("check function", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.XSYG, envir = environment())

  ##crash the function
  expect_error(
    apply_CosmicRayRemoval("error"),
    regexp = "An object of class 'character' is not supported as input; please read the manual!")
  expect_error(apply_CosmicRayRemoval(TL.Spectrum, method = "error"),
               "Unknown method for cosmic ray removal")

  ##run basic tests
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych"))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth"))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth", MARGIN = 1))
  expect_output(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych", MARGIN = 2, verbose = TRUE, plot = TRUE))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych",
                                       method.Pych.smoothing = 2, method.Pych.threshold_factor = 2))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth.spline",
                                       kind = "3RS3R", twiceit = TRUE,
                                       spar = NULL, MARGIN = 1))

  ##construct objects for different tests
  RLum_list <- list(TL.Spectrum)
  RLum.Analysis <- set_RLum("RLum.Analysis", records = RLum_list)
  RLum.Analysis_list <- list(RLum.Analysis)
  RLum_list_mixed <- list(TL.Spectrum, set_RLum("RLum.Data.Curve"))
  RLum.Analysis_mixed <-  set_RLum("RLum.Analysis", records = RLum_list_mixed )
  RLum.Analysis_mixed_list <- list(RLum.Analysis_mixed)

  ##run tests
  expect_type(apply_CosmicRayRemoval(RLum_list),"list")
  expect_s4_class(apply_CosmicRayRemoval(RLum.Analysis), class = "RLum.Analysis")
  expect_type(apply_CosmicRayRemoval(RLum.Analysis_list), "list")
  expect_error(apply_CosmicRayRemoval(RLum_list_mixed))
  expect_s4_class(apply_CosmicRayRemoval(RLum.Analysis_mixed), class = "RLum.Analysis")
  expect_type(apply_CosmicRayRemoval(RLum.Analysis_mixed_list), "list")
})
