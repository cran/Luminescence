test_that("Test the basic plot functionality", {
  testthat::skip_on_cran()

  ## create dataset
  #load Example data
  data(ExampleData.CW_OSL_Curve, envir = environment())
  temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
  temp_NA <- temp
  temp_NA@data[] <- suppressWarnings(NA_real_)

  ## break function
  expect_error(plot_RLum.Data.Curve("temp"), regexp = "Input object is not of type RLum.Data.Curve")

  ## trigger warning
  expect_warning(expect_null(plot_RLum.Data.Curve(temp_NA)),
                 "Curve contains only NA-values, nothing plotted")
  expect_warning(plot_RLum.Data.Curve(set_RLum("RLum.Data.Curve"), norm = TRUE), "Normalisation led to Inf or NaN values. Values replaced by 0")

  ## run function with various conditions
  expect_silent(plot_RLum.Data.Curve(temp))
  expect_silent(plot_RLum.Data.Curve(temp, norm = TRUE))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "max"))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "min"))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "last"))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "huot"))
  expect_silent(plot_RLum.Data.Curve(temp, smooth = TRUE))
  expect_silent(plot_RLum.Data.Curve(temp, par.local = FALSE))

  temp@recordType <- "OSL"
  temp@info <- list(interval = 1)
  expect_silent(plot_RLum.Data.Curve(temp))
  temp@recordType <- "TL"
  temp@info <- list(curveDescripter = "xlab;ylab", RATE = 2)
  expect_silent(plot_RLum.Data.Curve(temp))
})
