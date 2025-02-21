## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----init---------------------------------------------------------------------
library(Luminescence)

## ----simulate_signal, fig.dim = c(8, 6), out.width="800px"--------------------

vn_simulated <- sample(x = c(rnorm(n = 30, mean = 2000, sd = 500),
                             rnorm(n = 70, mean = 20, sd = 1)),
                       size = 100)

vn_simulated <- round(vn_simulated) # Because photons are discrete

head(vn_simulated, n = 25)

hist(vn_simulated,
     main = "Simulated signal (histogram)",
     xlab = "Photon counts",
     ylab = "Frequency",
     breaks = 30
     )

## ----show_disc_basic, fig.dim = c(7, 7), out.width="800px"--------------------

par(mar = c(1, 4, 6, 4))

plot_SingleGrainDisc(object = vn_simulated,
          main = "Simulated signal (measurement disc)"
          )


## ----calculate_moransI--------------------------------------------------------

calc_MoransI(object = vn_simulated)

calc_MoransI(object = vn_simulated, compute_pseudo_p = TRUE)



## ----add_crosstalk, fig.dim = c(8, 6), out.width="800px"----------------------

vn_simulated_with_crosstalk <- apply_Crosstalk(object = vn_simulated,
                                              n_crosstalk = 0.10)

vn_simulated_with_crosstalk <- round(vn_simulated_with_crosstalk)

hist(vn_simulated_with_crosstalk,
     main = "Simulated signal with crosstalk (histogram)",
     xlab = "Photon counts",
     ylab = "Frequency",
     breaks = 30
     )

plot_SingleGrainDisc(object = vn_simulated_with_crosstalk,
          main = "Simulated signal with crosstalk (measurement disc)")

calc_MoransI(object = vn_simulated_with_crosstalk)

calc_MoransI(object = vn_simulated_with_crosstalk, compute_pseudo_p = TRUE)



## ----test_different_amounts, fig.dim = c(8, 6), out.width="800px"-------------


df_MoransI <- data.frame(crosstalk = seq(from = 0,
                    to = 0.30,
                    length.out=50))

df_MoransI$MoransI <- NA
df_MoransI$pseudo_p <- NA

old.opts <- options(warn = -1) # silence warnings from compute_pseudo_p
for (i in 1:nrow(df_MoransI))
{

  vn_simulated_with_crosstalk <- apply_Crosstalk(object = vn_simulated,
                                              n_crosstalk = df_MoransI$crosstalk[i])

  df_MoransI$MoransI[i]  <- calc_MoransI(object = vn_simulated_with_crosstalk)
  df_MoransI$pseudo_p[i] <-
    calc_MoransI(object = vn_simulated_with_crosstalk, compute_pseudo_p = TRUE)
}
options(old.opts) # restore the default options

n_expected_I_no_spatial_autocorr <- calc_MoransI(1:100,
                                                 spatial_autocorrelation = FALSE)

##

plot(x = df_MoransI$crosstalk,
     y = df_MoransI$MoransI,
     ylim = range(
          pretty(x = c(df_MoransI$MoransI, n_expected_I_no_spatial_autocorr))
          ),
     ## Set ylim manually to make sure the value for I for no crosstalk is visible
     xlab = "Amount of added crosstalk",
     ylab = "Calculated Moran's I"
     )
graphics::grid()
abline(h = n_expected_I_no_spatial_autocorr,
       col = "purple")

legend(x = "topleft",
       legend = "Expected I if no spatial autocorrelation",
       lty = "solid",
       col = "purple",
       cex = 0.8)

plot(x = df_MoransI$crosstalk,
     y = df_MoransI$pseudo_p,
     xlab = "Amount of added crosstalk",
     ylab = "Generated pseudo-p of related Moran's I")
graphics::grid()

## ----show_`disc_extended_moran_scatterplot, fig.dim = c(8, 5), out.width="800px"----


plot_MoranScatterplot(object = vn_simulated,
           main = "Moran scatterplot, simulated signal without crosstalk")

vn_simulated_with_crosstalk <- apply_Crosstalk(object = vn_simulated,
                                              n_crosstalk = 0.25)
vn_simulated_with_crosstalk <- round(vn_simulated_with_crosstalk)

plot_MoranScatterplot(object = vn_simulated_with_crosstalk,
           main = "Moran scatterplot, simulated signal with added crosstalk")

## ----adjacent_grain_locations, fig.dim = c(8, 5), out.width="800px"-----------

    vn_simulated_with_holes <- c(rnorm(30, mean = 10, sd = 5),
                              rnorm(30, mean = 500, sd = 50),
                              rep(NA, times = 40)
                              )

df_Neighbours <- Luminescence:::.get_Neighbours(object = vn_simulated_with_holes)

head(df_Neighbours)

## ----adjacent_grain_locations_show_disc, fig.dim = c(8, 5), out.width="800px"----

    plot_SingleGrainDisc(object = vn_simulated_with_holes,
              show_neighbours = TRUE,
              show_location_ids = TRUE)

## ----adjacent_grain_locations_add, fig.dim = c(8, 5), out.width="800px"-------

df_Neighbours_with_diag <- Luminescence:::.get_Neighbours(object = vn_simulated)


for (i in c(1:9, 11:19, 21:29) )
{
  df_Neighbours_with_diag <- rbind(df_Neighbours_with_diag,
                                            c(i, i+11, 1/sqrt(2))
                                          )
}

tail(df_Neighbours_with_diag)


plot_SingleGrainDisc(object = vn_simulated,
          df_neighbours = df_Neighbours_with_diag,
          show_neighbours = TRUE,
          show_location_ids = TRUE)

## ----restrict_to_inner_8x8, fig.dim = c(8, 5), out.width="800px"--------------

vn_values_to_show <-
      sample(x = c(rnorm(n = 30, mean = 2000, sd = 500),
                   rnorm(n = 70, mean = 20, sd = 1)),
             size = 100)

## Set the outer rows to NA before adding crosstalk
vn_disc_border_locations <- c(1:10,
                              91:100,
                              seq(from = 11, to = 81, by = 10),
                              seq(from = 20, to = 90, by = 10)
                             )
vn_values_to_show[vn_disc_border_locations] <- NA

vn_values_to_show <- apply_Crosstalk(object = vn_values_to_show,
                                     n_crosstalk = 0.15)

calc_MoransI(object = vn_values_to_show)

plot_SingleGrainDisc(object = vn_values_to_show,
                     show_neighbours = TRUE,
                     ignore_borders = TRUE)

calc_MoransI(object = vn_values_to_show,
             ignore_borders = TRUE)


## ----show_disc_extended, fig.dim = c(8, 5), out.width="800px"-----------------

plot_SingleGrainDisc(object = vn_simulated,
          main = "",
          legend = TRUE,
          show_coordinates = TRUE,
          show_location_ids = TRUE,
          show_positioning_holes = FALSE)


## ----show_disc_scales, fig.dim = c(8, 5), out.width="800px"-------------------

plot_SingleGrainDisc(object = vn_simulated,
          main = "Linear scale",
          legend = TRUE,
          show_coordinates = FALSE,
          show_location_ids = FALSE,
          show_positioning_holes = TRUE,
          str_transform = "lin")


plot_SingleGrainDisc(object = vn_simulated,
          main = "Logarithmic scale",
          legend = TRUE,
          show_coordinates = FALSE,
          show_location_ids = FALSE,
          show_positioning_holes = TRUE,
          str_transform = "log")


## ----show_scatterplot_options, fig.dim = c(8, 5), out.width="800px"-----------


vn_simulated <- c(rnorm(75, mean = 10, sd = 5),
                  rnorm(25, mean = 500, sd = 50)  )
vn_simulated <- sample(size = 100, vn_simulated)

vn_simulated_with_crosstalk <- apply_Crosstalk(object = vn_simulated,
                                               n_crosstalk = 0.15)

## Base use
plot_MoranScatterplot(object = vn_simulated,
                      main = "Without crosstalk")


plot_MoranScatterplot(object = vn_simulated_with_crosstalk,
                      main = "With crosstalk")


## Layout options
plot_MoranScatterplot(object = vn_simulated_with_crosstalk,
           pch = "show_location_ids",
           legend = FALSE,
           log = "xy",
           main = "With location ID's; and with log scales"
           )

plot_MoranScatterplot(object = vn_simulated_with_crosstalk,
           pch = "show_n_neighbours",
           legend = FALSE,
           str_y_def = "weighted_sum",
           main = "With number of neighbours, and other y calculation"
           )

## ----moransI_intermediate_numbers---------------------------------------------


calc_MoransI(object = 1:100,
             return_intermediate_values = TRUE)


## ----moransI_intermediate_numbers_extended------------------------------------

vn_simulated <- sample(x = c(rnorm(n = 30, mean = 2000, sd = 500),
                             rnorm(n = 70, mean = 20, sd = 1)),
                       size = 100)
vn_simulated <- round(vn_simulated)

vn_simulated_with_crosstalk <- apply_Crosstalk(object = vn_simulated,
                                               n_crosstalk = 0.20)
vn_simulated_with_crosstalk <- round(vn_simulated_with_crosstalk)


df_compare <-
  data.frame(Case = c("Without crosstalk", "With crosstalk"),
             MoransI =  c(calc_MoransI(object = vn_simulated),
                          calc_MoransI(object = vn_simulated_with_crosstalk)
                          ),
             PopulationVar =
               c(calc_MoransI(object = vn_simulated,
                 return_intermediate_values = TRUE)$n_population_variance,
                 calc_MoransI(object = vn_simulated_with_crosstalk,
                 return_intermediate_values = TRUE)$n_population_variance),
             SpatialAutoCor =
               c(calc_MoransI(object = vn_simulated,
                 return_intermediate_values = TRUE)$n_average_auto_correlation,
                 calc_MoransI(object = vn_simulated_with_crosstalk,
                 return_intermediate_values = TRUE)$n_average_auto_correlation)
      )

df_compare[,2] <-  round(df_compare[,2],2)

(df_compare)


