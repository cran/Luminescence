##//////////////////////////////////////////////////////////////////////////////
##//zzz.R
##//////////////////////////////////////////////////////////////////////////////
##
##==============================================================================
##author: R Luminescence Package Team
##organisation: 
##version.: 0.1.1
##date: 2013-03-11
##==============================================================================
# Set namespace .LuminescenceEnv ------------------------------------------
.LuminescenceEnv <- new.env(parent = emptyenv())  
 
# Assign variables to Namespace -------------------------------------------     
##variable col to define colours in the functions for output
assign("col",  
       unlist(colors())[c(261,552,51,62,76,151,451,474,654,657,100,513,23,612,129,27,551,393)], 
       pos = ".LuminescenceEnv",
       envir = .LuminescenceEnv) 



##==============================================================================
##on Attach
.onAttach <- function(libname,pkgname){
  
  ##set startup message
  try(packageStartupMessage(paste("Welcome to the R package Luminescence version ",
                              packageDescription(pkg="Luminescence")$Version,
                              " [Built: ",
                              strsplit(packageDescription(pkg="Luminescence")$Packaged, ";")[[1]][1],
                             "]", sep="")), silent=TRUE)                        
}

##==============================================================================
# DO NOT TOUCH! -----------------------------------------------------------

sTeve<- function(n_frames = 20, t_animation = 3) {

par(new = TRUE)
plot(NA, xlim = c(0, 10), ylim = c(0, 10), main = "", xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)

n_frames      <- n_frames
t_animation   <- t_animation

dt            <- t_animation / n_frames
x1            <- seq(0, 10, length.out = n_frames)
y1            <- rep(1.5, n_frames)
r1            <- 0.5

x2            <- seq(0, 16, length.out = n_frames)
y2            <- rep(8.5, n_frames)
r2            <- 0.5

x4            <- seq(11, 0, length.out = n_frames)
y4            <- rep(5, n_frames)
r4            <- 0.5

# set angles for each step of mouth opening
angles_mouth <- rep(c(0.01, 0.25, 0.5, 0.25), length.out = n_frames)

for(i in 1:n_frames){
  # define pacman circles
  filledcircle(r1 = r1, r2 = 0.00001, mid = c(x1[i], y1[i]), from = angles_mouth[i], to = 2 * pi - angles_mouth[i], col = "yellow")
  filledcircle(r1 = r2, r2 = 0.00001, mid = c(x2[i], y2[i]), from = angles_mouth[i], to = 2 * pi - angles_mouth[i], col = "yellow")
  filledcircle(r1 = r4, r2 = 0.00001, mid = c(x4[i], y4[i]), from = angles_mouth[i] + 3, to = 2 * pi - angles_mouth[i] + 3, col = "yellow")
    
  # dinfine eyes for pacman
  points(x1[i] + 0.2, y1[i] + 0.75, pch = 21, bg = 1, cex = 0.7)
  points(x2[i] + 0.2, y2[i] + 0.75, pch = 21, bg = 1, cex = 0.7)
  points(x4[i] - 0.05, y4[i] + 0.75, pch = 21, bg = 1, cex = 0.7)

  Sys.sleep(dt)
  
  plotcircle(r = 1.1 * r1, mid = c(x1[i], y1[i]), col = "white", lcol = "white")
  plotcircle(r = 1.1 * r2, mid = c(x2[i], y2[i]), col = "white", lcol = "white")
  plotcircle(r = 1.1 * r4, mid = c(x4[i], y4[i]), col = "white", lcol = "white")
}
}
