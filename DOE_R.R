#-------------------------------------------
# Design of Experiment: 2^2 factorial design
#-------------------------------------------

# Create a data frame
data <- data.frame(
  Factor = c("- -", "+ -", "- +", "+ +"),
  Combination = c("A low, B low", "A high, B low", "A low, B high", "A high, B high"),
  Reactant = c(15, 25, 15, 25),
  Catalyst = c(1, 1, 5, 2),
  Replicate_A = c(28, 36, 18, 31),
  Replicate_B = c(25, 32, 19, 30),
  Replicate_C = c(27, 32, 23, 29),
  Total = c(80, 100, 60, 90),
  AverageYield = c(26.7, 33.4, 20,30))

data

# Data coded with -1 and +1
datacodedminusplus <- data.frame(
  react = c(-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1),
  cata = c(-1,-1,+1,+1,-1,-1,+1,+1,-1,-1,+1,+1),
  yield = c(28,36,18,31,25,32,19,30,27,32,23,29)
)

# Compute the average effects

A = (1/(2*3)) * (-60+90-80+100) # [1] 8.333333
B = (1/(2*3)) * (+60+90-80-100) # [1] -5
AB = (1/(2*3)) * (-60+90+80-100) # [1] 1.666667

# The effect of A (reactant concentration) is positive; this suggests that increasing A from the
# low level (15%) to the high level (25%) will increase the yield. The effect of B (catalyst) is
# negative; this suggests that increasing the amount of catalyst added to the process will
# decrease the yield. The interaction effect appears to be small relative to the two main effects.

Reactant <- datacodedminusplus[,1]
Catalyst <- datacodedminusplus[,2]
ReactantCatalyst <- datacodedminusplus[,1]*datacodedminusplus[,2]

datacoded <- data.frame(Reactant, Catalyst, ReactantCatalyst)
ls.print(lsfit(datacoded, datacodedminusplus[,3])) # only the intercept is significant
# Residual Standard Error=1.9791
# R-Square=0.903
# F-statistic (df=3, 8)=24.8227
# p-value=2e-04
# 
#                  Estimate Std.Err t-value Pr(>|t|)
# Intercept         27.5000  0.5713 48.1354   0.0000
# Reactant           4.1667  0.5713  7.2932   0.0001
# Catalyst          -2.5000  0.5713 -4.3759   0.0024
# ReactantCatalyst   0.8333  0.5713  1.4586   0.1828

#----------------------------------------------
# Central Composite Design and response surface
#----------------------------------------------

data <- data.frame(
  Run = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  Time = c(80, 80, 90, 90, 85, 85, 85, 92.07, 77.93, 85, 85),
  Temperature = c(170, 180, 170, 180, 175, 175, 175, 175, 175, 182.07, 167.93),
  X1 = c(-1, -1, 1, 1, 0, 0, 0, 1.414, -1.414, 0, 0),
  X2 = c(-1, 1, -1, 1, 0, 0, 0, 0, 0, 1.414, -1.414),
  Yield = c(76.5, 77, 78, 79.5, 79.9, 80, 80.3, 78.4, 75.6, 78.5, 77))

data
#    Run  Time Temperature     X1     X2 Yield
# 1    1 80.00      170.00 -1.000 -1.000  76.5
# 2    2 80.00      180.00 -1.000  1.000  77.0
# 3    3 90.00      170.00  1.000 -1.000  78.0
# 4    4 90.00      180.00  1.000  1.000  79.5
# 5    5 85.00      175.00  0.000  0.000  79.9
# 6    6 85.00      175.00  0.000  0.000  80.0
# 7    7 85.00      175.00  0.000  0.000  80.3
# 8    8 92.07      175.00  1.414  0.000  78.4
# 9    9 77.93      175.00 -1.414  0.000  75.6
# 10  10 85.00      182.07  0.000  1.414  78.5
# 11  11 85.00      167.93  0.000 -1.414  77.0

TimeD = data[,4]
TempD = data[,5]
TimeDsquared = data[,4]^2
TempDsquared = data[,5]^2
InteractionTimeTemp = data[,4] * data[,5]

datacoded <- data.frame(TimeD, TempD, TimeDsquared, TempDsquared, InteractionTimeTemp)
ls.print(lsfit(datacoded, data[1:11,6])) # all factors are significant except the interraction
# Residual Standard Error=0.2724
# R-Square=0.9851
# F-statistic (df=5, 5)=66.1178
# p-value=1e-04
# 
#                     Estimate Std.Err  t-value Pr(>|t|)
# Intercept            80.0666  0.1573 509.0411   0.0000
# TimeD                 0.9951  0.0963  10.3299   0.0001
# TempD                 0.5152  0.0963   5.3485   0.0031
# TimeDsquared         -1.4398  0.1147 -12.5562   0.0001
# TempDsquared         -1.0647  0.1147  -9.2849   0.0002
# InteractionTimeTemp   0.2500  0.1362   1.8353   0.1259

# coefficients full model with an interaction
fullmodel <- lsfit(datacoded[,-5],data[1:11,6])
fullmodel$coefficients
#  Intercept        TimeD        TempD TimeDsquared TempDsquared 
# 80.0665910    0.9950503    0.5152028   -1.4397770   -1.0646638


colors <- colorRampPalette(c("orange", "red", "darkred"))(15)

contour.f <- function(fname, xgrid, ygrid, xlab = "time", ylab = "temperature") { 
  x <- rep(xgrid, length(ygrid))
  y <- rep(ygrid, rep(length(xgrid), length(ygrid)))
  z <- fname(x, y)
  zz <- matrix(z, length(xgrid), length(ygrid))
  contour(xgrid, ygrid, zz, xlab = xlab, ylab = ylab, lwd = 2, drawlabels = TRUE,
          labcex = 1.3, method = "flattest", col = colors)
}

contour.f(surface.f, seq(-3, 3, 0.2), seq(-3, 3, 0.2))
title(main = "Contours of fitted response surface for the second example")

#----
# end
#----

