# Project 1 Due Friday Oct 21 - Nathan Fleet
#
# The first course project is to apply the simjple linear regression model for
# the two variables "hwy" and "displ" in a data set "mpg" given in ggplot2.
# You need to consider the following steps to complete the project
#
################################################################################
#
# 1.) Introduce the two variables. What does each variable represent? What kind
# of relationship do we expect the two variables to hold?
#
# The "displ" variable represents the displacement, or the measure of th cylinder
# volume swept by all of the pistons of a piston engine.
#
# The "hwy" variable represents the number of miles per gallon of gas on the 
# highway.
#
# The relationship we expect the two variables to hold is the larger the "displ"
# variable, the lower the "mpg" variable will become. This could also be referred
# to as a negative linear relationship.
#
################################################################################
#
# 2.) Summarize each of the two variables numerically (mean, median, standard
# deviation). Summarize each of the two variables graphically (histogram, box
# plot).

library(ggplot2)

# Mean, median, sd for displ
mean(mpg$displ)   # 3.471795
median(mpg$displ) # 3.3
sd(mpg$displ)     # 1.291959

# Histogram and boxplot for displ
hist(mpg$displ, xlab="Engine Displacement", ylab="Frequency", breaks=10, 
     main="Histogram of Engine Displacement")
boxplot(mpg$displ)

# Mean, median, sd for hwy
mean(mpg$hwy)    # 23.44017
median(mpg$hwy)  # 24
sd(mpg$hwy)      # 5.954643

# Histogram and boxplot for hwy
hist(mpg$hwy, xlab="MPG on highway", ylab="Frequency", breaks=10, 
     main="Histogram of MPG on Highway")
boxplot(mpg$hwy)

################################################################################
#
# 3.) Choose the variable "hwy" as the response. Use the scatterplot to 
# demonstrate the relationship between the response and the explanatory 
# variable "displ". Is there any linear relationship shown in the plot?

plot(hwy~displ, data=mpg, xlab="Engine Displacement", ylab="MPG on Highway",
     main="Engine Displacement vs MPG on Highway")

# Based on this scatterplot, there apepars to be a negative linear relationship
# between engine displacement and mpg on highway

################################################################################
#
# 4.) Explicitly write out the simple linear regression model of the response 
# and the explanatory variables. Then estimate all the unknown parameters in the
# model (intercept, slope and variance). Finally report the fitted regression 
# line.


# SIMPLE LINEAR REGRESSION MODEL

Y=mpg$hwy   # Response
x=mpg$displ # Explanatory variable

# Estimate Intercept and Slope:

beta1.hat=(sum((x-mean(x))*(Y-mean(Y))))/(sum((x-mean(x))^2)) # Estimated slope
beta0.hat=mean(Y)-beta1.hat*mean(x)                           # Estimated intercept
sigma2.hat=(sum((Y-beta0.hat-beta1.hat*x)^2))/(length(Y)-2)   # Estimated variance

# Report Fitted Regression Line:

# y.hat = beta0.hat + beta1.hatx
# y.hat = 35.69765 - 3.530589x

################################################################################
#
# 5.) Add the fitted regression line to the scatterplot.

plot(hwy~displ, data=mpg, xlab="Engine Displacement", ylab="MPG on Highway",
     main="Engine Displacement vs MPG on Highway")
abline(beta0.hat, beta1.hat, lwd=3, col="BLUE")

################################################################################
#
# 6.) Use the fitted regression line to predict the mpg in freeway when the displ 
# equals 2, 2.5, 5.5 and 6.7. Can we make a reliable prediction of mpg in freeway
# when displ equals 8? Justify your conclusion.

x.pred=2
Y.pred=beta0.hat+beta1.hat*x.pred # 28.63647 mpg in freeway
Y.pred

x.pred=2.5
Y.pred=beta0.hat+beta1.hat*x.pred # 26.87118 mpg in freeway
Y.pred

x.pred=5.5
Y.pred=beta0.hat+beta1.hat*x.pred # 16.27941 mpg in freeway
Y.pred

x.pred=6.7
Y.pred=beta0.hat+beta1.hat*x.pred # 12.04271 mpg in freeway
Y.pred

range(mpg$displ) # 1.6-7.0

# We cannot make a reliable prediction of mpg in freeway when mpg equals 8. This is
# because the range of our data for "displ" only goes up to 7. Trying to predict
# the mpg in freeway past 7 is called extrapolation, and any predictions made would
# be unreliable
#
################################################################################
#
# 7.) Test if the slope of the regression is less than zero. Report the value of
# test statistic, the p-value and the corresponding conclusion.

Y=mpg$hwy   # Response
x=mpg$displ # Explanatory variable

# Estimate B0, B1, and Sig^2
beta1.hat=(sum((x-mean(x))*(Y-mean(Y))))/(sum((x-mean(x))^2)) # Estimated slope
beta1.hat
beta0.hat=mean(Y)-beta1.hat*mean(x)                           # Estimated intercept
beta0.hat
sigma2.hat=(sum((Y-beta0.hat-beta1.hat*x)^2))/(length(Y)-2)   # Estimated variance
sigma2.hat

# Test if slope B1 equals a claimed value
# H0: B1=0    H1: B1≠0

# Compute test statistic
beta1.hat.se=sqrt((sigma2.hat)/sum((x-mean(x))^2))
beta1.hat.se
t.stat=beta1.hat/beta1.hat.se              # test statistic=-18.15085
t.stat

# Compute p-value
p.value=2*(1-pt(abs(t.stat), length(Y)-2)) # p-value=0
p.value

# Decision Making
# Since the p-value is less than 0.05, we reject the null hypothesis that B1 equals
# 0. This means that the relationship between the response and explanatory variable
# is linear.