# Open the data file, using a file open dialog, select the file, CH01TA01.txt
toluca <- read.table(file.choose(),header=F,col.names=c("X","Y"))

# This command allows us to use the variable names, X and Y, directly, without
# having to use the prefix, toluca$
attach(toluca)

# print the data table
toluca
 
# Plot the data
plot(X,Y)

# Calculate linear regression fit using the solution to the normal equations, 1.10a-b, p. 17
# This is more of a "manual" calculation showing the intermediate steps
n <- length(Y)

mean_x <- mean(X)
mean_y <- mean(Y)
var_x <- var(X) 
var_y <- var(Y)
cov_xy <- cov(X,Y)
 
SS_xx <- (n-1)*var_x
SS_xy <- (n-1)*cov_xy
SS_yy <- (n-1)*var_y
 
b1 <- SS_xy/SS_xx
b0 <- mean_y - b1*mean_x

# Calculate the predicted values and residuals 
yhat <- b0 + b1*X
e <- Y-yhat
 
# Calculate error sum of squares, error mean square, and s, the estimate of sigma
SSE <- sum(e^2)
MSE <- SSE/(n-2)
s <- sqrt(MSE)
 
# Print the data point, (mean of X, mean of Y)
print(cbind(mean_x,mean_y))

# Print the regression line parameter estimates
print(cbind(b0,b1))
          
# Print the response data (Y), the fitted values (yhat), and the residuals, e
print(cbind(Y,yhat,e))

#Plot the data and add the fitted regression line
plot(X,Y,xlim=c(0,125))
abline(b0,b1)  

# Fitted value for X=100
fit1 <- b0 + b1*100

# You can get most of the same results as above with these commands
# The lm function does all of the intermediate calculations and creates an object
# that can be queried for other details
fit <- lm (Y ~ X)
fit
b1 <- fit$coeff[2]
b0 <- fit$coeff[1]
anova(fit)
yhat.values <- fit$fitted
e.values <- fit$residuals
