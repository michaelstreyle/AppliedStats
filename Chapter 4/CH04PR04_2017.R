# CH04PR04.R
# Set the working directory

setwd("C:/Users/iverph01/Documents/Stat 327/KutnerData/Chapter  1 Data Sets")

# Open the data file, CH01PR21.txt
mydata <- read.table(file="CH01PR21.txt",header=F,col.names=c("Y","X"))

xname = "Num Transfers"
yname = "Breakage"

# This command allows us to use the variable names, X and Y, directly, without
# having to use the dataset name and $ as a prefix.
attach(mydata)

# Plot the data
plot(X,Y,xlab=xname,ylab=yname, ylim=c(8,25))

myfit <- lm (Y ~ X)
myfit
abline(myfit)

# Individual 95% Confidence limits for intercept and slope
confint(myfit, level=0.95)

# Joint 95% Confidence limits for intercept and slope
confint(myfit, level=0.975)

# Check joint confidence limits
b0 = myfit$coeff[1]
b1 = myfit$coeff[2]
summ = summary(myfit)$coefficients
se.b1 = summ[2,2]
aov = anova(myfit)
error.df = aov$Df[2]
b1.lower = b1 - qt(1-0.05/4, error.df)*se.b1
b1.upper = b1 + qt(1-0.05/4, error.df)*se.b1

cat(b1.lower, b1.upper, sep=", ")


# INDIVIDUAL 95% CONFIDENCE INTERVALS FOR MEAN RESPONSE at Transfers = 2, 3, or 4
predict(myfit, data.frame(X=c(2,3,4)), interval="confidence", se.fit=F, level=1-0.05)

# JOINT 95% CONFIDENCE INTERVALS FOR MEAN RESPONSE at Transfers = 2, 3, or 4
predict(myfit, data.frame(X=c(2,3,4)), interval="confidence", se.fit=F, level=1-0.05/3)

# Individual 95% PREDICTION INTERVALs FOR AN INDIVIDUAL RESPONSE at Transfers = 2, 3, or 4
predict(myfit, data.frame(X=c(2,3,4)), interval="prediction", se.fit=F, level=1-0.05)

# Individual 95% PREDICTION INTERVALs FOR AN INDIVIDUAL RESPONSE at Transfers = 2, 3, or 4
predict(myfit, data.frame(X=c(2,3,4)), interval="prediction", se.fit=F, level=1-0.05/3)


# Working-Hotelling limits (confidence band for the regression line) at Xh = 100
fcrit <- qf(0.95,2,error.df)   
xgrid = seq(0, 3, 0.1)
pred <- predict(myfit, data.frame(X=xgrid),se.fit=TRUE)   
w <- sqrt(2*fcrit)           
marg.err <- w*pred$se.fit   

# Add Working-Hotelling limits to the plot
yhat=pred$fit
wh.lower=pred$fit-marg.err
wh.upper=pred$fit+marg.err
lines (xgrid, wh.lower, lty=2)
lines (xgrid, wh.upper, lty=2)

# plot lines for response mean and prediction limits at X=2
pred.mean = predict(myfit, data.frame(X=c(2)), interval="confidence", se.fit=F, level=1-0.05)
lines(c(2,2), pred.mean[2:3], lwd=11)

pred.mean.jt = predict(myfit, data.frame(X=c(2)), interval="confidence", se.fit=F, level=1-0.05/3)
lines(c(2,2), pred.mean.jt[2:3], lwd=7, col="red")

pred.indiv = predict(myfit, data.frame(X=c(2)), interval="prediction", se.fit=F, level=1-0.05)
lines(c(2,2), pred.indiv[2:3], lwd=3, col="green")

pred.indiv.jt = predict(myfit, data.frame(X=c(2)), interval="prediction", se.fit=F, level=1-0.05/3)
lines(c(2,2), pred.indiv.jt[2:3], lwd=1, lty=2, col="black")


# Lack of fit test
full = lm (Y ~ 0 + as.factor(X))
full

# Lack of fit test for the usual linear regression
anova(myfit, full)

# Using the original fit, predict X at Y=16
# Must use equations 4.31, 4.32, and 4.32a from the book, pp. 168-9
# Repeat some statements from before, for completeness
myfit = lm (Y ~ X)
b0 = myfit$coeff[1]
b1 = myfit$coeff[2]
xnew = (16 - b0)/b1
summ = summary (myfit)
mse = summ$sigma^2
n = length(Y)
numer = (xnew - mean(X))^2
denom = sum ((X - mean(X))^2)
s.predx.sq = (mse/b1^2)*(1 + 1/n + numer/denom)
xnew.lower = xnew - qt(0.95, n-2)*sqrt(s.predx.sq)  # Change confidence level, as needed
xnew.upper = xnew + qt(0.95, n-2)*sqrt(s.predx.sq)  # Change confidence level, as needed

# Prediction and Confidence interval for predicted X value at Y=16
data.frame(Xnew = c(xnew), Lower = c(xnew.lower), Upper = c(xnew.upper), row.names=c("Prediction"))

# Plot the prediction interval
lines (c(xnew.lower, xnew.upper), c(16, 16), lwd=3)
lines (c(xnew, xnew), c(8,16), lwd=1, lty=2)

# Equation to check the reasonableness of the interval, equation 4.33, p. 170 should be approx. < 0.1
eqn4.33 = (qt(0.95, n-2)^2)*mse/(b1^2 * denom)
eqn4.33

# Fit X vs Y (reverse the roles of X and Y) to get a direct prediction of
# X for Y=16.

revfit = lm (X ~ Y)
plot (Y, X, ylab="Breakage", xlab="Num Transfers")
abline (revfit)
pred16 = predict (revfit, data.frame(Y=16), interval='prediction')
pred16
lines (c(16, 16), pred16[2:3])
