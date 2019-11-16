
setwd("C:/Users/Michael Streyle/Desktop/Applied Stats 1/Chapter 3")

# Open the data file, using a file open dialog, select the file, CH01PR22.txt
drugs <- read.table(file.choose(),header=F,col.names=c("dose","residual"))

# This command allows us to use the variable names, X and Y, directly, without
# having to use the dataset name and $ as a prefix.
attach(drugs)

# Plot the data
plot(dose,residual)

hfit <- lm (residual ~ dose)
summary(hfit)
abline(hfit$coefficients)


cat("\nCONFIDENCE LIMITS FOR INTERCEPT AND SLOPE:\n\n")
confint(hfit, level=0.90)

cat("\nHYPOTHESIS TESTS FOR INTERCEPT AND SLOPE:\n\n")
summary(hfit)$coefficients

# Correlation between hardness and time
cat("\nR SQUARED:\n\n")
summary(hfit)$r.squared

cat("\nCORRELATION COEFFICIENT:\n\n")
cor(hardness,time)

h.resid=hfit$residuals
h.yhat=hfit$fitted.values
boxplot(h.resid)
boxplot(time)
plot(h.yhat,h.resid)
plot(time,h.resid)
qqnorm(h.resid)

library(lmtest)
bptest(hfit)

# Lack of Fit test
h.full = lm (hardness ~ 0 + as.factor(time))
anova(hfit,h.full)

detach (hardnessdata)

# Open data for Table 3.4, CH03TA04.txt
deposits <- read.table(file.choose(),header=F,col.names=c("MinSize","NumAccounts"))

attach(deposits)

plot(MinSize,NumAccounts)

dfit = lm(NumAccounts ~ MinSize)
abline(dfit$coefficients)
summary(dfit)

# Lack of fit test
d.full = lm(NumAccounts ~ 0 + as.factor(MinSize))
summary(d.full)
aggregate(NumAccounts, by=list(MinSize), FUN=mean)
anova(dfit,d.full)

