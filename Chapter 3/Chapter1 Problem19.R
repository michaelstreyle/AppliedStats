# Open the data file, using a file open dialog, select the file, CH01PR19.txt
setwd ("C:/users/iverph01/documents/Stat 327/KutnerData")
gpa <- read.table(file.choose(),header=F,col.names=c("GPA","ACT"))

# Alternatively, set the working directory and supply the file name
#setwd ("C:/users/iverph01/documents/Stat 327/KutnerData/Chapter  1 Data Sets")
#gpa <- read.table("CH01PR19.txt",header=F,col.names=c("GPA","ACT"))


# This command makes data frame column names available as variables
attach(gpa)

# print the data table
gpa
 
# Plot the data
plot(ACT,GPA)

# Fit the regression model
fit <- lm (GPA ~ ACT)
fit
b1 <- fit$coeff[2]
b0 <- fit$coeff[1]
anova(fit)
ACTat30 <- b0 + b1*30
abline (b0,b1)

summary(fit)
confint(fit)

mse = anova(fit)[2,3]

gpa.resid = fit$residuals
gpa.fit = fit$fitted.values

plot(ACT,gpa.resid)
abline(h=0)

plot(gpa.fit,gpa.resid)
abline(h=0)

plot(ACT,gpa.fit)

plot(ACT,abs(gpa.resid))

# Semi studentized residuals
gpa.semist.res = gpa.resid/sqrt(mse)

plot(ACT,gpa.semist.res)
abline(h=0)
abline(h=c(-2, 2), lty=2)

# Pagan-Breusch test for normality
library(lmtest)
bptest(fit)

# Normal probability plot
qqnorm(gpa.resid)

# Added variable plots
# Choose file, CH03PR03.txt
gpa2 <- read.table(file.choose(),header=F,col.names=c("GPA","ACT","ITscore","ClassRank"))

plot(gpa2$ITscore, gpa.resid)
plot(gpa2$ClassRank, gpa.resid)
