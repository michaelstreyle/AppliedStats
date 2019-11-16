# Applied Statistics Project 1
# Michael Streyle, Matt Foundos, Carly Mester


beef <- read.table('http://ww2.amstat.org/publications/jse/v22n1/kopcso/BeefDemand.txt', header = TRUE)

 
attach(beef)

# Plot some of the data
plot(Year,RealBeefPrice)
plot(Year,BeefConsump)
plot(Year, RealChickenPrice)
typeof(beef)


#showing CPI and DPI have a very high correlation, indicates we dont need to use them both
cor(DPI, CPI)

#some boxplots to check distribution, all seem relatively normal
boxplot(RealBeefPrice)
boxplot(RealChickenPrice)
boxplot(RealDPI)
boxplot(X.RDPI.Mean..sq)

#shows the correlation matrix and pairs plots
cor(beef)
pairs(beef)

#trying to use RealChickenPrice and RealBeefPrice to model BeefConsump
myfit <- lm (BeefConsump ~ RealChickenPrice + RealBeefPrice)
myfit
summary(myfit)
plot(myfit)

#this boxplot shows there are outliers on both ends of the data
resid = myfit$residuals
fitted = myfit$fitted.values
boxplot(resid)

plot (fitted, resid, xlab="Fitted Values", ylab="Residuals", col='blue')
abline(h=0, col='red')

#qqplot

qqnorm (resid, col='blue')
qqline (resid, col='red')

#this indicates a transformation might be helpful.
library(MASS)
boxcox(RealChickenPrice ~ BeefConsump)
boxcox(RealBeefPrice ~ BeefConsump)
#boxcox plots are not conclusive

#going to see what a model with all the variables looks like
myfit2 <- lm (BeefConsump ~ RealChickenPrice + RealBeefPrice + RealDPI + Year + X.RDPI.Mean..sq)
plot(myfit2)
plot(myfit2$residuals)

plot(resid, RealDPI)
myfit3 <- lm(BeefConsump ~ RealChickenPrice + RealBeefPrice + X.RDPI.Mean..sq)
plot(myfit3)
myfit3
summary(myfit3)
boxcox(myfit3)

#what exactly is X.RDPI.Mean..sq?
# it is The square of the difference between Inflation-adjusted Disposable Personal Income per capita and its mean


#bp test
library(lmtest)
bptest(myfit3, studentize=FALSE)
#the results of the bptest indicate non-constant variance since the p-value is 3.908e-05 which is less than 0.01 (my chosen level of significance)


#lack of fit test

aggregate (BeefConsump, by=list(RealBeefPrice, RealChickenPrice, RealDPI), FUN='mean')

# Fit the full model:
full = lm (BeefConsump ~ 0 + as.factor(RealBeefPrice) * as.factor(RealChickenPrice) * as.factor(RealDPI))
anova (myfit3, full)
summary(myfit3)
#the summary shows a p-value of 6.909e-07 which is small suggesting a good regression relation

#looking at some residual diagnostics of myfit3

plot(myfit3)
#semistudentized residuals
plot(myfit3$fitted.values, studres(myfit3), main="Studentized residuals vs fitted values
")
#note that none of the studentized residuals are greater than the absolute value of 4 - no outliers
plot( RealBeefPrice, myfit3$residuals, main="RealBeefPrice vs Residuals")
plot( RealChickenPrice, myfit3$residuals, main="RealChickenPrice vs Residuals")
plot( X.RDPI.Mean..sq, myfit3$residuals, main="X.RDPI.Mean..sq vs Residuals")
#the X.RDPI.Mean..sq vs residuals seems interesting. 

boxplot(myfit3$residuals)
qqnorm(myfit3$residuals)
qqline(myfit3$residuals)

#before running the following lines, enlarge your plots window, or it might produce an error
hist(RealBeefPrice)
hist(RealChickenPrice)     #these show some interesting distributions
hist(X.RDPI.Mean..sq)
#RealChickenPrice and X.RDPI.Mean..sq show some indication of being right skewed so
#even though other indicators don't suggest this, maybe try a log transformation (as suggested in advice doc on katie)

#random lines of code
confint(myfit3, level=0.95)  
summary(myfit3)$r.squared
cor(BeefConsump, myfit3$fitted.values)^2 #same as line 112



#some confidence interval stuff
mean1 = mean(X.RDPI.Mean..sq)  #setting X.RDPI.Mean..sq to its mean because using a random number for it seems somewhat meaningless because it is a derived variable. (Maybe I should make derive its value with 50,50)

predict (myfit3, data.frame (RealBeefPrice=50, RealChickenPrice=50, X.RDPI.Mean..sq=mean1), interval='confidence', level=0.99)
predict (myfit3, data.frame (RealBeefPrice=50, RealChickenPrice=50, X.RDPI.Mean..sq=mean1), interval='prediction', level=0.99)

