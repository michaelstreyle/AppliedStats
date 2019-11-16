data = read.table(file.choose(), sep=',', header = TRUE)
attach(data)

fit1 = lm(Brain_Weight ~ Body_Weight, data = data)
summary(fit1)
par(mfrow=c(1,1))
plot(x=Body_Weight,y=Brain_Weight,xlab="Body Weight",
     ylab="Brain Weight",main="Simple Linear Regression (Bad)",pch=21, bg="red")
abline(fit1)
#par(mfrow=c(2,2))
#plot(fit1)

#Lack-of-fit test  - NOTE: can't do LOF on this data set - no replication.
#Reduced=lm(Brain_Weight ~ Body_Weight)   #fit reduced model
#Full=lm(Brain_Weight~0+as.factor(Body_Weight))    #fit full model
#anova(Reduced, Full)    #get lack-of-fit test


# Breusch-Pagan Test for the constancy of the error variance

library(lmtest)
bptest(fit1, studentize=FALSE)

# Normality of error terms.

shapiro.test(fit1$residuals)

hist(fit1$residuals)
boxplot(fit1$residuals)
hist(Body_Weight)
boxplot(Body_Weight)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit2 = lm(Brain_Weight ~ log(Body_Weight), data = data)
par(mfrow=c(1,1))
plot(x=log(data$Body_Weight),y=data$Brain_Weight,xlab="Log(Body Weight)",
     ylab="Brain Weight",main="Simple Linear Regression (Still Bad)",pch=22, bg="blue" )
abline(fit2)


# Breusch-Pagan Test for the constancy of the error variance

#library(lmtest)
bptest(fit2, studentize=FALSE)

# Normality of error terms.

shapiro.test(fit2$residuals)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(MASS)
boxcox(fit2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit3 = lm(log(Brain_Weight) ~ log(Body_Weight), data = data)
par(mfrow=c(1,1))
plot(log(data$Body_Weight),log(data$Brain_Weight),xlab="Log(Body Weight)",
     ylab="Log(Brain Weight)",main="Simple Linear Regression (Better)", pch=23, bg="Purple")
abline(fit3)


# Breusch-Pagan Test for the constancy of the error variance

#library(lmtest)
bptest(fit3, studentize=FALSE)

# Normality of error terms.

shapiro.test(fit3$residuals)

hist(fit3$residuals) 
boxplot(fit3$residuals)

# New data table (data frame) with log values
data2 = cbind (data, log(Body_Weight), log(Brain_Weight))
head(data2)
names(data2)
names(data2)[4:5]
names(data2)[4:5] = c("Log_Body_wt", "Log_Brain_wt")
head(data2)
attach(data2)
plot(Log_Body_wt,Log_Brain_wt)

# Who are the 3 outliers?
data2[Log_Body_wt>9,]
data2[Log_Body_wt>7,]
data2[Log_Brain_wt>6,]

# Fit without the dinosaurs
data3 = data2[Log_Body_wt<9,]
nodino = lm(Log_Brain_wt ~ Log_Body_wt, data=data3)
abline(nodino)
summary(nodino)
plot(data3$Log_Body_wt, nodino$residuals)
abline(h=0)
confint(nodino)
boxplot(nodino$residuals)
