
# Open the data file, using a file open dialog, select the file, CH01PR20.txt
copierdata <- read.table(file.choose(),header=F,col.names=c("ServiceMinutes","NumCopiers"))

# This command allows us to use the variable names, X and Y, directly, without
# having to use the dataset name and $ as a prefix.
attach(copierdata)

# Plot the data
plot(NumCopiers,ServiceMinutes)

fit <- lm (ServiceMinutes ~ NumCopiers)
fit
b1 <- fit$coeff[2]
b0 <- fit$coeff[1]
abline(b0,b1)

cat("\nCONFIDENCE LIMITS FOR INTERCEPT AND SLOPE:\n\n")
confint(fit, level=0.90)

qt(.95, 43)
pt(2.1428, 43, lower=FALSE)


cat("\nHYPOTHESIS TESTS FOR INTERCEPT AND SLOPE:\n\n")
summary(fit)$coefficients

meantime=predict(fit, data.frame(NumCopiers=6), interval="confidence", se.fit=TRUE, level=0.9)$fit
cat("\nCONFIDENCE INTERVAL FOR A MEAN RESPONSE at NumCopiers = 6:\n\n")
meantime

predtime = predict(fit, data.frame(NumCopiers=6), interval="prediction", se.fit=TRUE, level=0.9)$fit
cat("\nPREDICTION INTERVAL FOR AN INDIVIDUAL RESPONSE at NumCopiers = 6:\n\n")
predtime

cat("\nPREDICTION INTERVAL FOR AN EXPECTED RESPONSE, PER COPIER:\n\n")
meantime/6

# Working-Hotelling limits (confidence band for the regression line) at Xh = 100
df <- length(ServiceMinutes)-2        # Error degrees of freedom
fcrit <- qf(0.90,2,df)       # F* = critical, or cutoff, value from the F distribution
pred <- predict(fit,data.frame(NumCopiers=6),se.fit=TRUE)   # Predicted response at X=100
w <- sqrt(2*fcrit)           # W value in equations 2.40 and 2.40a, pg. 61
marg.err <- w*pred$se.fit    # Margin of error = everything after the +/- sign.

# Put the Working-Hotelling limit into a data frame, for display:
cat("\nWORKING-HOTELLING CONFIDENCE BAND (LIMITS) at NumCopiers = 6")
data.frame(fit=c(pred$fit),lower=c(pred$fit-marg.err),upper=c(pred$fit+marg.err))

aov = anova(fit)

corr.total = data.frame(cbind(c(sum(aov[,1])), c(sum(aov[,2]))), row.names="Corr. Total")
colnames(corr.total) = c("DF", "Sum Sq")

cat("\nANOVA TABLE:\n\n")
aov
corr.total

# Correlation between ServiceMinutes and NumCopiers
cat("\nR SQUARED:\n\n")
summary(fit)$r.squared

cat("\nCORRELATION COEFFICIENT:\n\n")
cor(ServiceMinutes,NumCopiers)

detach (copierdata)

