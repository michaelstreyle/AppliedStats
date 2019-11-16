
# Open the data file, using a file open dialog, select the file, CH01PR21.txt
brkgdata <- read.table(file.choose(),header=F,col.names=c("Breakage","Transfers"))

# This command allows us to use the variable names, X and Y, directly, without
# having to use the dataset name and $ as a prefix.
attach(brkgdata)

# Plot the data
plot(Transfers,Breakage)

fit <- lm (Breakage ~ Transfers)
fit
b1 <- fit$coeff[2]
b0 <- fit$coeff[1]
abline(b0,b1)

qt(.975, 8)
pt(1.810, 8, lower = FALSE)

cat("\nCONFIDENCE LIMITS FOR INTERCEPT AND SLOPE:\n\n")
confint(fit, level=0.95)

cat("\nHYPOTHESIS TESTS FOR INTERCEPT AND SLOPE:\n\n")
summary(fit)$coefficients

cat("\nCONFIDENCE LIMITS FOR INTERCEPT AND SLOPE:\n\n")
confint(fit, level=0.975)

meantime=predict(fit, data.frame(Transfers=c(2,4)), interval="confidence", se.fit=TRUE, level=0.99)$fit
row.names(meantime) = c("Transfers=2", "Transfers=4")
cat("\nCONFIDENCE INTERVAL FOR A MEAN RESPONSE at Transfers = 2 or 4:\n\n")
meantime

predtime = predict(fit, data.frame(Transfers=2), interval="prediction", se.fit=TRUE, level=0.99)$fit
cat("\nPREDICTION INTERVAL FOR AN INDIVIDUAL RESPONSE at Transfers = 2:\n\n")
predtime

# Prediction limits for the mean of m new predicted values at Xh = 100
#   Step 1. Get se.fit from a predict function result
#   Step 2. square that and add MSE/m
#   Step 3. Take the square root and multiple by t quantile.
#   Step 4. Add and subtract the marge of error from step 3 to/from the fitted value
pred=predict(fit, data.frame(Transfers=2), interval="confidence", se.fit=TRUE)
se.fit = pred$se.fit             # This pred comes from the saved pred above
mse = anova(fit)[2,3]            # MSE is in the 2nd row, 3rd column of the anova(fit) object (table)
var.predmean = se.fit^2 + mse/3     # m=3
marg.err2 = qt(0.995,23)*sqrt(var.predmean)     # 0.995 corresponds to 99% confidence
pred.fit2 = pred$fit[1]

# Present results as a table
cat("\nPREDICTION LIMITS FOR THE MEAN OF 3 NEW OBSERVATIONS at Transfers=2:\n\n")
data.frame(fit=c(pred.fit2),lower=c(pred.fit2-marg.err2),upper=c(pred.fit2+marg.err2))

# Present results as a table
cat("\nPREDICTION LIMITS FOR THE TOTAL OF 3 NEW OBSERVATIONS at Transfers=2:\n\n")
data.frame(fit=3*c(pred.fit2),lower=3*c(pred.fit2-marg.err2),upper=3*c(pred.fit2+marg.err2))

# Working-Hotelling limits (confidence band for the regression line) at Xh = 100
df <- length(Breakage)-2        # Error degrees of freedom
fcrit <- qf(0.99,2,df)       # F* = critical, or cutoff, value from the F distribution
pred <- predict(fit,data.frame(Transfers=c(2, 4)),se.fit=TRUE)   # Predicted response at X=100
w <- sqrt(2*fcrit)           # W value in equations 2.40 and 2.40a, pg. 61
marg.err <- w*pred$se.fit    # Margin of error = everything after the +/- sign.

# Put the Working-Hotelling limit into a data frame, for display:
cat("\nWORKING-HOTELLING CONFIDENCE BAND (LIMITS) at Transfers = 2 or 4")
data.frame(fit=c(pred$fit),lower=c(pred$fit-marg.err),upper=c(pred$fit+marg.err))

aov = anova(fit)

corr.total = data.frame(cbind(c(sum(aov[,1])), c(sum(aov[,2]))), row.names="Corr. Total")
colnames(corr.total) = c("DF", "Sum Sq")

cat("\nANOVA TABLE:\n\n")
aov
corr.total
qf(.95, 1, 8)
# Correlation between Breakage and Transfers
cat("\nR SQUARED:\n\n")
summary(fit)$r.squared

cat("\nCORRELATION COEFFICIENT:\n\n")
cor(Breakage,Transfers)

detach (brkgdata)

