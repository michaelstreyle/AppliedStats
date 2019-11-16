# Open the data file, using a file open dialog, select the file, CH01PR19.txt
ds <- read.table(file.choose("CH01PR20.txt"),header=F,col.names=c("minutes","machine"))

# This command allows us to use the variable names, X and Y, directly, without
# having to use the dataset name and $ as a prefix.
attach(ds)

# Plot the data
plot(machine,minutes)

fit <- lm (minutes ~ machine)
fit
b1 <- fit$coeff[2]
b0 <- fit$coeff[1]
abline(b0,b1)

cat("\nCONFIDENCE LIMITS FOR INTERCEPT AND SLOPE:\n\n")
confint(fit, level=0.99)

cat("\nHYPOTHESIS TESTS FOR INTERCEPT AND SLOPE:\n\n")
summary(fit)$coefficients

cat("\nCONFIDENCE INTERVAL FOR A MEAN RESPONSE:\n\n")
predict(fit, data.frame(machine=43), interval="confidence", se.fit=TRUE, level=0.9)$fit

cat("\nPREDICTION INTERVAL FOR AN INDIVIDUAL RESPONSE:\n\n")
predict(fit, data.frame(ACT=28), interval="prediction", se.fit=TRUE, level=0.9)$fit

aov = anova(fit)

corr.total = data.frame(cbind(c(sum(aov[,1])), c(sum(aov[,2]))), row.names="Corr. Total")
colnames(corr.total) = c("DF", "Sum Sq")

cat("\nANOVA TABLE:\n\n")
aov
corr.total

# Correlation between GPA and ACT
cat("\nCORRELATION COEFFICIENT:\n\n")
cor (GPA, ACT)

detach (ds)

