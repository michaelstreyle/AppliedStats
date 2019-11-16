# Regression through the origin
# Select table CH04TA02.txt

reg0 = read.table (file.choose(), col.names=c("work.units", "labor.cost"))
attach (reg0)

# Regular linear fit

reg0.fit1 = lm (labor.cost ~ work.units)
plot (work.units, labor.cost, xlim=c(0,200), ylim=c(0,1000))
abline (reg0.fit1)
pred50 = predict (reg0.fit1, data.frame(work.units=c(50,100)), interval='prediction')
pred50

reg0.fit2 = lm (labor.cost ~ 0 + work.units)
abline (reg0.fit2, col=2)
pred50.0 = predict (reg0.fit2, data.frame(work.units=c(50,100)), interval='confidence')
pred50.0
