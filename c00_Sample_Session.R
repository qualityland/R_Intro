
# Appendix A, sample session
x <- rnorm(50)
y <- rnorm(x)
plot(x, y)
ls()
rm(x, y)


x <- 1:20
# weight vector of standard deviations
w <- 1 + sqrt(x) / 2
# data frame
dummy <- data.frame(x = x, y = x + rnorm(x) * w)
dummy
# fit linear regression, y dependent on x
fm <- lm(y ~ x, data = dummy)
summary(fm)

# fit a weighted linear regression
fm1 <- lm(y ~ x, data = dummy, weights = 1/w^2)
summary(fm1)

# make dataframe columns visible as variables
attach(dummy)

# non-parametric local regression function
lrf <- lowess(x, y)
plot(x, y)
# add local regression line
lines(x, lrf$y)
# add true regression line
abline(0, 1, lty=3)
# add unweighted regression line
abline(coef(fm))
# add weighted regression line
abline(coef(fm1), col = "red")

# remove data from search path
detach()

# standard regression diagnostic plot to check for heteroscedasticity  
plot(fitted(fm), resid(fm),
     xlab="Fitted values",
     ylab="Residuals",
     main="Residuals vs Fitted")

# normal scores plot to check for skewness, kurtosis and outliers
qqnorm(resid(fm), main="Residuals Rankit Plot")

# clean up
rm(fm, fm1, lrf, w, dummy)


filepath <- system.file("data", "morley.tab" , package="datasets")
filepath
