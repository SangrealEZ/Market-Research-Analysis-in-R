library(glmnet)

# Generate a simulated data for illustration purpose
set.seed(12345)
n <- 1000
p <- 100
true.x <- p / 10 # Only 10 true x, others are pure noises
x <- matrix(rnorm(n*p), n, p) # Data contains 100 xs
beta <- rnorm(true.x)
eps  <- rnorm(n) * 5
y <- drop(x[, 1:true.x] %*% beta + eps)

# Fit three shrinkage models
fit.lasso <- glmnet(x, y, family = "gaussian", alpha = 1)
fit.ridge <- glmnet(x, y, family = "gaussian", alpha = 0)
fit.elnet <- glmnet(x, y, family = "gaussian", alpha = 0.5)
# Summary of steps
print(fit.lasso)
print(fit.ridge)
print(fit.elnet)
# Get the coefficients at given lambda value(s)
coef(fit.lasso, s = 0.1)
coef(fit.ridge, s = c(0.1, 0.5))
coef(fit.elnet, s = 0.5)
# Plots 
par(mfrow = c(2, 2))
plot(fit.lasso, xvar = "lambda", label = TRUE, main = "Lasso")
plot(fit.ridge, xvar = "lambda", label = TRUE, main = "Ridge")
plot(fit.elnet, xvar = "lambda", label = TRUE, main = "Elastic-net")
# Can use ?plot.glmnet to find more options

# cross validation to find the optimal lambda
# Can change number of folds by altering the argument: nfold =
cv.lasso <- cv.glmnet(x, y, family = "gaussian", alpha = 1)
cv.ridge <- cv.glmnet(x, y, family = "gaussian", alpha = 0)
cv.elnet <- cv.glmnet(x, y, family = "gaussian", alpha = 0.5)
# Check the plot to find the optimal value of lambda (or a range)
plot(cv.lasso) # Optimal value shown at the vertical line
plot(cv.ridge)
plot(cv.elnet)
# Get the optimal lambda values
print(cv.lasso$lambda.min)
print(cv.lasso$lambda.1se)
# What is the model coefs at the optimal lambda value?
coef(cv.lasso, s = "lambda.min") # alternative would be "lambda.1se"

# How to get the optimal value of alpha?
# Choose between lasso, ridge and elastic-net 
# Again use cross validation to compare different values of alpha and judge based on MSE
# Try alpha from 0 to 1 with interval of 0.1
alphas <- seq(from = 0, to = 1, by = 0.1)
mses   <- rep(NA, length(alphas))
for (i in 1:length(alphas)) {
    cv.fit <- cv.glmnet(x, y, family = "gaussian", alpha = alphas[i])
    mses[i] <- cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.min]
}
output <- cbind(alphas, mses)
print(output)
# 0.2 is the optimal one in this case