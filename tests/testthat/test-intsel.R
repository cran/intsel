n <- 1000
p.int <- 5
p.noint <- 3
intercept <- TRUE
p.screen <- 5

p.int.expand <- p.int*(p.int-1)/2
p.main <- p.int + p.noint
x <- matrix(rnorm(n * p.main), nrow = n, ncol = p.main)

# true model
# logit(p) = 0.1 + 0.3 x1 + 0.3 x2 + 0.3 x8 + 0.2 * x1 * x2


# set.seed(1)
beta.true <- rep(0, p.main)
beta.true[c(1, 2, p.main)] <- 0.3
eta <- x %*% beta.true + 0.2 * x[, 1] * x[, 2]

if (intercept) eta <- eta + 0.1

py <- 1/(1 + exp(-eta))

y <- rbinom(n, 1, py); table(y)

if (intercept) {
  glm(y~x + x[,1]:x[,2], family = binomial())
} else {
  glm(y~x + x[,1]:x[,2] - 1, family = binomial())
}


nlam <- 30
lambdas <- exp(seq(log(0.1), log(0.00005), length.out = nlam))

# All the pairwise two-way interactions for the first p.screen variables 
# are included in the model and screened in a data-driven manner.
fit <- intsel(x = x,
              y = y,
              p.screen = 5,
              intercept = intercept,
              lambda = lambdas)
fit$iterations
plot(fit)

cv <- intsel_cv(x = x,
                y = y,
                p.screen =5,
                intercept = intercept,
                stepsize_init = 1,
                lambda = lambdas,
                nfolds = 5,
                foldid = NULL)

plot(cv)
plot(cv, type = "solution-path")

(estimates.min <- cv$intsel.fit$estimates[, cv$index["min",]])
(estimates.1se <- cv$intsel.fit$estimates[, cv$index["1se",]])


pred.cv <- predict(cv, type = "response")
dim(pred.cv)

newx <- x[sample(1:nrow(x), size = 100), ]
pred.cv.newx <- predict(cv, newx = newx, type = "link")
dim(pred.cv.newx)



pred <- predict(fit, type = "response")
dim(pred)

newx <- x[sample(1:nrow(x), size = 100), ]
pred.newx <- predict(fit, newx = newx, type = "link")
dim(pred.newx)
