% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.intsel_cv.R
\name{predict.intsel_cv}
\alias{predict.intsel_cv}
\title{Predict Method for \code{intsel_cv}}
\usage{
\method{predict}{intsel_cv}(object, newx, type = "link", ...)
}
\arguments{
\item{object}{A fitted \code{intsel} object}

\item{newx}{Optional, a matrix in which to look for variables with which to predict. If ommitted, the original data is used.}

\item{type}{The type of prediction required. The default "\code{link}" is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable.}

\item{...}{Additional arguments passed to \code{\link{predict}}.}
}
\value{
A matrix containing the prediction.
}
\description{
description
Obtains predictions from a fitted \code{intsel_cv} object
}
\examples{
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

beta.true <- rep(0, p.main)
beta.true[c(1, 2, p.main)] <- 0.3
eta <- x \%*\% beta.true + 0.2 * x[, 1] * x[, 2]

if (intercept) eta <- eta + 0.1

py <- 1/(1 + exp(-eta))

y <- rbinom(n, 1, py)

nlam <- 30
lambdas <- exp(seq(log(0.1), log(0.00005), length.out = nlam))

# All the pairwise two-way interactions for the first p.screen variables 
# are included in the model and screened in a data-driven manner.
cv <- intsel_cv(x = x,
                y = y,
                p.screen =5,
                intercept = intercept,
                stepsize_init = 1,
                lambda = lambdas,
                nfolds = 5,
                foldid = NULL)
newx <- x[sample(1:nrow(x), size = 100), ]
pred.cv.newx <- predict(cv, newx = newx, type = "link")
dim(pred.cv.newx)
}
