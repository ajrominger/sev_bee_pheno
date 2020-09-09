expitSpecial <- function(x) {
    (1 / (1 + exp(-x)) - 0.5) * 3.5 / 0.5
}

gausNBLogLik <- function(p, x, y) {
    # force to be positive
    p[c(1, 3, 4)] <- exp(p[c(1, 3, 4)])

    # force to be in [-3.5, 3.5]
    p[2] <- expitSpecial(p[2])

    a1 <- p[1]
    a2 <- p[2]
    a3 <- p[3]
    k <- p[4]

    mu <- gaus(x, a1, a2, a3)

    return(-sum(dnbinom(y, size = k, mu = mu, log = TRUE)))
}

fitGausNB <- function(x, y) {
    p0 <- c(log(max(y)), 0, log(1), log(1))

    o <- optim(p0, gausNBLogLik, x = x, y = y, method = 'BFGS', hessian = TRUE)

    # back transform
    o$par[c(1, 3, 4)] <- exp(o$par[c(1, 3, 4)])
    o$par[2] <- expitSpecial(o$par[2])

    return(unlist(o[c('par', 'value', 'convergence')]))
}




# gausNBLogLik <- function(p, x, y) {
#     a1 <- p[1]
#     a2 <- p[2]
#     a3 <- p[3]
#     k <- p[4]
#
#     mu <- gaus(x, a1, a2, a3)
#
#     o <- dnbinom(y, size = k, mu = mu, log = TRUE)
#     o[!is.finite(o)] <- -.Machine$integer.max
#
#     return(-sum(o))
# }
#
# fitGausNB <- function(x, y) {
#     p0 <- c(max(y) / 2, 0, 1, 1)
#
#     nbk <- MASS::fitdistr(y, 'negative binomial')$estimate['size']
#     # peak <- x[which.max(y)]
#     #
#     # lwr <- c(0.1, peak - 1.5, 0.1, nbk * 0.25)
#     # if(lwr[2] < -3.5) lwr[2] <- -3.5
#     #
#     # upr <- c(max(y), peak + 1.5, 10, nbk * 1.25)
#     # if(upr[2] > 3.5) upr[2] <- 3.5
#
#     lwr <- c(-3.5, 0.1, 0.01, 0.01)
#     upr <- c(3.5, 1000, 5, 100)
#
#     o <- optim(p0, gausNBLogLik, x = x, y = y, method = 'L-BFGS-B',
#                lower = lwr, upper = upr)
#
#     return(unlist(o[c('par', 'value', 'convergence')]))
# }
#
#
#
#
# aa <- c(40, 0.5, 1)
# k <- 10
# x <- rep(3:10, 25) - 6.5
# yhat <- gaus(x, aa[1], aa[2], aa[3])
# y <- rnbinom(length(x), size = k, mu = yhat)
# plot(x, y)
#
# gausNBLogLik(c(aa, k), x, y)
# fitGausNB(x, y)
#
