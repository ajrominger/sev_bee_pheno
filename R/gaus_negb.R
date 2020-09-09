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
