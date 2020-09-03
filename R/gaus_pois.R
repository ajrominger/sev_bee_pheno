gaus <- function(x, a1, a2, a3) {
    a1 * exp(-(x - a2)^2 / (2 * a3)^2)
}

gausLogLik <- function(p, x, y) {
    p[c(1, 3)] <- exp(p[c(1, 3)])

    a1 <- p[1]
    a2 <- p[2]
    a3 <- p[3]

    mu <- gaus(x, a1, a2, a3)

    return(-sum(dpois(y, mu, log = TRUE)))
}

fitGaus <- function(x, y) {
    p0 <- c(max(y), 0, 0.5)

    o <- optim(p0, gausLogLik, x = x, y = y, method = 'BFGS', hessian = TRUE)

    return(unlist(o[c('par', 'value', 'convergence')]))
}

gaus2 <- function(x, a1, a2, a3, b1, b2, b3) {
    a1 * exp(-(x - a2)^2 / (2 * a3)^2) + b1 * exp(-(x - b2)^2 / (2 * b3)^2)
}

gaus2LogLik <- function(p, x, y) {
    p <- exp(p)

    a1 <- p[1]
    a2 <- -p[2]
    a3 <- p[3]
    b1 <- p[4]
    b2 <- p[5]
    b3 <- p[6]

    mu <- gaus2(x, a1, a2, a3, b1, b2, b3)

    return(-sum(dpois(y, mu, log = TRUE)))
}

fitGaus2 <- function(x, y) {
    p0 <- c(max(y), -3, 0.5, max(y), 3, 0.5)

    o <- optim(p0, gaus2LogLik, x = x, y = y, method = 'BFGS')

    return(unlist(o[c('par', 'value', 'convergence')]))
}
