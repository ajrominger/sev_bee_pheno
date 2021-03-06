library(data.table)

source('R/gaus_pois.R')
source('R/gaus_negb.R')

# beeSpp <- fread('data/Focus_Bees.csv')
# beeSpp <- unique(beeSpp$code)
beeAllDat <- fread('data/SEVBeeData2002-2015.csv')

# reshape into long format
beeAllDat <- melt(beeAllDat, id.vars = c('Month', 'Year', 'Ecosystem', 'Transect', 'Direction', 'Color'),
                  variable.name = 'spp', value.name = 'abundance', variable.factor = FALSE)


# identify most abundant bees
beeAbund <- tapply(beeAllDat$abundance, beeAllDat$spp, sum)
beeSpp <- names(beeAbund[beeAbund >= 20])

# subset to just focus species
beeAllDat <- beeAllDat[beeAllDat$spp %in% beeSpp, ]

# make month a factor ranging from 1 to 12 as a trick to create 0 abundances
# for unsampled months when using `aggregate` below
# beeAllDat$Month <- factor(beeAllDat$Month, levels = 1:12)

# sum across `Direction` and `Color`
beeAllDat <- aggregate(list(abundance = beeAllDat$abundance),
                       beeAllDat[, c('Month', 'Year', 'Transect', 'spp')],
                       sum, drop = FALSE)
beeAllDat$abundance[is.na(beeAllDat$abundance)] <- 0

# recenter months to mid June
# beeAllDat$Month <- as.integer(beeAllDat$Month)
beeAllDat$Month <- beeAllDat$Month - 6.5

# loop over each species
beeNBMLE <- mclapply(beeSpp, mc.cores = 10, FUN = function(s) {
    dat <- beeAllDat[beeAllDat$spp == s, ]
    o <- try(fitGausNB(dat$Month, dat$abundance))

    if('try-error' %in% class(o)) {
        o <- rep(NA, 6)
    }

    return(o)
})

beeNBMLE <- do.call(rbind, beeNBMLE)



plotBee <- function(sp) {
    x <- beeAllDat[beeAllDat$spp == sp, c('Month', 'abundance')]

    y <- aggregate(list(n = x$abundance), list(Month = x$Month, abundance = x$abundance),
                   length)
    col <- quantCol(y$n, viridis(50), trans = 'log', xlim = c(1, 210))

    layout(matrix(1:2, nrow = 1), widths = c(3, 1))

    par(mar = c(2, 2, 1, 0) + 0.5, mgp = c(1.5, 0.5, 0), tcl = -0.25)
    plot(y[, 1:2], col = col, pch = 16, ylim = c(0, 5))

    a <- beeNBMLE[beeSpp == sp, ]
    curve(sqrt(gaus(x, a[1], a[2], a[3])), add = TRUE)

    mtext(sp)

    # scale
    par(mar = c(3, 1, 2, 3) + 0.5, mgp = c(2, 0.5, 0))
    plot(rep(1, 210), 1:210, type = 'n', xlim = 0:1, xaxs = 'i', yaxs = 'i',
         log = 'y', axes = FALSE, xlab = '')
    rect(xleft = 0, xright = 1, ybottom = 0:209, ytop = 1:210,
         col = quantCol(1:210, viridis(50), trans = 'log', xlim = c(1, 210)),
         border = NA)
    box()
    logAxis(4)
    mtext('Frequency', side = 4, line = 1.5)
}

pdf('all_pheno_gaus.pdf', width = 5, height = 4)
for(i in beeSpp) {
    print(i)
    plotBee(i)
}
dev.off()


plotByMonth <- function(X) {
    X <- X[abs(X[, 1]) <= 3.5, ]
    x <- X[, 1]
    y <- X[, 2]

    m <- sort(unique(x))

    n <- sum(x == x[1])
    plot(c(1, n), c(0, max(y)), type = 'n', log = 'x',
         xlab = 'Replicates', ylab = 'Abundance')

    mcols <- viridis::magma(length(m))
    names(mcols) <- as.character(m)

    for(i in m) {
        thisY <- sort(y[x == i], decreasing = TRUE)
        points(thisY, type = 'l', col = mcols[as.character(i)], lwd = 2)
    }
}

plotByMonth(beeAllDat[beeAllDat$spp == beeSpp[1], c('Month', 'abundance')])

# loop over all month/species combos and see what distributions look like
library(MASS)
library(parallel)

sppMonth <- unique(beeAllDat[, c('Month', 'spp')])

logLikSppMonth <- mclapply(1:nrow(sppMonth),
                           mc.cores = 10,
                           FUN = function(i) {
    sp <- sppMonth$spp[i]
    m <- sppMonth$Month[i]

    dat <- beeAllDat$abundance[beeAllDat$Month == m & beeAllDat$spp == sp]

    nbfit <- try(fitdistr(dat, 'negative binomial'), silent = TRUE)
    if('try-error' %in% class(nbfit)) {
        nbfit <- rep(NA, 3)
        names(nbfit) <- c('estimate.size', 'estimate.mu', 'loglik')
    }
    nbfit <- unlist(nbfit)[c('estimate.size', 'estimate.mu', 'loglik')]

    pofit <- try(fitdistr(dat, 'poisson'), silent = TRUE)
    if('try-error' %in% class(pofit)) {
        pofit <- rep(NA, 2)
        names(pofit) <- c('estimate.lambda', 'loglik')
    }
    pofit <- unlist(pofit)[c('estimate.lambda', 'loglik')]

    return(c(nbfit, pofit))
})

logLikSppMonth <- do.call(rbind, logLikSppMonth)
colnames(logLikSppMonth)[c(3, 5)] <- paste(colnames(logLikSppMonth)[c(3, 5)],
                                           c('nb', 'po'), sep = '.')
logLikSppMonth <- as.data.frame(logLikSppMonth, stringsAsFactors = FALSE)
logLikSppMonth$aic.nb <- -2 * (logLikSppMonth$loglik.nb - 2)
logLikSppMonth$aic.po <- -2 * (logLikSppMonth$loglik.po - 1)


plot(logLikSppMonth[, c('estimate.mu', 'estimate.size')], log = 'xy', col = 'gray')
points(logLikSppMonth[sppMonth$spp == beeSpp[13], c('estimate.mu', 'estimate.size')])


library(socorro)
library(viridis)
with(logLikSppMonth[logLikSppMonth$aic.nb < (logLikSppMonth$aic.po - 0), ], {
    # layout(matrix(1:2, nrow = 1))
    # par(mar = c(3, 3, 0, 0) + 0.5, mgp = c(2, 0.75, 0))

    plot(log(estimate.mu), log(estimate.size), #log = 'xy',
         col = quantCol(aic.po - aic.nb, viridis(20), trans = 'log'))
    # abline(lm(log(estimate.size) ~ (estimate.mu)))

    # plot(sort(aic.po - aic.nb),
    #      col = quantCol(sort(aic.po - aic.nb), viridis(20), trans = 'log'),
    #      xlab = '', xaxt = 'n', ylab = 'PO_AIC - NB_AIC', log = 'y')
})

mean(logLikSppMonth$aic.nb < (logLikSppMonth$aic.po - 0), na.rm = TRUE)
plot(sort(logLikSppMonth$aic.nb - logLikSppMonth$aic.po))

