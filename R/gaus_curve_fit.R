library(reshape2)

source('R/gaus.R')

beeSpp <- read.csv('data/Focus_Bees.csv', as.is = TRUE)
beeSpp <- unique(beeSpp$code)
beeAllDat <- read.csv('data/SEVBeeData2002-2015.csv', as.is = TRUE)

# reshape into long format
beeAllDat <- melt(beeAllDat, id.vars = c('Month', 'Year', 'Ecosystem', 'Transect', 'Direction', 'Color'),
                  variable.name = 'spp', value.name = 'abundance')

# subset to just focus species
beeAllDat <- beeAllDat[beeAllDat$spp %in% beeSpp, ]

# make month a factor ranging from 1 to 12 as a trick to create 0 abundances
# for unsampled months when using `aggregate` below
beeAllDat$Month <- factor(beeAllDat$Month, levels = 1:12)

# sum across `Direction` and `Color`
beeAllDat <- aggregate(list(abundance = beeAllDat$abundance),
                       beeAllDat[, c('Month', 'Year', 'Transect', 'spp')],
                       sum, drop = FALSE)
beeAllDat$abundance[is.na(beeAllDat$abundance)] <- 0

# recenter months to mid June
beeAllDat$Month <- as.integer(beeAllDat$Month)
beeAllDat$Month <- beeAllDat$Month - 6.5

# loop over each species
sapply(beeSpp[1:3], function(n) {
    dat <- beeAllDat[beeAllDat$spp == n, ]
    try(fitGaus(dat$Month, dat$abundance))
})


plot(beeAllDat[beeAllDat$spp == beeSpp[3], c('Month', 'abundance')], col = gray(0, alpha = 0.1), pch = 16)


plotByMonth <- function(X) {
    X <- X[abs(X[, 1]) <= 3.5, ]
    x <- X[, 1]
    y <- X[, 2]

    m <- sort(unique(x))

    n <- sum(x == x[1])
    plot(c(1, n), c(0, max(y)), type = 'n', log = 'x')

    mcols <- viridis::magma(length(m))
    names(mcols) <- as.character(m)

    for(i in m) {
        thisY <- sort(y[x == i], decreasing = TRUE)
        points(thisY, type = 'l', col = mcols[as.character(i)], lwd = 2)
    }
}

plotByMonth(beeAllDat[beeAllDat$spp == beeSpp[1], c('Month', 'abundance')])





