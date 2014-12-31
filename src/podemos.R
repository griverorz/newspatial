## Analysis of the location of PODEMOS
## Gonzalo
## Sun Dec 28 15:23:09 CET 2014

library(ggplot2)
library(rjags)
library(reshape2)

setwd("~/Documents/datablog/podemos/")

podem <- read.csv("dta/DA3041.csv")

## Unfactor the main variable
ideonum <- function(x) {
    levels(x) <- gsub("[A-Za-z ]", "", levels(x))
    levels(x) <- gsub("\\.\\.", "-1", levels(x))
    x <- as.numeric(as.character(x))
    x[x == -1] <- NA
    return(x)
}

## Data cleaning
## Ideology - individual 
ideo <- podem$P25
ideo <- ideonum(ideo)

## Simpatia
simpatia <- as.character(podem$P24)
simpatia <- gsub("IU.*", "IU", podem$P24)
simpatia <- model.matrix(~ simpatia - 1)
simpatia <- simpatia[, c("simpatiaIU", "simpatiaPodemos", "simpatiaPSOE", "simpatiaPP")]

## Ideology - party
pideo <- podem[, c("P2603", "P2605", "P2602", "P2601")]
pideo <- sapply(pideo, ideonum)

## Voto
voto <- podem$P23

## Distance
distance <- (ideo - pideo)

## Simpatia only
local <- proxim <- vector('list', ncol(simpatia))
for (i in 1:ncol(distance)) {
    proxim[[i]] <- data.frame("distance" = distance[simpatia[, i] == 1, i],
                              "party" = c("IU", "Podemos", "PSOE", "PP")[i])
    local[[i]] <- data.frame("local" = ideo[simpatia[, i] == 1],
                              "party" = c("IU", "Podemos", "PSOE", "PP")[i])
}

proxim <- do.call(rbind, proxim)
local <- do.call(rbind, local)

#################### Model ####################
jinits <- function() {
    list("beta" = rnorm(1, 0, 5),
         "cons" = rnorm(4, 0, 5))
}


## Remove incomplete cases
keep <- complete.cases(distance)
distance <- distance[keep,]
simpatia <- simpatia[keep, ]

## Remove non-voters
keep <- rowSums(simpatia) == 1
simpatia <- simpatia[keep, ]
distance <- distance[keep, ]

jdata <- list("N"=nrow(simpatia),
              "K"=ncol(simpatia),
              "simpatia"=simpatia,
              "ideo"=abs(distance))


jmodel <- jags.model("src/clogit.jags", 
                     ## inits = jinits,
                     data = jdata,
                     n.adapt = 1E3)

jfit <- coda.samples(jmodel,
                     variable.names = c("beta", "cons"),
                     n.iter = 5E3,
                     n.thin = 5)

prepare_data <- function(x) {
    if (length(x) > 1) {
        stop("Cannot handle more than one chain")
    }
    dnames <- attr(x[[1]], 'dimnames')[[2]]
    x <- data.frame(x[[1]])
    names(x) <- dnames
    return(x)
}

jfit <- prepare_data(jfit)

predict_model <- function(x, data) {
    p <- matrix(NA, nrow=nrow(x), ncol=ncol(x) - 1)
    for (i in 1:nrow(x)) {
        for (k in 1:(ncol(x) - 1)) {
            p[i, k] <- exp(x[i, 1]*data[k] + x[i, k + 1])
        }
    }
    p <- apply(p, 1, function(x) x/sum(x))
    return(t(p))
}

## Catch area of parties
ploc <- as.vector(by(local$local, local$party, function(x) mean(x, na.rm=TRUE)))
simul <- Map(function(x) abs(x - ploc), seq(1, 10, by=0.2))

out <- lapply(simul, function(x) predict_model(jfit, x))
out <- t(sapply(out, colMeans))

out <- data.frame(out)
names(out) <- c("IU", "Podemos", "PSOE", "PP")
out <- melt(out)
out$ideo <- seq(1, 10, by=0.2)

p <- ggplot(out, aes(x=ideo, y=value, group=variable))
pq <- p + geom_density(stat='identity', aes(colour=variable)) +
    labs(title="Party of choice by ideology",
         x="Ideology",
         y="Probability") + 
    scale_color_discrete(name="Party")
ggsave('./img/pvote.png', pq)

## Location of parties
theme_set(theme_bw())
p <- ggplot(local, aes(x=local, group=party, fill=party))
pq <- p + geom_histogram(position='dodge', binwidth=1,
                   aes(group=party, colour=party, y=..density..)) +
                   facet_wrap(~ party) +
                   labs(x="Ideology", y="Density") +
                   scale_fill_discrete(name="Party") +
                   scale_colour_discrete(name="Party")
ggsave('./img/location.png', pq)

