require(tidyverse)
require(randomForest)
require(sf)
require(bestglm)

load("Base.RData")
dat <- rename(base,y=fusion) 
summary(dat)

## On vire les variables sur lesquelles il y a trop de données manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>20)
dat <- dat[,-lesquelles]


logit <- bestglm(dat[,-c(1,2)],family=binomial  )
summary(dat)
