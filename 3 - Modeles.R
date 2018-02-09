require(tidyverse)
require(randomForest)
require(sf)
require(bestglm)

load("Base.RData")
dat <- mutate(base,y=fusion/2)
summary(dat)

## On vire les variables sur lesquelles il y a trop de données manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>20)
dat <- dat[,-lesquelles]

# On remplace les NA par la moyenne et les NaN par le max 
# (c'est quand on a un zéro et on considère la dist comme max dan ces cas)
maxi <- sapply(dat[,-1], max,na.rm=T)
moy  <- sapply(dat[,-1], mean,na.rm=T)

for (ii in 2:ncol(dat))
{
  dat[is.na(dat[,ii]),ii] <- moy[ii-1]
  dat[is.nan(dat[,ii]),ii] <- maxi[ii-1]
}

logit <- glm(y~.,data=dat[,-1],family=binomial)
summary(dat)

