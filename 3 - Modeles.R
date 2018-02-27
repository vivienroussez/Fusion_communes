require(tidyverse)
require(randomForest)
require(sf)
require(bestglm)
require(glmnet)
require(leaps)

load("Base.RData")
table(base$fusion)
dat <- mutate(base)
summary(dat)

## On vire les variables sur lesquelles il y a trop de données manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>20)
dat <- dat[,-lesquelles]

num  <- select(dat,starts_with("dist"),starts_with("nb"))
fact <- select(dat,-starts_with("dist"),-starts_with("nb"))

# On remplace les NA par la moyenne et les NaN par le max 
# (c'est quand on a un zéro et on considère la dist comme max dan ces cas)
maxi <- sapply(num, max,na.rm=T)
moy  <- sapply(num, mean,na.rm=T)

for (ii in 1:ncol(num))
{
  dat[is.na(num[,ii]),ii] <- moy[ii]
  dat[is.nan(num[,ii]),ii] <- maxi[ii]
}

dat <- cbind(fact,num) %>% mutate(y=as.factor(fusion/2))

## On ne va prendre qu'un nombre restreint de ligne (tirées au hasard mais en gardant la proportion de fusion identique)
## Sinon, les modèles mettent des plombes à tourner. Quand tout sera calé, on balancera sur toute la base

set.seed(1234)

echant <- group_by(dat,y) %>%
          sample_frac(.1) %>% 
          as.data.frame() ## Attention, bestglm ne passe pas si on laisse en tibble !!!

row.names(echant) <- echant$ident
echant <- select(echant,-ident,-first,-second,-fusion)
XX <- model.matrix(y~.,data = echant)
y <- echant$y

logi  <- glm(y~.,family = binomial,data=echant)
bestlog <- step(logi)

lambda_lasso <- cv.glmnet(XX,echant$y,family="binomial",alpha=1)$lambda.1se
lambda_ridge <- cv.glmnet(XX,echant$y,family="binomial",alpha=0)$lambda.1se



