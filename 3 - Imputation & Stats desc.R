require(tidyverse)
require(FactoMineR)

load("Base.RData")
table(base$fusion)
dat <- base
summary(dat)

## On vire les variables sur lesquelles il y a trop de données manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>2000)
dat <- dat[,-lesquelles]

num  <- select(dat,starts_with("dist"),starts_with("nb")) 
fact <- select(dat,-starts_with("dist"),-starts_with("nb"))

# On remplace les NA par la moyenne et les NaN par le max 
# (c'est quand on a un zéro et on considère la dist comme max dan ces cas)
maxi <- sapply(num, max,na.rm=T)
moy  <- sapply(num, mean,na.rm=T)

for (ii in 1:ncol(num))
{
  num[is.na(num[,ii]),ii] <- moy[ii]
  num[is.nan(num[,ii]),ii] <- maxi[ii]
}


## Matrice de corrélation et ACP pour description rapide
cor(num)
#acp <- PCA(num)

dat <- cbind(fact,num) %>% 
  mutate(y=as.factor(fusion)) %>%
  select(-ident,-first,-second,-starts_with("fusion"))
row.names(dat) <- fact$ident

baseML <- dat
save(base,baseML,mapCom,couples,file="Base.RData")
