require(tidyverse)
require(sf)
require(pROC)
require(data.table)
require(caret)

prev <- fread("PrevML.csv",sep=";",dec=',')[,-1] %>% as.data.frame() %>%
      mutate(Y=as.factor(Y))

prev.factor <- select(prev,-Y) %>% mutate_all(function(x) as.factor((x>.5)+0))

prev.roc  <- lapply(prev[,-1],function(x) roc(prev$Y,x))
prev.conf <- lapply(prev.factor, function(x) confusionMatrix(x,prev$Y))

save(prev,prev.roc,prev.conf,file="DiagML.RData")

## Exploitation des résultats pour comparaison des modèles

plot(prev.roc$MCO.step)

par(mfrow=c(3,3))
lapply(prev.roc, auc)
par(mfrow=c(1,1))

plot(prev.roc$MCO)
str(prev.roc[[1]])

ggplot(prev,aes(x=DNN)) + geom_density()

for (ii in seq(.1,.9,.1))
{
  (prev$MCO>ii) %>% sum() %>% print()
}
