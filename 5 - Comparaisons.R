require(tidyverse)
require(sf)
require(pROC)
require(data.table)
require(caret)
require(rpart)
require(visNetwork)

load("Base.RData")

arbres <- read.csv("PrevML_ARBRES.csv")
prev.DNN <- read.csv("PrevML_DNN.csv")[,-1] %>% as.data.frame() 
# prev.reste <- read.csv("PrevML_REG_ARBRES.csv")[,-1] %>% as.data.frame() %>%
#   mutate(Y=as.factor(Y)) %>% select(-arbre,-rf)

prev <- cbind(arbres[,-1],dnn[,-1])

# prev <- cbind(prev.reste,prev.DNN)

prev.roc  <- lapply(prev[,-1],function(x) roc(prev$Y,x))
prev.bin <- lapply(prev[,-1],function(x) (x>.5)+0) %>% as.data.frame()
prev.conf <- lapply(prev.bin,function(x) confusionMatrix(x,prev$Y))

par(mfrow=c(2,2))
lapply(prev.roc,plot)
lapply(prev[,-1],function(x) plot(density(x),col="blue"))
lapply(prev,function(x) sum(x>.5))

arbre <- rpart(data=baseML,y~.)
visTree(arbre)

glm(data=baseML,formula = y~.,family=binomial) %>% summary()
summary(g)

save(prev,prev.roc,prev.conf,file="DiagML.RData")

## Exploitation des résultats pour comparaison des modèles

plot(prev.roc$MCO.step)

par(mfrow=c(3,3))
lapply(prev.roc, plot)
par(mfrow=c(1,1))

plot(prev.roc$MCO)
str(prev.roc[[1]])
  
ggplot(prev,aes(x=DNN)) + geom_density()

for (ii in seq(.1,.9,.1))
{
  (prev$MCO>ii) %>% sum() %>% print()
}
