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
