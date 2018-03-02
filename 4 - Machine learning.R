require(tidyverse)
require(randomForest)
require(sf)
require(bestglm) # Selection GLM
require(glmnet) # Elasticnet
require(GGally)
require(FactoMineR)
require(gbm)  # boosting
require(e1071) # SVM
require(rpart) # arbres
require(caret) # Package pour sélection de paramètre par CV
require(parallel)
require(keras)

load("Base.RData")
table(base$y)
dat <- base
summary(dat)

## On ne va prendre qu'un nombre restreint de ligne (tirées au hasard mais en gardant la proportion de fusion identique)
## Sinon, les modèles mettent des plombes à tourner. Quand tout sera calé, on balancera sur toute la base

set.seed(1234)

echant <- group_by(dat,y) %>%
  sample_frac(.1) %>% 
  as.data.frame() ## Attention, bestglm ne passe pas si on laisse en tibble !!!

row.names(echant) <- echant$ident

nbloc <- 12 # parce ce que j'ai 4 coeurs et on va faire les simul sur 3
bloc <- sample(1:nrow(echant)%%(nbloc+1))

bloc.actif <- 1  
don <- echant

# Création d'une fonction qui va implémenter les modèles pour chaque bloc de CV
# On l'excutera en parallèle une fois calée

predit <- function(bloc.actif,don=dat) # Prend en para le DF en entrée et le bloc sur lequel faire la prév
{
  train <- don[bloc!=bloc.actif,]
  test  <- don[bloc==bloc.actif,]
  
  XX <- model.matrix(y~.,data=train)
  YY <- train$y
  
  XX.test <- model.matrix(y~.,data=test)
  
  ### GLM
  mco <- glm(y~.,data=train,family = binomial)
  mco.prev <- predict(mco,newdata=test,type="response") %>% as.data.frame() 
  names(mco.prev)[1] <- "MCO"
  
  ### GLM avec choix des variables
  mco.step <- step(mco)
  mco.step.prev <- predict(mco.step,newdata=test,type="response") %>% as.data.frame() 
  names(mco.step.prev)[1] <- "MCO.step"
  
  ### Lasso
  lambda_lasso <- cv.glmnet(XX,YY,family="binomial",alpha=1)$lambda.1se ## Détermination auto du lambda
  lasso <- glmnet(XX,YY,family="binomial",alpha=1,lambda = lambda_lasso)
  lasso.prev <- predict(lasso,newx = XX.test,type="response") %>% as.data.frame() 
  names(lasso.prev)[1] <- "LASSO"
  
  ### Ridge
  lambda_ridge <- cv.glmnet(XX,YY,family="binomial",alpha=0)$lambda.1se ## Détermination auto du lambda
  ridge <- glmnet(XX,YY,family="binomial",alpha=0,lambda = lambda_ridge)
  ridge.prev <- predict(ridge,newx = XX.test,type="response") %>% as.data.frame() 
  names(ridge.prev)[1] <- "RIDGE"
  
  ### Elasticnet
  lambda_elastic <- cv.glmnet(XX,YY,family="binomial",alpha=.5)$lambda.1se ## Détermination auto du lambda
  elastic <- glmnet(XX,YY,family="binomial",alpha=.5,lambda = lambda_elastic)
  elastic.prev <- predict(elastic,newx = XX.test,type="response") %>% as.data.frame() 
  names(elastic.prev)[1] <- "ELASTIC"
  
  ### Arbre
  arbre <- rpart(y~.,data=train)
  arbre.prev <- predict(arbre,newdata = test,type="prob")[,2] %>% as.data.frame()
  names(arbre.prev) <- "ARBRE"
  
  ### Adaboost
  ada <- gbm(y~.,data=train,distribution="adaboost",
             interaction.depth=2,shrinkage=0.005,train.fraction=0.8,n.trees=100)
  iter <- gbm.perf(ada)
  ada <- gbm(y~.,data=train,distribution="adaboost",
             interaction.depth=2,shrinkage=0.005,n.trees=iter)
  ada.prev <- predict(ada,test,type="response",n.trees=iter) %>% as.data.frame()
  names(ada.prev)[1] <- "ADABOOST"
  
  ### Forêt
  rf <- randomForest(y~.,data=train,ntrees=500)
  rf.prev <- predict(rf,newdata = test,type = "prob")[,2] %>% as.data.frame()
  names(rf.prev)[1] <- "FORET"
  
  ### SVM
  best.svm <- tune(svm,y~.,data=train,kernel="linear",
                  ranges=list(cost=c(0.001,0.01,1,10,100,1000)),probability=T)
  summary(best.svm)
  svm.mod <- best.svm$best.model
  #svm.mod <- svm(y~.,data=train,probability=T,cost=0.001)
  svm.prev <- predict(svm.mod,newdata=test,probability=T)
  
  res <- data.frame(Y=test$y,SVM=svm.prev,GLM=mco.prev,BestGLM=mco.step.prev,lasso=lasso.prev,
                   ridge=ridge.prev)
  
}

predit(bloc.actif = 1,don=echant)


logi  <- glm(y~.,family = binomial,data=echant)
bestlog <- step(logi)
bestlog2 <- bestglm(echant,family = binomial)




