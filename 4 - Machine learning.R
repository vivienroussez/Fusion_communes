require(tidyverse)
require(randomForest)
require(sf)
# require(bestglm) # Selection GLM
require(glmnet) # Elasticnet
require(GGally)
require(FactoMineR)
require(gbm)  # boosting
require(e1071) # SVM
require(rpart) # arbres
require(caret) # Package pour sélection de paramètre par CV
require(parallel)
require(foreach)
require(doParallel)
require(tfestimators) # tensorFlow
require(keras)
require(kernlab) # SVM
#install_keras()


load("Base.RData")
base <- rename(base,y=fusion)
table(baseML$y)
dat <- baseML
summary(dat)

## On ne va prendre qu'un nombre restreint de ligne (tirées au hasard mais en gardant la proportion de fusion identique)
## Sinon, les modèles mettent des plombes à tourner. Quand tout sera calé, on balancera sur toute la base

set.seed(1234)

echant <- group_by(dat,y) %>%
  sample_frac(.01) %>% 
  as.data.frame() ## Attention, bestglm ne passe pas si on laisse en tibble !!!

row.names(echant) <- echant$ident

nbloc <- 12 # parce ce que j'ai 4 coeurs et on va faire les simul sur 3. Sur le serveur il y a 8 coeurs
            # donc on fait 21 blocs (qui est le ppmc(7,3))


# bloc.actif <- 1  
don <- dat
bloc <- 1+sample(1:nrow(don)%%(nbloc))
table(bloc)


### Mise en forme des données pour tensorflow
# On transforme les var quali en dummies
dummies <- select_if(don,is.factor) %>%
  select(-y) %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  as.data.frame()
# On normalise les variables continues
num <- select_if(don,is.numeric) %>%
  scale %>% 
  as.data.frame()

feat <- cbind(dummies,num)
resp <- select(don,y) 
don.nn <- cbind(feat,resp)

# Création d'une fonction qui va implémenter les modèles pour chaque bloc de CV
# On l'excutera en parallèle une fois calée

predit <- function(bloc.actif,don=dat) # Prend en para le DF en entrée et le bloc sur lequel faire la prév
{
  print(bloc.actif)
  train <- don[bloc!=bloc.actif,]
  test  <- don[bloc==bloc.actif,]
  
  train.nn <- don.nn[bloc!=bloc.actif,]
  test.nn <- don.nn[bloc==bloc.actif,]
  
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
  
  ## Adaboost
  ada <- gbm(y~.,data=train,distribution="adaboost",
             interaction.depth=2,shrinkage=0.005,train.fraction=0.8,n.trees=2000)
  iter <- which(ada$valid.error<0.01) %>% min() # on prend le nombre d'itération tel qu'on passe à une 
                                                # erreur inf à 1% (sinon, le min est le dernier point dans notre cas, l'erreur decroit tout le temps)
  ada <- gbm(y~.,data=train,distribution="adaboost",
             interaction.depth=2,shrinkage=0.005,n.trees=iter)
  ada.prev <- predict(ada,test,n.trees=iter) %>% as.data.frame()
  names(ada.prev)[1] <- "ADABOOST"
  
  ### Forêt
  rf <- randomForest(y~.,data=train,ntrees=500)
  rf.prev <- predict(rf,newdata = test,type = "prob")[,2] %>% as.data.frame()
  names(rf.prev)[1] <- "FORET"
  
  ### SVM linéaire
  # best.svm <- tune(svm,y~.,data=train,kernel="linear",
  #                 ranges=list(cost=c(0.001,0.01,1,10,100,1000)),probability=T)
  # summary(best.svm$best.model)
  # svm.mod <- best.svm$best.model
  # svm.prev <- predict(svm.mod,newdata=test,probability=T)
  # 

  ### SVM avec kernel radial
  # best.svm.rad <- tune(svm,y~.,data=train,kernel="radial",
  #                  ranges=list(cost=c(0.001,0.01,1,10,100,1000)),probability=T)
  # summary(best.svm.rad$best.model)
  # svm.mod.rad <- best.svm.rad$best.model
  # svm.prev.rad <- predict(svm.mod.rad,newdata=test,decision.values=T,probabilities=T)
  # svm.prev.rad <- attr(svm.prev.rad,"decision.values")
  
  # Réseaux de neurone
  response <- function() "y"
  features <- function() names(select(train.nn,-y))

  feature_columns <- feature_columns(column_numeric(features()))

  classifier <- dnn_classifier(
    feature_columns = feature_columns,
    hidden_units = c(10, 20, 10),
    n_classes = 2
  )
  DNN_input_fn <- function(data) {
    input_fn(data, features = features(), response = response())
  }

  train(classifier, input_fn = DNN_input_fn(train.nn))
  nn.prev <- predict(classifier, input_fn = DNN_input_fn(test.nn),predict_keys="probabilities")
  nn.prev <- sapply(nn.prev$probabilities,function(x) x[2])
  nn.eval <- evaluate(classifier, input_fn = DNN_input_fn(test.nn))


  prev <- data.frame(Y=test$y,GLM=mco.prev,BestGLM=mco.step.prev,lasso=lasso.prev,ridge=ridge.prev,elastic=elastic.prev,
                   DNN=nn.prev,ada=ada.prev,arbre=arbre.prev,foret=rf.prev)
  
  # prev <- data.frame(SVM.lin=svm.prev,SVM.rad=svm.prev.rad)
  
  # modeles <- list(MCO=mco,MCO.step=mco.step,RIDGE=ridge,LASSO=lasso,ELAST=elastic,ARBRE=arbre,BOOST=ada,FORET=rf,DNN=nn.eval)
  res <- prev
  return(res)
  
}
#,SVM=svm.prev
# system.time(
#   toto <- predit(bloc.actif = 1,don=don)
# )


cl <- makeCluster(detectCores()-1) # ouverture du cluster
registerDoParallel(cl)

# Exécution en parallèle

system.time(
  prev <- foreach(ii=1:nbloc,.export = c("predit"), 
                .packages = c("dplyr","gbm","tfestimators","e1071","randomForest","rpart","glmnet"))  %dopar%
      predit(bloc.actif = ii,don=don)
)

stopCluster(cl) # arrêt du cluster
prev.data <- bind_rows(prev)
