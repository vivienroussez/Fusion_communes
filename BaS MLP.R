require(tfestimators)
require(keras)

load("Base.RData")

### Ceci est le bas à sable qui a servi à
### tester le réseau de neurones avant de le mettre dans le prog principal

# On transforme les var quali en dummies
dummies <- select_if(train,is.factor) %>%
  select(-y) %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  as.data.frame()

# On normalise les variables continues
num <- select_if(train,is.numeric) %>%
  scale %>% 
  as.data.frame()

train.nn.feat <- cbind(dummies,num) 
train.nn.resp <- train$y %>% as.numeric() 
train.nn <- cbind(train.nn.feat,train.nn.resp) %>% as.matrix()

test.nn.resp <- don.nn[bloc==bloc.actif,"y"] %>% as.numeric()
test.nn.feat <- don.nn[bloc==bloc.actif,-39] %>% as.matrix()

# Utilisation tfestimators

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
nn.prev <- predict(classifier, input_fn = DNN_input_fn(test.nn))
nn.eval <- evaluate(classifier, input_fn = DNN_input_fn(test.nn))


## Avec keras


monDNN <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(nrow(train.nn))) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

monDNN %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- monDNN %>% fit(
  train.nn.feat,
  train.nn.resp,
  epochs = 20,
  batch_size = 1000,
  validation_data = list(test.nn.feat, test.nn.resp)
)

## Adaboost
train <- baseML[1:90000,]
test <-baseML[90001:length(baseML),]
ada <- gbm(y~.,data=train,distribution="adaboost",
           interaction.depth=2,shrinkage=0.005,train.fraction=0.8,n.trees=2000)
iter <- which(ada$valid.error<0.01) %>% min()
ada <- gbm(y~.,data=train,distribution="adaboost",
           interaction.depth=2,shrinkage=0.005,n.trees=iter)
ada.prev <- predict(ada,test,n.trees=500) %>% as.data.frame()
names(ada.prev)[1] <- "ADABOOST"
