require(tfestimators)
require(keras)

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
train.nn.resp <- select(train,y) 
train.nn <- cbind(train.nn.feat,train.nn.resp)


# Utilisation tfestimators

response <- function() "y"
features <- function() names(select(train.nn,-y))

feature_columns <- feature_columns(column_numeric(features()))

classifier <- dnn_classifier(
  feature_columns = feature_columns,
  hidden_units = c(10, 20, 10),
  n_classes = 3
)
DNN_input_fn <- function(data) {
  input_fn(data, features = features(), response = response())
}

train(classifier, input_fn = DNN_input_fn(train.nn))
nn.prev <- predict(classifier, input_fn = DNN_input_fn(test.nn))
nn.eval <- evaluate(classifier, input_fn = DNN_input_fn(test.nn))


