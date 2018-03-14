res <- ""
for (ii in 2:length(colnames(XX)))
{
  res <- paste(res,colnames(XX)[ii],sep="+")
}

# On transforme les var quali en dummies
dummies <- select_if(train,is.factor) %>%
  select(-y) %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  as.matrix()

# On normalise les variables continues
num <- select_if(train,is.numeric) %>%
  mutate_all(scale) %>% 
  as.matrix()

train.nn.feat <- cbind(dummies,num)
train.nn.resp <- select(train,y) %>%   
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  as.matrix() %>%
  to_categorical()

