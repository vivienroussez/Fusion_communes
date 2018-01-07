require(tidyverse)
require(spdep)
require(sf)
require(data.table)
require(FactoMineR)
require(parallel)
require(foreach)
require(doParallel)

datacomm <- fread("Sources/Base_communes.csv",sep=";",dec=",",colClasses = c("REG"="chr"))
mapCom <- st_read("Sources_orig/COMMUNE.shp")

mapCom <- select(mapCom,-ID_GEOFLA, -starts_with("CODE"), -POPULATION,-starts_with("NOM")) %>%
       rename(codgeo=INSEE_COM)
contig <- st_intersects(mapCom,mapCom) # Matrice de contiguité

# On enlève le premier élément qui est la commune elle-même
#contig <- lapply(contig,function(x) x[-1]) 

# On retire les iles monocommunales qui vont poser pb par la suite...
iles <- which(sapply(contig,length) ==1) 
mapCom  <- mapCom[-iles,]

# On refait la matrice de contiguité sur ce nouvel ensemble de communes
contig <- st_intersects(mapCom,mapCom) 
#contig <- lapply(contig,function(x) x[-1]) 

#contig <- poly2nb(mapCom,row.names = mapCom$INSEE_COM,queen = T)

# Vérification de la contiguité
plot(mapCom[contig[[1]],3],col="blue")
plot(mapCom[contig[[1]][[1]],3],col="black",add=T)

# On va transformer la liste en dataframe à 2 colonnes avec chaque couple de communes contigues
couples <- data.frame()
for (ii in 1:length(contig))
{
  if (ii %% 1000==0) print(ii)
  aa <- expand.grid(ii,contig[[ii]])  %>% filter(Var1 != Var2)
  couples <- rbind(couples,aa)
}

# On crée un identifiant par couple (identique pour le couple A,B et B,A)
couples$first <- ifelse(couples[,1]<couples[,2],yes = couples[,1],no=couples[,2])
couples$second <- ifelse(couples[,1]<couples[,2],yes = couples[,2],no=couples[,1])
couples$ident <- paste(couples$first,couples$second,sep="_")

# Création d'une table avec un unique couple (qui sera la base finale)
doublons <- which(duplicated(couples$ident))
uniques <- couples[-doublons,]

# On va calculer une distance entre chaque couple de communes contigues
# Sur chacune des variables de la base datacomm
datacomm <- match(mapCom$codgeo,datacomm$CODGEO) %>% datacomm[.,]
dat <- select_if(datacomm,is.numeric)

calculeDist <- function(couple)
{
  sel <- dat[as.numeric(couple),]
  aa <- apply(sel,MARGIN = 2,dist)   %>% as.numeric()
  aa <- c(as.numeric(couple),aa) # %>% t() %>% as.data.frame()
  return(aa)
}
calculeDist(uniques[1,c("first","second")]) 

cl <- makeCluster(3)
clusterEvalQ(cl,require(dplyr))
clusterExport(cl,c("uniques","calculeDist","dat"))
distances <- parApply(cl,uniques[,c("first","second")],MARGIN = 1,calculeDist)
stopCluster(cl)
rm(cl)

distances <- t(distances) %>% as.data.frame()
names(distances) <- c("first","second",paste("dist",names(dat),sep="_"))
head(distances,1000) %>% View()


