require(tidyverse)
require(spdep)
require(sf)
require(data.table)
require(FactoMineR)

datacomm <- fread("Sources/Base_communes.csv",sep=";",dec=",",colClasses = c("REG"="chr"))
mapCom <- st_read("Sources_orig/COMMUNE.shp")

mapCom <- select(mapCom,-ID_GEOFLA, -starts_with("CODE"), -POPULATION,-starts_with("NOM")) %>%
          rename(codgeo=INSEE_COM) %>% 
          arrange(codgeo)
contig <- st_intersects(mapCom,mapCom)      # Matrice de contiguité
contig <- lapply(contig,function(x) x[-1])  # On enlève le premier élément qui est la commune elle-même

iles <- which(sapply(contig,length) ==0)    # On retire les iles monocommunales
contig <- contig[-iles]
mapCom  <- mapCom[-iles,]
contig <- st_intersects(mapCom,mapCom)      # On refait la matrice de contiguité sans les iles

datacomm <- match(mapCom$codgeo,datacomm$CODGEO) %>% datacomm[.,]

# Vérification de la contiguité
# Dans la liste, la commune i figure dans la liste de celles qui lui sont contigues
# Illustration :
plot(mapCom[contig[[1]],3],col="blue")
plot(mapCom[contig[[1]][1],3],add=T,col="black")


df_contig <- data.frame()
selection <- select_if(datacomm,is.numeric) %>%
              select(-SUPERF)              # Sélection des variables pour calculer la distance 
acp <- PCA(selection,ncp=5,graph = F)      # ACP pour calcul des distances sur coord factorielles

for (ii in 1:length(contig))
{
  print(ii)
  aa <- expand.grid(ii,contig[[ii]])
  dd <- acp$ind$coord[c(ii,contig[[ii]]),] %>% dist() %>% as.matrix() # On calcule la distance entre les communes
                                                                      # Sur la base des indicateurs de datacomm
  aa <- cbind(aa,dd[-1,1])
  df_contig <- rbind(df_contig,aa) 
}
rm(aa,ii,dd)
names(df_contig) <- c("codgeo1","codgea2","dissim")

