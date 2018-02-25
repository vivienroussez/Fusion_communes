# require(devtools)
# devtools::install_github("antuki/COGugaison")

require(COGugaison)
require(tidyverse)
require(FactoMineR)

elect <- read.csv("Sources/Elections.csv",sep=";",skip=2,header = T,dec=',') %>%
        select(-starts_with("pct_"),-starts_with("prenom")) 

# on repère les variables quali des variables de comptage pour la suite (changement géo)
var_nom <- grep("nom",names(elect))
var_voix <- grep("voix",names(elect))
var_nom_lib <- names(elect)[var_nom]
var_voix_lib <- names(elect)[var_voix]


# Changement de géogprahie : on passe du COG2017 au COG2015
noms <- elect[,c(3,var_nom)] %>% 
      changement_COG_typo(typos=var_nom_lib,codgeo_entree = "codgeo",annees = 2017:2015) 

voix <- elect[,c(3,var_voix)] %>% 
        changement_COG_varNum(var_num = var_voix_lib,codgeo_entree = "codgeo",annees = 2017:2015)


# On reformate la table pour avoir un format "long". 
# On procède séparément pour les 2 variables quali latentes (candidats et nb votes)
dat <- merge(noms,voix,by.x="codgeo",by.y="codgeo")
noms <- select(dat,codgeo,starts_with("nom")) %>%  gather(key = "nom",value = "candidat",-codgeo)
voix <- select(dat,codgeo,starts_with("voix")) %>%  gather(key = "voix",value = "nombre_voix",-codgeo)
# Rapprochement des 2 
dat <- cbind(noms,voix)[,-4]
dat <- dat %>% filter( !substr(dat$codgeo,1,2) %in% c("ZZ","97")) 

biz <- grep("et",dat$candidat) # la fonction de COGugaison a donné des modalités nouvelles... On les enlève
dat <- dat[-biz,]

# On fait le tableau de contingence pour l'AFC avec factominer
tab <- xtabs(data=dat,formula=nombre_voix~codgeo+candidat)
# On élimine quelques communes pour lesquelles il n'y a pas de vote (hors blancs)
sans.vote <- which(apply(tab,MARGIN = 1,sum) ==0)
tab <- tab[-sans.vote,]


afc <- CA(tab,graph = F,ncp=4) 
plot.CA(afc,invisible = "row")
plot.CA(afc,invisible = "row",axes = 3:4)
barplot(afc$eig[,1])

coord <- data.frame(codgeo=rownames(afc$row$coord),afc$row$coord)
names(coord)[2:5] <- paste("Pol",1:4,sep="")

# Vu l'éboulis des VP, on peut se contenter des deux premiers axes
# (vu les graphs, ils permettent bien de séparer extremes versus modérés et droite versus gauche)
write.csv2(coord[,1:3],file = "Sources/Synthese_pol.csv",row.names = F)
