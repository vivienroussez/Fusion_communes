install.packages("devtools")
library(devtools)

install("C:/Users/umdp023/Downloads/COGugaison-master")
devtools::install_github("antuki/COGugaison")
library(COGugaison)
library(dplyr)

load("Base.RData")

#Base par code geo des libelles et population
head(COG2017)
COG2017<-COG2017

#Base des fusions/défusions d'une année à l'autre
head(PASSAGE_2016_2017_insee)
PASSAGE_2016_2017_insee<-PASSAGE_2016_2017_insee
head(PASSAGE_2015_2016_insee)
PASSAGE_2015_2016_insee<-PASSAGE_2015_2016_insee

#creation de la tables des fusions
##De 2016 à 2017
##filtre sur le typemodif=f
fusion_2016_2017<-filter(PASSAGE_2016_2017_insee,typemodif=="f")
nrow(fusion_2016_2017)
##suppression des communes qui n'ont pas bougées
fusion_2016_2017<-subset(fusion_2016_2017,!cod2016==cod2017)
nrow(fusion_2016_2017)
##création d'un indicateur de fusion selon l'année
temp<-split(fusion_2016_2017,fusion_2016_2017$cod2017)
temp1<-lapply(temp,function(x) expand.grid(x[,1],x[,1]))
fusion_2016_2017_bis<-bind_rows(temp1)

fusion_2016_2017_bis$fusion_2016<-1
fusion_2016_2017_bis

couples_ebh<-merge(couples,fusion_2016_2017_bis,by.x=c("codgeo1","codgeo2"),by.y=c("Var1","Var2"),all.x=T)
couples_ebh
table(couples_ebh$fusion_2016)

##De 2015 à 2016
##filtre sur le typemodif=f
fusion_2015_2016<-filter(PASSAGE_2015_2016_insee,typemodif=="f")
nrow(fusion_2015_2016)
##suppression des communes qui n'ont pas bougées
fusion_2015_2016<-subset(fusion_2015_2016,!cod2015==cod2016)
nrow(fusion_2015_2016)
##création d'un indicateur de fusion selon l'année
essai<-split(fusion_2015_2016,fusion_2015_2016$cod2016)
essai1<-lapply(essai,function(x) expand.grid(x[,1],x[,1]))
fusion_2015_2016_bis<-bind_rows(essai1)

fusion_2015_2016_bis$fusion_2015<-1
fusion_2015_2016_bis

couples_ebh<-merge(couples_ebh,fusion_2015_2016_bis,by.x=c("codgeo1","codgeo2"),by.y=c("Var1","Var2"),all.x=T)
couples_ebh
table(couples_ebh$fusion_2015)
table(couples_ebh$fusion_2016)

couples_ebh$fusion_2015 <- ifelse(is.na(couples_ebh$fusion_2015),yes = 0, no = couples_ebh$fusion_2015)
couples_ebh$fusion_2016 <- ifelse(is.na(couples_ebh$fusion_2016),yes = 0, no = couples_ebh$fusion_2016)
table(couples_ebh$fusion_2015)
table(couples_ebh$fusion_2016)

#dédupliquer pour ne garder que les identifiants uniques
couples_ebh1$fusion <- couples_ebh1$fusion_2015+couples_ebh1$fusion_2016
couples_ebh1 <- group_by(couples_ebh1,ident) %>% summarise(fusion=sum(fusion))

base<-merge(base,couples_ebh1,by.x="ident",by.y="ident",all.x=T)

