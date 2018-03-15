#install.packages("devtools")
library(devtools)
#devtools::install_github("antuki/COGugaison")
library(COGugaison)
library(tidyverse)

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
#fusion_2016_2017<-subset(fusion_2016_2017,!cod2016==cod2017)
#nrow(fusion_2016_2017)
##création d'un indicateur de fusion selon l'année
temp<-split(fusion_2016_2017,fusion_2016_2017$cod2017)
temp1<-lapply(temp,function(x) expand.grid(x[,1],x[,1]))
fusion_2016_2017_bis<-bind_rows(temp1)

####verif de la totalité des fusions sur 89405
#toto<-fusion_2016_2017_bis[,Var1=="01172"]
#toto<-select(fusion_2016_2017_bis,Var1=="01172")
#toto<-subset(fusion_2016_2017_bis,Var1=="89405")
#subset(fusion_2016_2017_bis,Var1=="89174")

fusion_2016_2017_bis<-subset(fusion_2016_2017_bis,!Var1==Var2)
fusion_2016_2017_bis$fusion_2016<-1
fusion_2016_2017_bis

couples_ebh<-merge(couples,fusion_2016_2017_bis,
                   by.x=c("codgeo1","codgeo2"),by.y=c("Var1","Var2"),all.x=T)
couples_ebh$fusion_2016<-ifelse(is.na(couples_ebh$fusion_2016),yes=0,no=couples_ebh$fusion_2016)
sum(couples_ebh$fusion_2016)



##De 2015 à 2016
##filtre sur le typemodif=f
fusion_2015_2016<-filter(PASSAGE_2015_2016_insee,typemodif=="f")
nrow(fusion_2015_2016)
##suppression des communes qui n'ont pas bougées
#fusion_2015_2016<-subset(fusion_2015_2016,!cod2015==cod2016)
#nrow(fusion_2015_2016)
##création d'un indicateur de fusion selon l'année
essai<-split(fusion_2015_2016,fusion_2015_2016$cod2016)
essai1<-lapply(essai,function(x) expand.grid(x[,1],x[,1]))
fusion_2015_2016_bis<-bind_rows(essai1)

fusion_2015_2016_bis$fusion_2015<-1
fusion_2015_2016_bis

couples_ebh<-merge(couples_ebh,fusion_2015_2016_bis,
                   by.x=c("codgeo1","codgeo2"),by.y=c("Var1","Var2"),all.x=T)
couples_ebh$fusion_2015<-ifelse(is.na(couples_ebh$fusion_2015),yes=0,no=couples_ebh$fusion_2015)
sum(couples_ebh$fusion_2015)


#dédupliquer pour ne garder que les identifiants uniques
couples_ebh$fusion <- couples_ebh$fusion_2015+couples_ebh$fusion_2016
#sum(couples_ebh$fusion)
#arrange(couples_ebh,ident)
#sum(couples_ebh$fusion)

#subset(couples_ebh,fusion>=1)
#couples_ebh <- group_by(couples_ebh,ident) %>% summarise(fusion=sum(fusion))
doublons <- which(duplicated(couples_ebh$ident))
couples_ebh_uniques <- couples_ebh[-doublons,] %>%
    select(ident,starts_with("fusion"))
#sum(couples_ebh_uniques$fusion)

base <- merge(base,couples_ebh_uniques,by.x="ident",by.y="ident",all.x=T)

save(base,mapCom,couples,file="Base.RData")
