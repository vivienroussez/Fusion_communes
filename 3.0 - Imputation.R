require(tidyverse)
require(FactoMineR)

load("Base.RData")
table(base$fusion)
dat <- base
summary(dat)

## On vire les variables sur lesquelles il y a trop de données manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>2000)
dat <- dat[,-lesquelles]

num  <- select(dat,starts_with("dist"),starts_with("nb")) 
fact <- select(dat,-starts_with("dist"),-starts_with("nb"))

# On remplace les NA par la moyenne et les NaN par le max  
# (c'est quand on a un zéro et on considère la dist comme max dan ces cas)
maxi <- sapply(num, max,na.rm=T)
moy  <- sapply(num, mean,na.rm=T)

for (ii in 1:ncol(num))
{
  num[is.na(num[,ii]),ii] <- moy[ii]
  num[is.nan(num[,ii]),ii] <- maxi[ii]
}

## Matrice de corrélation et ACP pour description rapide
cor(num)

dat <- cbind(fact,num) %>% 
  mutate(y=as.factor(fusion)) %>%
  select(-ident,-first,-second,-starts_with("fusion"))
row.names(dat) <- fact$ident

acp <- PCA(dat,quali.sup = c(1:8,39),graph = F)
plot.PCA(acp,choix="var",col.var="blue")
plot.PCA(acp,choix=c("ind"),select = "contrib20")

baseML <- dat
save(base,baseML,mapCom,couples,file="Base.RData")



#summary(num)
dat1 <- cbind(fact,num)
ncol(num)
dat1$fusion<-as.factor(dat1$fusion)

#J'ai essayé de visualiser les corrélations sur les variables de distance 
install.packages("corrplot")
library(corrplot)
mat_cor<-select(dat1,starts_with("dist"),-dist_Pol1,-dist_Pol2)
colnames(mat_cor)
dat1_cor<-round(cor(mat_cor),2)
corrplot(dat1_cor, method = "circle")
corrplot(dat1_cor, method = "ellipse")
corrplot(dat1_cor, method = "number")
corrplot(dat1_cor, method = "pie")

#je voudrais visualiser les vars dist + les vars nb en fonction de la fusion
library(ggplot2)
lg<-sqrt(ncol(mat_cor))
par(mfrow=c(lg,lg))
ggplot(dat1,aes(x=fusion,y=dist_DECE0813))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_POP))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P08_POP))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_SUPERF))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_NAIS0813))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_MEN))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_LOG))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_RP))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_RSECOCC))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_LOGVAC))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_RP_PROP))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_EMPLT))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_EMPLT_SAL))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P08_EMPLT))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_POP1564))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_CHOM1564))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_P13_ACT1564))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_ETTOT14))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_ETAZ14))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_ETFZ14))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_ETGU14))+geom_boxplot()
ggplot(dat1,aes(x=fusion,y=dist_ETOQ14))+geom_boxplot() 
ggplot(dat1,aes(x=fusion,y=dist_ETTEF114))+geom_boxplot() 
ggplot(dat1,aes(x=fusion,y=dist_revmoy))+geom_boxplot() 
ggplot(dat1,aes(x=fusion,y=dist_pot_fin))+geom_boxplot() 



#Creation de plusieurs bases pour traiter le faible nombre de fusions
#en dupliquant les fusions et en les bruitants
base_fusion <- filter(dat,fusion==1)
#base_fusion<-dat[dat$fusion==1, ]

coli <- which(colnames(base_fusion) %in% colnames(select(base_fusion,starts_with("dist"))))
base_fusion_bis <- base_fusion

for (ii in coli)
{
  a<-mean(base_fusion[,ii],is.na=F)
  b<-var(base_fusion[,ii],na.rm=TRUE)
  base_fusion_bis[,ii]<-base_fusion[,ii]+rnorm(1, a,b)
  print(ii)
}

coli <- which(colnames(base_fusion_bis) %in% colnames(select(base_fusion_bis,starts_with("nb"))))

for (ii in coli)
{
  a<-mean(base_fusion_bis[,ii],is.na=F)
  b<-var(base_fusion_bis[,ii],na.rm=TRUE)
  base_fusion_ter[,ii]<-base_fusion_bis[,ii]+rnorm(1, a,b)
  print(ii)
}



