<<<<<<< HEAD
require(tidyverse)
require(FactoMineR)
require(dplyr)

load("Base.RData")
table(base$fusion)
dat <- base
summary(dat)

## On vire les variables sur lesquelles il y a trop de donnÃ©es manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>round(nrow(dat)*0.03,0))
dat <- dat[,-lesquelles]

num  <- select(dat,starts_with("dist"),starts_with("nb")) 
fact <- select(dat,-starts_with("dist"),-starts_with("nb"))

# On remplace les NA par la moyenne et les NaN par le max  
# (c'est quand on a un zÃ©ro et on considÃ¨re la dist comme max dan ces cas)
maxi <- sapply(num, max,na.rm=T)
moy  <- sapply(num, mean,na.rm=T)

for (ii in 1:ncol(num))
{
  num[is.na(num[,ii]),ii] <- moy[ii]
  num[is.nan(num[,ii]),ii] <- maxi[ii]
}

# On refusionne les bases
dat1 <- cbind(fact,num)

#On met en facteur la variable de fusion
dat1$fusion<-as.factor(dat1$fusion)
#on crée un identifiant
dat1$indic<-rownames(dat1$ident)

#Visualisation des boxplots pour les vars dist en fonction de la fusion
library(ggplot2)

ggplot(dat1,aes(x=fusion,y=dist_P13_POP))+geom_violin()+geom_boxplot(width=0.1)
ggplot(dat1,aes(x=fusion,y=dist_Pol1))+geom_violin()+geom_boxplot(width=0.1)

ggplot(dat1,aes(x=fusion,y=log(nb_navettes)))+geom_violin()+geom_boxplot(width=0.1)

#On visualise les corrélations sur les variables de distance 
install.packages("corrplot")
library(corrplot)
mat_cor<-select(dat1,starts_with("nb"),starts_with("dist"))
class(mat_cor)

dat1_cor<-round(cor(mat_cor),2)

#corrplot(dat1_cor, method = "circle")
#corrplot(dat1_cor, method = "ellipse")
#corrplot(dat1_cor, method = "number")
corrplot(dat1_cor, method = "pie")

#K means
#attention, il faut réduire et centrer qd les variables sont dans des unités différentes
#on fait sur les données de l'ACP
lst_km <- list()
lst_km.ss <- list()
for (ii in 1:20)
{
  lst_km[[ii]] <- kmeans(mat_cor,ii,iter.max = 20)
  lst_km.ss[[ii]] <- lst_km[[ii]]$betweenss/lst_km[[ii]]$totss
}
unlist(lst_km.ss) %>% plot(col="blue",xlab="Nombre de classes",ylab="Variance interclasse")

#On voit que la variance intra-classes stagne à partir de 5 classes.
#On voit qu'avec les 5 classes, près de 90% de la variance totale sur l'ensemble des variables une 

cl <- lst_km[[6]]$cluster

cl <- data.frame(cl)
cl$ident<-rownames(cl)
dat1$ident<-rownames(dat1)

dat1Cl <- merge(dat1,cl,by.x="ident",by.y="ident",all.x=T)

dat1Cl$fusion_class <- ifelse(dat1Cl$fusion=="0","Non Fusion","Fusion")
dat1Cl$cl <- as.factor(dat1Cl$cl)
dat1Cl$cl_class<-paste("cluster",dat1Cl$cl)

tableau<-table(dat1Cl$cl_class,dat1Cl$fusion_class) %>% addmargins()


##ACP pour description rapide
#dat <- cbind(fact,num) %>% 
#  mutate(y=as.factor(fusion)) %>%
#  select(-ident,-first,-second,-starts_with("fusion"))
#row.names(dat) <- fact$ident

#acp <- PCA(dat,quali.sup = c(1:8,39),graph = F)
#plot.PCA(acp,choix="var",col.var="blue")
#plot.PCA(acp,choix=c("ind"),select = "contrib20")

#baseML <- dat

save(base,baseML,mapCom,couples,file="Base.RData")




#Creation de plusieurs bases pour traiter le faible nombre de fusions
#en dupliquant les fusions et en les bruitants
#base_fusion <- filter(dat,fusion==1)
#base_fusion<-dat[dat$fusion==1, ]

#coli <- which(colnames(base_fusion) %in% colnames(select(base_fusion,starts_with("dist"))))
#base_fusion_bis <- base_fusion

#for (ii in coli)
#{
#  a<-mean(base_fusion[,ii],is.na=F)
#  b<-var(base_fusion[,ii],na.rm=TRUE)
#  base_fusion_bis[,ii]<-base_fusion[,ii]+rnorm(1, a,b)
#  print(ii)
#}

#coli <- which(colnames(base_fusion_bis) %in% colnames(select(base_fusion_bis,starts_with("nb"))))

#for (ii in coli)
#{
#  a<-mean(base_fusion_bis[,ii],is.na=F)
#  b<-var(base_fusion_bis[,ii],na.rm=TRUE)
#  base_fusion_ter[,ii]<-base_fusion_bis[,ii]+rnorm(1, a,b)
#  print(ii)
#}







=======
require(tidyverse)
require(FactoMineR)
require(sf)

load("Base.RData")
table(base$fusion)
dat <- base
summary(dat)

## On vire les variables sur lesquelles il y a trop de donnÃ©es manquantes
manquants <- sapply(dat,function(x) sum(is.na(x))) 
lesquelles <- which(manquants>2000)
dat <- dat[,-lesquelles]

num  <- select(dat,starts_with("dist"),starts_with("nb")) 
fact <- select(dat,-starts_with("dist"),-starts_with("nb"))

# On remplace les NA par la moyenne et les NaN par le max  
# (c'est quand on a un zÃ©ro et on considÃ¨re la dist comme max dan ces cas)
maxi <- sapply(num, max,na.rm=T)
moy  <- sapply(num, mean,na.rm=T)

for (ii in 1:ncol(num))
{
  num[is.na(num[,ii]),ii] <- moy[ii]
  num[is.nan(num[,ii]),ii] <- maxi[ii]
}

## Matrice de corrÃ©lation et ACP pour description rapide
cor(num)

dat <- cbind(fact,num) %>% 
  mutate(y=as.factor(fusion)) %>%
  select(-ident,-first,-second,-starts_with("fusion"))
row.names(dat) <- fact$ident

baseML <- dat
save(base,mapCom,baseML,couples,mapLight,file="Base.RData")

#############################################
########## Vraie partie stat desc ###########
#############################################


acp <- PCA(dat,quali.sup = c(1:8,39),graph = F)
plot.PCA(acp,choix="var",col.var="blue")
plot.PCA(acp,choix=c("ind"),select = "contrib20")


#summary(num)
dat1 <- cbind(fact,num)
ncol(num)
dat1$fusion<-as.factor(dat1$fusion)

#J'ai essayÃ© de visualiser les corrÃ©lations sur les variables de distance 
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



>>>>>>> b2d0e210951c350347e94c01a307b98c934ab4b1
