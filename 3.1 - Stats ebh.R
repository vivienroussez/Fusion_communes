require(tidyverse)
require(FactoMineR)
require(dplyr)

load("Base.RData")
dat1 <- baseML
summary(dat1)

dat1<-rename(dat1,fusion=y)

#Visualisation des boxplots pour les vars dist en fonction de la fusion
library(ggplot2)

ggplot(dat1,aes(x=fusion,y=dist_P13_POP))+geom_violin()+geom_boxplot(width=0.1)
ggplot(dat1,aes(x=fusion,y=dist_Pol1))+geom_violin()+geom_boxplot(width=0.1)

ggplot(dat1,aes(x=fusion,y=log(nb_navettes)))+geom_violin()+geom_boxplot(width=0.1)

#On visualise les corr?lations sur les variables de distance 
install.packages("corrplot")
library(corrplot)
mat_cor<-select(dat1,starts_with("nb"),starts_with("dist"))
#class(mat_cor)

dat1_cor<-round(cor(mat_cor),2)

#corrplot(dat1_cor, method = "circle")
#corrplot(dat1_cor, method = "ellipse")
#corrplot(dat1_cor, method = "number")
corrplot(dat1_cor, method = "pie")

#K means
#attention, il faut r?duire et centrer qd les variables sont dans des unit?s diff?rentes
#on fait sur les donn?es de l'ACP
lst_km <- list()
lst_km.ss <- list()
for (ii in 1:20)
{
  lst_km[[ii]] <- kmeans(mat_cor,ii,iter.max = 20)
  lst_km.ss[[ii]] <- lst_km[[ii]]$betweenss/lst_km[[ii]]$totss
}
unlist(lst_km.ss) %>% plot(col="blue",xlab="Nombre de classes",ylab="Variance interclasse")

#On voit que la variance intra-classes stagne à partir de 6 classes.
#On voit qu'avec les 5 classes, on a près de 90% de la variance totale 

cl <- lst_km[[6]]$cluster

cl <- data.frame(cl)
cl$ident<-rownames(cl)
dat1$ident<-rownames(dat1)

dat1Cl <- merge(dat1,cl,by.x="ident",by.y="ident",all.x=T)

dat1Cl$fusion_class <- ifelse(dat1Cl$fusion=="0","Non Fusion","Fusion")
dat1Cl$cl <- as.factor(dat1Cl$cl)
dat1Cl$cl_class<-paste("cluster",dat1Cl$cl)

tableau<-table(dat1Cl$cl_class,dat1Cl$fusion_class) %>% addmargins()
tableau


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






