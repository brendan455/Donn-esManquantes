setwd("working directory")
getwd()

#introduction
num <- c(-10,0,10, 20, NA, 30, 40)
str<-c("M1","M2",".",NA,NA)
num==NA
is.na(num)
!is.na(num)
str==NA
is.na(str)
!is.na(str)
which(is.na(num))
which(is.na(str))

factor(num)
factor(num, exclude=NULL)
table(num)
table(num, exclude=NULL)
summary(num)
mean(num)
mean(num,na.rm=TRUE)
sd(num)
sd(num,na.rm=TRUE)
sum(num)
sum(num,na.rm=TRUE)

#1. importation des données
data<-read.table("donneesinit.csv",header = T, sep=";",dec=".")
summary(data)
mean(data$pas.M6)
mean(data$pas.M6,na.rm=TRUE)
sd(data$pas.M6)
sd(data$pas.M6,na.rm=TRUE)


#2. Calculer la variable "bmi","hta.M0" et "hta.M6"
bmi<-c(0)
data<-cbind(data,bmi)
data$bmi<-data$poids/(data$taille*data$taille)
hta.M0<-c(0)
hta.M6<-c(0)
data<-cbind(data,hta.M0,hta.M6)
data$hta.M0<-ifelse(data$pas.M0 >= 140, 1, 0)
data$hta.M6<-ifelse(data$pas.M6 >= 140, 1, 0)


#3. Supprimer tous les sujets ne vérifiant pas les critères d'inclusion.
## hta diagnostiqué avant inclusion M0 / PAD>=90 / PAS>=140
data<-data[data$pad.M0>=90 | data$hta.M0==1,]

## age moins de 80 ans
data<-data[data$age<=80,]
dim(data)
summary(data)


#4.Calculer le % de sujets sans mesure manquante à  M3.
summary(data$pas.M3)
round(16/length(data$pas.M3),2)


#5. Y-a-t il des sujets avec des mesures manquantes à  M3 mais pas à  M6
test<-data[is.na(data$pas.M3)==T & !is.na(data$pas.M6)==T,]
test
summary(test)


#6. Créer les variables pas.M6.missing, pad.M6.missing et hta.M6.missing codant si les mesures de PAS, de PAD et du diagnostic d'HTA
#sont manquantes à  M6 (1=oui,0=non).
pas.M6.missing<-c(0)
pad.M6.missing<-c(0)
hta.M6.missing<-c(0)
data<-cbind(data,pas.M6.missing,pad.M6.missing,hta.M6.missing)
data$pas.M6.missing<-ifelse(is.na(data$pas.M6)==T, 1, 0)
data$pad.M6.missing<-ifelse(is.na(data$pad.M6)==T, 1, 0)
data$hta.M6.missing<-ifelse(is.na(data$hta.M6)==T, 1, 0)


#7. Créer les variables diffpasM3 et diffpadM3 calculant la différence M3-M0 pour la PAS et la PAD.
diffpasM3<-c(0)
diffpadM3<-c(0)
diffpasM6<-c(0)
diffpadM6<-c(0)
data<-cbind(data,diffpasM3,diffpadM3,diffpasM6,diffpadM6)
data$diffpasM3<-data$pas.M3-data$pas.M0
data$diffpadM3<-data$pad.M3-data$pad.M0
data$diffpasM6<-data$pas.M6-data$pas.M0
data$diffpadM6<-data$pad.M6-data$pad.M0


#8. Effectuer la régression logistique de présence de données manquantes à  M6 pour la PAS en fonction des valeurs de PAS à  l'inclusion.
regression<-glm(formula = data$pas.M6.missing ~ data$pas.M0, family = binomial(link = logit))
summary(regression)

##la régression logistique de présence de données manquantes à  M6 pour la PAS en fonction de l'évolution des mesures entre l'inclusion et M3.
regression2<-glm(formula = data$pas.M6.missing ~ data$diffpasM3, family = binomial(link = logit))
summary(regression2)

#L'hypothèse de données manquantes complètement aléatoirement (MCAR) pour la PAS à  M6 est-elle rejetée ?
# OUI


#9. Calculer les moyennes de PAS ainsi que les IC95% dans chacun des groupes à  M0 et à  M6 (première ligne du tableau de synthèse).
Trt1<-data[data$groupe=="Trt1",]
summary(Trt1)
Trt2<-data[data$groupe=="Trt2",]
summary(Trt2)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

#complete case
Trt1<-data[data$groupe=="Trt1",]
Trt2<-data[data$groupe=="Trt2",]
Trt1<-Trt1[is.na(Trt1$pas.M6)==F ,]
dim(Trt1)
Trt2<-Trt2[is.na(Trt2$pas.M6)==F ,]
dim(Trt2)
t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#best case
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)

Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M6)==T, pmin(Trt1$pas.M0,Trt1$pas.M3, na.rm=T), Trt1$pas.M6)
Trt1$diffpasM6<-Trt1$pas.M6-Trt1$pas.M0
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M6)==T, pmin(Trt1$pas.M0,Trt1$pas.M3, na.rm=T), Trt2$pas.M6)
Trt2$diffpasM6<-Trt2$pas.M6-Trt2$pas.M0
length(Trt1$diffpasM6)
length(Trt2$diffpasM6)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#worst case
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)
Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M6)==T, pmax(Trt1$pas.M0,Trt1$pas.M3, na.rm=T), Trt1$pas.M6)
Trt1$diffpasM6<-Trt1$pas.M6-Trt1$pas.M0
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M6)==T, pmax(Trt1$pas.M0,Trt1$pas.M3, na.rm=T), Trt2$pas.M6)
Trt2$diffpasM6<-Trt2$pas.M6-Trt2$pas.M0
length(Trt1$diffpasM6)
length(Trt2$diffpasM6)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#personnal mean score
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)
Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M6)==T, mean(Trt1$pas.M0,Trt1$pas.M3), Trt1$pas.M6)
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M6)==T, mean(Trt2$pas.M0,Trt1$pas.M3), Trt2$pas.M6)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#mean score
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)
Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M6)==T, mean(Trt1$pas.M6,na.rm=T), Trt1$pas.M6)
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M6)==T, mean(Trt2$pas.M6,na.rm=T), Trt2$pas.M6)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#LOCF
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)
Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M3)==T, Trt1$pas.M0, Trt1$pas.M3)
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M3)==T, Trt2$pas.M0, Trt2$pas.M3)
Trt1$pas.M6<-ifelse(is.na(Trt1$pas.M6)==T, Trt1$pas.M3, Trt1$pas.M6)
Trt2$pas.M6<-ifelse(is.na(Trt2$pas.M6)==T, Trt2$pas.M3, Trt2$pas.M6)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#regression linéaire simple
model<-lm(data$pas.M3~data$pas.M0, data = data)
data$pas.M0*1.001-4.407
data$pas.M3<-ifelse(is.na(data$pas.M3)==T, data$pas.M0*1.001-4.407  , data$pas.M3)

data$diffpasM3<-ifelse(is.na(data$diffpasM3)==T, data$pas.M3-data$pas.M0  , data$diffpasM3)


model<-lm(data$pas.M6~data$diffpasM3, data = data)
data$diffpasM3*0.2236+153.7488
data$pas.M6<-ifelse(is.na(data$pas.M6)==T, data$diffpasM3*0.2236+153.7488 , data$pas.M6)

Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)

#regression linéaire multiple
model<-lm(data$pas.M3~data$pas.M0+data$fumeur+data$age+data$bmi+data$sexe+data$groupe, data = data)
data$pas.M0*1.00251 - data$fumeur*0.62967 + data$age*0.03226 - data$bmi*360.51209 + data$sexe*0.12019 - 5.55356
data$pas.M3<-ifelse(is.na(data$pas.M3)==T, data$pas.M0*1.00251 - data$fumeur*0.62967 + data$age*0.03226 - data$bmi*360.51209 + data$sexe*0.12019 - 5.55356 , data$pas.M3)

model<-lm(data$pas.M6~data$diffpasM3+data$fumeur+data$age+data$bmi+data$sexe+data$groupe, data = data)
data$diffpasM3*1.805e-01 - data$fumeur*2.024e+00 + data$age*2.869e-02 - data$bmi*1.457e+03 + data$sexe*4.999e+00 + 1.439e+02
data$pas.M6<-ifelse(is.na(data$pas.M6)==T, data$diffpasM3*1.805e-01 - data$fumeur*2.024e+00 + data$age*2.869e-02 - data$bmi*1.457e+03 + data$sexe*4.999e+00 + 1.439e+02, data$pas.M6)
Trt1<-data[data$groupe=="Trt1",]
dim(Trt1)
Trt2<-data[data$groupe=="Trt2",]
dim(Trt2)

t.test(Trt1$pas.M0, conf.level = 0.95)
t.test(Trt1$pas.M6, conf.level = 0.95)
t.test(Trt1$diffpasM6, conf.level = 0.95)
t.test(Trt2$pas.M0, conf.level = 0.95)
t.test(Trt2$pas.M6, conf.level = 0.95)
t.test(Trt2$diffpasM6, conf.level = 0.95)

Trt12<-rbind(Trt1,Trt2)
t.test(Trt12$diffpasM6 ~ Trt12$groupe, conf.level = 0.95)



