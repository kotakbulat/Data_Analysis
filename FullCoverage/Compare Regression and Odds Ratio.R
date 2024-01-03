
#import data Fullcoverage.csv 
library(readr)
Fullcov <- read_csv("FullCoverage.csv")
head(Fullcov)

unique(Fullcov$marital)

#  Tabel deskriptif statistik
table(Fullcov$y)

tab22<-function(a,y){table(a,y)
  print(table(a,y))
  options(digits=4)
  print(prop.table(table(a,y),1)*100)
  print(margin.table(table(a,y),1))
  options(digits=7)}

tab22(Fullcov$men,Fullcov$y) #(baris,kolom)
tab22(Fullcov$urban,Fullcov$y)
tab22(Fullcov$private,Fullcov$y)
tab22(Fullcov$marital,Fullcov$y)

by(Fullcov$age,list(Fullcov$y),FUN=mean)  #melihat rata-rata umur
mean(Fullcov$age)

by(Fullcov$seniority,list(Fullcov$y),FUN=mean) 
mean(Fullcov$seniority)

library(Hmisc)
library(tidyverse)

#Penentuan Reference Category
Fullcov2=Fullcov%>%
  mutate(marital=ifelse(marital=="S",0,marital))

#########################################################################################################
######################               Praktikum Post Test 2                 #############################
#Model Regresi Probit
FullcovModelProbit = glm(y~men+urban+private+factor(marital)+age+seniority,family=binomial(link="probit"),data=Fullcov2)

#logLik(FullcovModelProbit)

#Model Regresi Logistik
FullcovModelLogit = glm(y~men+urban+private+factor(marital)+age+seniority,family=binomial(link=logit),data=Fullcov2)

logLik(FullcovModelLogit)

# ODDS-RATIO
library(questionr)
exp(coef(summary(FullcovModelLogit,1)))
odds.ratio(FullcovModelLogit)
summary(FullcovModelProbit)
summary(FullcovModelLogit)