#limpiando 
cat("\014")
rm(list=ls())
## simulacion de los datos para 
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(survival)
library(readxl)
library(dplyr)
library(tidyr)

set.seed(2021)
# se simula una nueva tabla de datos basada en datos del 2019
#932 en promedio de los años anteriores 757 cadaverico y 175 vivo
#IPS
TRASPLANTE <- read_excel("D:/Trabajo de Grado/Trasplante Renal por IPS.xlsx", 
                         sheet = "T.renal x IPS art")
IPS<-TRASPLANTE$IPS            

Trasplante<-as.data.frame(TRASPLANTE[,-c(1,2)])
rownames(Trasplante)<-IPS
Trasplante[is.na(Trasplante)]<-0

Total<-apply(Trasplante[,-7], 1, sum)
mutate(Trasplante, Total=Total)


prob.IPS<-prop.table(Trasplante[,-7])

IPS_simu<-sample(IPS,865,replace = T,prob = prob.IPS$`2018`)

######## INICIAMOS CON TABLA DE TRASPLANTADOS ############

#cadaverico o vivo 
vivo<-rep(c("VIVO"),141)
cadaverico<-rep(c("CADAVERICO"),724)
DONANTE<-c(vivo,cadaverico)

#edad
edad<-c("<1año","1 a 5 años","6 a 11 años", "12 a 17 años","18 a 28 años", "29 a 59 años", "mayores de 60")
prob.edad.cadaverico<-c(0,0.007,0.017,0.033,0.12,0.632,0.188)
prob.edad.vivo<-c(0,0,0.028,0.064,0.27,0.553,0.085)
edad.vivo<-sample(edad,prob = prob.edad.vivo,replace = T,size=141)
edad.cadaverico<-sample(edad,prob = prob.edad.cadaverico,replace = T,size=724)
EDAD<-c(edad.vivo,edad.cadaverico)

#GENERO

sexo<-c("F","M")
sexo.vivo<-sample(sexo,prob=c(0.468,0.532), replace = T, size=141)
sexo.cadaverico<-sample(sexo,prob=c(0.405,0.595), replace = T, size=724)
SEXO<-c(sexo.vivo,sexo.cadaverico)

#ABO
abo<-c("O+","O-","A+","A-","B+","B-","AB+","AB-","sin dato")
abo.vivo<-sample(abo, prob=c(0.61,0.035,0.248,0,0.078,0.007,0.014,0,0.007), size = 141,replace = T)
abo.cadaverico<-sample(abo,size = 724,replace = T,prob=c(0.56,0.031,0.262,0.013,0.10,0,0.024,0.003,0.007))
ABO<-c(abo.vivo,abo.cadaverico)

#simulacion HLA
#A

library(readxl)
HLA <- read_excel("D:/Trabajo de Grado/HLA.xlsx")
#View(HLA)

HLA_A<-filter(HLA,Variacion=="A")$alelo
prob_HLA_A<-filter(HLA,Variacion=="A")$frecuencia
HLA_A_simu<-sample(HLA_A,size = 865,prob = prob_HLA_A,replace = T)
#sort(table(HLA_A_simu))
#sort(prop.table(table(HLA_A_simu)))

#B
HLA_B<-filter(HLA,Variacion=="B")$alelo
prob_HLA_B<-filter(HLA,Variacion=="B")$frecuencia
HLA_B_simu<-sample(HLA_B,size = 865,prob = prob_HLA_B,replace = T)
sort(prop.table(table(HLA_B_simu)))
#C
#HLA_C<-filter(HLA,Variacion=="C")$alelo
#prob_HLA_C<-filter(HLA,Variacion=="C")$frecuencia
#HLA_C_simu<-sample(HLA_C,size = 865,prob = prob_HLA_C,replace = T)

#DQB1
HLA_DQ<-filter(HLA,Variacion=="DQB1")$alelo
prob_HLA_DQ<-filter(HLA,Variacion=="DQB1")$frecuencia
HLA_DQ_simu<-sample(HLA_DQ,size = 865,prob = prob_HLA_DQ,replace = T)
sort(prop.table(table(HLA_DQ_simu)))

#DRB1
HLA_DR<-filter(HLA,Variacion=="DRB1")$alelo
prob_HLA_DR<-filter(HLA,Variacion=="DRB1")$frecuencia
HLA_DRB1_simu<-sample(HLA_DR,size = 865,prob = prob_HLA_DR,replace = T)
sort(prop.table(table(HLA_DRB1_simu)))

#RECIBE
RECIBE<-sample(c( "CADAVERICO","VIVO"),size=865, replace = T, prob = c(0.83,0.17))
#REGIONAL<-Trasplante$REGIONAL
tabla_trasplantados<-data.frame(IPS_simu,RECIBE,SEXO,EDAD,ABO,
                                HLA_A_simu,HLA_B_simu,HLA_DQ_simu,HLA_DRB1_simu)

######### TABLA PARA LA LISTA DE ESPERA #####

n_LE=2576

library(readxl)
lista_de_espera_por_IPS <- read_excel("D:/Trabajo de Grado/lista de espera por IPS.xlsx", 
                                      sheet = "Lista art")
#IPS
IPS_simu<-sample(lista_de_espera_por_IPS$IPS,prob = lista_de_espera_por_IPS$prob,
                 replace = T,size=n_LE)
#RECIBE
RECIBE<-sample(c("MUERE","NO MUERE"),prob=c(133/n_LE,(n_LE-133)/n_LE),size = n_LE,replace = T)

#SEXO
SEXO<-sample(c("M","F"),replace = T,size=n_LE,prob = c(0.512,0.488))

#EDAD
#se toma la suma tanto de cadaverico como de vivo y se hace la misma proporcion de edad
EDAD<-sample(edad,size = n_LE,prob = c(0,0.005,0.019,0.038,0.15,0.62,0.17),replace = T)

#ABO
prob_ABO<-c(0.521,0.07,0.28,0.025,0.067,0.008,0.017,0.003,0.09)
ABO<-sample(abo,replace = T,size=n_LE,prob = prob_ABO)

#simulacion HLA
#A
HLA_A_simu<-sample(HLA_A,size = n_LE,prob = prob_HLA_A,replace = T)

#B
HLA_B_simu<-sample(HLA_B,size = n_LE,prob = prob_HLA_B,replace = T)

#C
#HLA_C_simu<-sample(HLA_C,size = n_LE,prob = prob_HLA_C,replace = T)

#DQB1
HLA_DQ_simu<-sample(HLA_DQ,size = n_LE,prob = prob_HLA_DQ,replace = T)

#DRB1
HLA_DRB1_simu<-sample(HLA_DR,size = n_LE,prob = prob_HLA_DR,replace = T)

tabla_no_trasplantados<-data.frame(IPS_simu,RECIBE,SEXO,EDAD,ABO,
                                   HLA_A_simu,HLA_B_simu,HLA_DQ_simu,HLA_DRB1_simu)


TABLA<-rbind(tabla_trasplantados,tabla_no_trasplantados)

### agregando la regional 
regional<-read_excel("D:/Trabajo de Grado/Trasplante Renal por IPS.xlsx", 
                     sheet = "regional x IPS")
colnames(regional)<-c("IPS_simu", "CIUDAD", "REGIONAL")
TABLA<-merge(TABLA,regional,by="IPS_simu")[,-12]
#agregandole la misma distribucion del tiempo de espera a todos

##### tiempo en lista de espera ####
n=3441

TABLA<-mutate(TABLA,TIEMPO=0)
TABLA->TABLA_SIMULADA
#TABLA<-as.matrix(TABLA)

for(i in 1:n){
  if(TABLA_SIMULADA$REGIONAL[i]=="REG 1" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,min = 3,max = 3701)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 2" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,2,1980)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 3" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,2,2173)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 4" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,7,1562)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 5" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,6,2367)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 6" & TABLA_SIMULADA$RECIBE[i]=="CADAVERICO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,83,2220)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 1" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,2583)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 2" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,838)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 3" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,1126)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 4" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,2)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 5" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,1946)
  }else if(TABLA_SIMULADA$REGIONAL[i]=="REG 6" & TABLA_SIMULADA$RECIBE[i]=="VIVO"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,1,2583)
  }else if(TABLA_SIMULADA$EDAD=="<1año"| TABLA_SIMULADA$EDAD =="6 a 11 años" | TABLA_SIMULADA$EDAD== "12 a 17 años" | TABLA_SIMULADA$EDAD == "12 a 17 años"){
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,3,1844)
  }else{
    TABLA_SIMULADA$TIEMPO[i]<-runif(1,2,370)
  }
}

TABLA_SIMULADA$TIEMPO<-round(TABLA_SIMULADA$TIEMPO)

#categorizando el tiempo
Tiempo_cat<-cut(TABLA_SIMULADA$TIEMPO,breaks = c(0,8,16,31,60,90,180,365,545,730,1095,1460,3676))
levels(Tiempo_cat)<-c("1 semana","2 semana","1 mes","2 mes","3 mes","6 mes","1 año","1.5 año","2 año","3 año","4 año","> 4 año")


TABLA<-mutate(TABLA_SIMULADA,Tiempo_cat=Tiempo_cat)

#disminuyendo categorias de la IPS


### exporte de tabla

write.csv(TABLA,file="D:/Trabajo de Grado/Tabla_art.csv")
