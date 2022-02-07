################## Arbol de decision##################
library(rpart)
library(rpart.plot)
set.seed(2020)
train<-sample_frac(TABLA,0.7)
test<-setdiff(TABLA,train)

arbol<-rpart(RECIBE~SEXO+EDAD+Tiempo_cat,data = TABLA)
arbol$parms
rpart.plot(arbol)

#prediccion

prediccion<-predict(arbol,newdata = test,type="class")
confusionMatrix(prediccion,test[["RECIBE"]])
prediccion


###### estimacion de las probabilidad con bootstrap y intervalos de confianza##############
library(fastDummies)
library(boot)

library(readr)
TABLA_SIMULADA <- read_csv("D:/Trabajo de Grado/TABLA_SIMULADA.csv")
View(TABLA_SIMULADA)
TABLA_SIMULADA->TABLA

dummy=dummy_cols(TABLA$RECIBE)
TABLA<-cbind(TABLA,dummy)

#cadaverico
set.seed(2020)
x<-TABLA$.data_CADAVERICO
myprop<-function(x,i){
  sum(x[i]==1)/length(x)
}
bootprop1 <- boot(x,myprop,1000)
plot(bootprop1, main="distribución de proporcion de pacientes cadavericos")
boot.ci(bootprop1, type="norm")
mean(bootprop1$t)

#vivo
set.seed(2020)
x<-TABLA$.data_VIVO
myprop<-function(x,i){
  sum(x[i]==1)/length(x)
}
bootprop2 <- boot(x,myprop,1000)
plot(bootprop2, main="Histograma sobre remuestereo de rpoporcion de donantes vivo")
boot.ci(bootprop2, type="norm")
mean(bootprop2$t)

#muerto
set.seed(2020)
x<-TABLA$.data_MUERE
myprop<-function(x,i){
  sum(x[i]==1)/length(x)
}
bootprop3 <-boot(x,myprop,1000)
plot(bootprop3)
boot.ci(bootprop3, type="norm")
mean(bootprop3$t)

#no recibe
set.seed(2020)
x<-TABLA$.data_NO
myprop<-function(x,i){
  sum(x[i]==1)/length(x)
}
bootprop4 <- boot(x,myprop,1000)
plot(bootprop4)
boot.ci(bootprop4, type="norm")
mean(bootprop4$t)