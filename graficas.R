#probando graficas para Figura 8
cat("\014")
rm(list=ls());
library(ggplot2)
library(readxl)
library(ggrepel)
library(tidyr)
library(tidyverse)
TransVsEsp <- read_excel("D:/Trabajo de Grado/Articulo/TransVsEsp.xlsx", 
                         sheet = "Trans-Esp")
View(TransVsEsp)

Transplantes<-c(TransVsEsp$`2019`,TransVsEsp$`2018`,TransVsEsp$`2017`,TransVsEsp$`2016`)
Espera<-c(TransVsEsp$`Riñon-2019`,TransVsEsp$`Riñón-2018`,TransVsEsp$`Riñón-2017`,TransVsEsp$`Riñón-2016`)
IPS<-rep(TransVsEsp$IPS,4)
Año<-rep(c("2019","2018","2017","2016"),each=21)

data<-data.frame(IPS,Transplantes,Espera,Año)
#opcion1
ggplot(data,aes(x=Transplantes,y=Espera,colour=Año))+
  geom_point()+
  geom_text_repel(aes(label=IPS))
#opcion 2 (esta es por los años)
ggplot(data,aes(x=Transplantes,y=Espera))+
  geom_point(col="grey")+
  facet_wrap(~Año)+
  geom_text_repel(aes(label=IPS), size = 2,position="identity")+
  labs(x="Trasplantes", y="Lista de espera")+
  theme_bw()
#opcion 3 (esta por la IPS)
ggplot(data,aes(x=Transplantes,y=Espera,colour=IPS))+
  geom_point()+
  facet_wrap(~IPS)+
  geom_text(aes(label=Año),vjust = -0.1, size = 2)
########3###### REALIZANDO UN RESUMEN #####################
TransVsEsp$IPS->IPS
TransVsEsp<-TransVsEsp[,-1]
rownames(TransVsEsp)<-IPS
# en el 2019 la IPS que mas trasplanto fue Valle de lili  

data<-data %>% 
  mutate(Tasa=round((Transplantes/Espera),2))
  
data %>% group_by(Año) %>% 
  summarise(max=max(Transplantes), media=mean(Transplantes),n=n())
  
data2<-data[,-c(2,3)]
#rownames(data2)<-data$IPS
spread(data2,Año,Tasa) %>% group_by(IPS) %>% 
      summarise(max_219=max(`2019`))
  
apply(spread(data2,Año,Tasa),2,max)
