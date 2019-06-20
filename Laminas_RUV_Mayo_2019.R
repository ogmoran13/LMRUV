#Inicio
library(foreign)
library(xlsx)
library(openair)
library(lubridate)
library(tidyverse)
library(dplyr)

# LM6 Registro de Vivienda
RegViv<-(REGISTRO_RUV_5_2019[c(3)])
  #6.1 Preparación BD
RegVivVF<-RegViv %>% 
  mutate(FH_PAGO_REGISTRO = ymd(FH_PAGO_REGISTRO)) %>% 
  mutate_at(vars(FH_PAGO_REGISTRO), list(year, month, day))
  #6.2Corte Año y Mes
RegVivXLM<- RegVivVF %>% 
  filter((fn1== 2019 & fn2==4)|(fn1== 2018 & fn2==4)|(fn1== 2017 & fn2==4))%>% 
  group_by(fn1,fn2)%>%
  count(fn1,fn2)
write.xlsx(as.data.frame(RegVivXLM), "C:/Users/IN335375/Documents/GPE/RUV/Abril2019/RegViv.xlsx")

# LM7 Verificación de Vivienda
VerViv<-(REGISTRO_RUV_5_2019[c(6)])
  #7.1 Preparación BD
VerVivVF<-VerViv %>% 
  mutate(FH_INICIO_VERIFICACION = ymd(FH_INICIO_VERIFICACION)) %>% 
  mutate_at(vars(FH_INICIO_VERIFICACION), list(year, month, day))
  #7.2Corte Año y Mes
VerVivXLM<- VerVivVF %>% 
  filter((fn1== 2019 & fn2==4)|(fn1== 2018 & fn2==4)|(fn1== 2017 & fn2==4))%>% 
  group_by(fn1,fn2)%>%
  count(fn1,fn2)
write.xlsx(as.data.frame(VerVivXLM), "C:/Users/IN335375/Documents/GPE/R/Abril2019/VerViv.xlsx")

# LM8 Producción de Vivienda
ProViv<-(REGISTRO_RUV_5_2019[c(34)])
  #8.1 Preparación BD
ProVivVF<-ProViv %>% 
  mutate(FH_HABITABILIDAD = ymd(FH_HABITABILIDAD)) %>% 
  mutate_at(vars(FH_HABITABILIDAD), list(year, month, day))
  #8.2Corte Año y Mes
ProVivXLM<- ProVivVF %>% 
  filter((fn1== 2019 & fn2==5)|(fn1== 2018 & fn2==5)|(fn1== 2017 & fn2==5))%>% 
  group_by(fn1,fn2)%>%
  count(fn1,fn2)
write.xlsx(as.data.frame(ProVivXLM), "C:/Users/IN335375/Documents/GPE/RUV/Abril2019/ProViv.xlsx")

#LM9 Tipo de vivienda
  #9.1 Vivienda horizontal y vertical
HB<-(vivienda_registrada[c(34,43)])

HBVF<-HB %>% 
  mutate(FH_HABITABILIDAD = ymd(FH_HABITABILIDAD)) %>% 
  mutate_at(vars(FH_HABITABILIDAD), list(year, month, day))

HBVFXL<- HBVF %>% 
filter((fn1== 2019 )|(fn1== 2018 )|(fn1== 2017 ))%>% 
  group_by(fn1,fn2)%>%
  count(CV_TIPO_VIVIENDA)
write.xlsx(as.data.frame(HBVFXL), "C:/Users/IN335375/Documents/GPE/RUV/Abril2019/HBVFXL.xlsx")

# Dictamen antiguedad
AntViv<-(vivienda_registrada[c(34,1)])
  #Preparación BD
AntVivVF<-AntViv %>% 
  mutate(FH_HABITABILIDAD = ymd(FH_HABITABILIDAD)) %>% 
  mutate_at(vars(FH_HABITABILIDAD), list(year, month, day))
  #Corte Año y Mes
AntVivXLM<- AntVivVF %>% 
  filter(IN_CLASIFICADO) %>% 
  group_by(fn1,fn2)%>%
  count(fn1,fn2)
write.xlsx(as.data.frame(AntVivXLM), "C:/Users/IN335375/Documents/GPE/RUV/Abril2019/AntViv.xlsx")
