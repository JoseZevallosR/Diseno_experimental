rm(list=ls())
library(lubridate)
library(zoo)
library(plotly)
library(ggplot2)

#---------------------------------------------------------------------------------------------------------------------------------------
#----------------- Importación de datos en formato .csv -------35040-------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------

dir<-'G:/My Drive/10_TRABAJOS UNALM/1_Diseño estadístico/01_Informe_A_Estadístico_DICA_02_09_2024/Archivos'
aa1<-read.csv('G:/My Drive/10_TRABAJOS UNALM/1_Diseño estadístico/01_Informe_A_Estadístico_DICA_02_09_2024/Archivos/2022_Toribio_04_1-1-22_00-00_1_Year_1719460093_v2.csv', header = F,skip=6)
aa1<-aa1[1:(dim(aa1)[1]-1),]
aa2<-read.csv('G:/My Drive/10_TRABAJOS UNALM/1_Diseño estadístico/01_Informe_A_Estadístico_DICA_02_09_2024/Archivos/2023_Toribio_04_1-1-23_00-00_1_Year_1719460101_v2.csv', header = F,skip=6)
aa2<-aa2[1:(dim(aa2)[1]-1),]
dates1<-as.POSIXct(aa1$V1,format="%d/%m/%Y %H:%M") 
dates2<-as.POSIXct(aa2$V1,format="%d/%m/%Y %H:%M")
aa<-rbind(aa1,aa2)         #### Combinación de tablas de 2022 y 2023
dates<-c(dates1,dates2)    #### Combinación de fechas

#----------------------------------------------------------------------------------------------------------------------------------------
#----------------- Procesamiento de datos de temperaturas -------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------

#---------------- Estimación de promedio diarios ----------------------------
datazoo <- zoo(readr::parse_number(aa$V3)/10, order.by = dates)
#windows();plot(datazoo,type="l",col = "firebrick",ylab="Temperatura",xlab="Tiempo");grid()
yyddmm=paste0(sprintf("%02d", year(dates)),sprintf("%02d", month(dates)),sprintf("%02d", day(dates)))
df<-data.frame(data=datazoo,dates=dates,yyddmm=yyddmm)
#calcula  promedio en f temporalidad
day_temp<-aggregate(datazoo ~ yyddmm, data=df, FUN=mean)
str(day_temp$datazoo)

#---------------- Generación de gráfico de serie temporal diaria ----------------------------
datesdays<-seq(as.Date('2022-01-01'),as.Date('2023-12-31'),by='days')
datazoo_day <- zoo(day_temp$datazoo, order.by = datesdays)
plot(datazoo_day,xlab="Fecha",ylab="Temperatura",main="Temperatura diaria - Chachapoyas 2022-2023",
     cex.lab=1.7, cex.axis=2, cex.main=2.5,col="lightcoral",lwd=2.5)
grid()

#-------------- Generación de gráficos de boxplots -------------------------
dfdatos<-data.frame(meses=month(datesdays,label=TRUE),daytemp=day_temp$datazoo,mesnum=month(datesdays))

## Gráfico con plot_ly
plot_ly(data = dfdatos, x = ~meses, y = ~daytemp, color = ~meses, type = "box")%>%
  layout(title = 'Distribución de temperatura diaria \n Chachapoyas 2022-2023', 
         xaxis = list(title = 'Meses'),
         yaxis = list(title = 'Temperatura (°C)'))

## Gráfico con ggplot
ggplot(data=dfdatos, aes(meses, daytemp,color=meses)) +
  geom_boxplot() + # dibujamos el diagrama de cajas
  theme(text = element_text(size = 20),
        plot.title = element_text(size=24, face="bold",hjust = 0.5)
  )+
  xlab("Meses")+
  ylab("Temperatura")+
  ggtitle("Distribución de temperatura diaria\n Chachapoyas 2022-2023")+
  geom_jitter() 

#-------------- Muestreo Aleatorio simple -------------------------
dat<-dfdatos
nporc=0.10 ### Porcentaje de datos para la muestra(0.10 = 10%)
set.seed(460)

## Muestreo para cada mes
mmes<-list()
for (i in 1:12){
id_muestra<-sample(1:sum(dat$mesnum==i),round(nporc*sum(dat$mesnum==i)))
mesind<-which(dat$mesnum==i)
mmes[[i]]<-dat[mesind[id_muestra],]
}
muestra = do.call(rbind, mmes)

## Gráfico con plot_ly
plot_ly(data = muestra, x = ~meses, y = ~daytemp, color = ~meses, type = "box")%>%
layout(title = 'Muestra de datos de temperatura \n Chachapoyas 2022-2023', 
       xaxis = list(title = 'Meses'),
       yaxis = list(title = 'Temperatura'))

## Gráfico con ggplot
ggplot(data=muestra, aes(meses, daytemp,color=meses)) +
  geom_boxplot() + # dibujamos el diagrama de cajas
  theme(text = element_text(size = 20),
        plot.title = element_text(size=24, face="bold",hjust = 0.5)
  )+
  xlab("Meses")+
  ylab("Temperatura")+
  ggtitle("Muestra de datos de temperatura \n Chachapoyas 2022-2023")+
  geom_jitter()

#----------------------------------------------------------------------------------------------------------------------------------------
#----------------- Procesamientp de datos de precipitación ------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------

#---------------- Estimación de acumulados diarios ----------------------------
datazoo <- zoo(readr::parse_number(aa$V18)/10, order.by = dates)
yyddmm=paste0(sprintf("%02d", year(dates)),sprintf("%02d", month(dates)),sprintf("%02d", day(dates)))
yyddmm2=c(rep("211231",28),yyddmm[1:(length(yyddmm)-28)])
df<-data.frame(data=datazoo,dates=dates,yyddmm=yyddmm2)
day_pp<-aggregate(datazoo ~ yyddmm, data=df, FUN=sum)
day_pp<-day_pp[2:dim(day_pp)[1],]
str(day_pp$datazoo)

#---------------- Generación de gráfico de serie temporal diaria ----------------------------
datesdays<-seq(as.Date('2022-01-01'),as.Date('2023-12-31'),by='days')
datazoo_day <- zoo(day_pp$datazoo, order.by = datesdays)
plot(datazoo_day,xlab="Fecha",ylab="Precipitación",main="Precipitación diaria - Chachapoyas 2022-2023",
     cex.lab=1.7, cex.axis=2, cex.main=2.5,col="olivedrab3",lwd=2.5)
grid()

#-------------- Generación de gráficos de boxplots -------------------------
dfdatos<-data.frame(meses=month(datesdays,label=TRUE),daypp=day_pp$datazoo,mesnum=month(datesdays))

## Gráfico con plot_ly
plot_ly(data = dfdatos, x = ~meses, y = ~daypp, color = ~meses, type = "box")%>%
  layout(title = 'Distribución de precipitación diaria \n Chachapoyas 2022-2023', 
         xaxis = list(title = 'Meses'),
         yaxis = list(title = 'Precipitación (mm/día)'))


## Gráfico con ggplot
ggplot(data=dfdatos, aes(meses, daypp,color=meses)) +
  geom_boxplot() + # dibujamos el diagrama de cajas
  theme(text = element_text(size = 20),
        plot.title = element_text(size=24, face="bold",hjust = 0.5)
  )+
  xlab("Meses")+
  ylab("Precipitación (mm/día)")+
  ggtitle("Distribución de precipitación diaria\n Chachapoyas 2022-2023")+
  geom_jitter() 

#-------------- Muestreo Aleatorio simple -------------------------
dat<-dfdatos
nporc=0.10 ### Porcentaje de datos para la muestra(0.10 = 10%)
set.seed(433)

#--- MUESTREO ALEATORIO SISTEMÁTICO------------------
dat
N=dim(dat)[1]
n=40
r=N/n
r
set.seed(555)
a=sample(r,1, replace = F)
a

id_muestra=seq(a,N,r)
id_muestra

muestra1<-dat[id_muestra,]
muestra1






## Muestreo para cada mes
mmes<-list()
for (i in 1:12){
  id_muestra<-sample(1:sum(dat$mesnum==i),round(nporc*sum(dat$mesnum==i)))
  mesind<-which(dat$mesnum==i)
  mmes[[i]]<-dat[mesind[id_muestra],]
}
muestra = do.call(rbind, mmes)
## Gráfico con plot_ly
plot_ly(data = muestra, x = ~meses, y = ~daypp, color = ~meses, type = "box")%>%
  layout(title = 'Muestra de datos de precipitación \n Chachapoyas 2022-2023', 
         xaxis = list(title = 'Meses'),
         yaxis = list(title = 'Precipitación (mm/día)'))


## Gráfico con ggplot
ggplot(data=muestra, aes(meses, daypp,color=meses)) +
  geom_boxplot() + # dibujamos el diagrama de cajas
  theme(text = element_text(size = 20),
        plot.title = element_text(size=24, face="bold",hjust = 0.5)
  )+
  xlab("Meses")+
  ylab("Precipitación (mm/día)")+
  ggtitle("Muestra de datos de precipitación \n Chachapoyas 2022-2023")+
  geom_jitter()

