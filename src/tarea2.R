setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea2")

#librerias para series de tiempo
library(lubridate)
library(zoo)
#librerias para graficos
library(plotly)
library(ggplot2)
library(readr)  # Asegúrate de tener esta librería para usar `parse_number`

#aa1 es temperatura del ano 2022
#aa2 es temperatura del ano 2023
aa1<-read.csv('2022_Toribio_04_1-1-22_00-00_1_Year_1719460093_v2.csv', header = F,skip=6)
aa1<-aa1[1:(dim(aa1)[1]-1),]
aa2<-read.csv('2023_Toribio_04_1-1-23_00-00_1_Year_1719460101_v2.csv', header = F,skip=6)
aa2<-aa2[1:(dim(aa2)[1]-1),]

dates1 <- as.POSIXct(aa1$V1, format = "%d/%m/%Y %H:%M")
dates2 <- as.POSIXct(aa2$V1, format = "%d/%m/%Y %H:%M")

# Combinar datos
aa <- rbind(aa1, aa2)
dates <- c(dates1, dates2)

# Crear objeto zoo para la temperatura (V3 se supone que es la columna de temperatura)
datazoo <- zoo(readr::parse_number(aa$V3) / 10, order.by = dates)

# Calcular la media mensual
mes_temp <- aggregate(datazoo, format(index(datazoo), "%Y-%m"), mean)

# Separar las temperaturas de 2022 y 2023
temp_2022 <- mes_temp[1:12]
temp_2023 <- mes_temp[13:24]

#plot(index(mes_temp),coredata(mes_temp),xlab="Fecha",ylab="Temperatura",main="Temperatura promedio mensual - Chachapoyas 2022-2023",lwd=4, pch = 19,type="b",cex.lab=1.5, cex.axis=2, cex.main=2.5,col="brown1")
# Realizar el test de t de Student
t_test_result <- t.test(coredata(temp_2022), coredata(temp_2023), var.equal = TRUE)

print(t_test_result)

#Precipitacion
datazoo <- zoo(readr::parse_number(aa$V18)/10, order.by = dates)
yyddmm=paste0(sprintf("%02d", year(dates)),sprintf("%02d", month(dates)),sprintf("%02d", day(dates)))
yyddmm2=c(rep("211231",28),yyddmm[1:(length(yyddmm)-28)])
df<-data.frame(data=datazoo,dates=dates,yyddmm=yyddmm2)
day_pp<-aggregate(datazoo ~ yyddmm, data=df, FUN=sum)
day_pp<-day_pp[2:dim(day_pp)[1],]
str(day_pp$datazoo)

#---------------- Estimación de acumulados diarios ----------------------------
datesdays<-seq(as.Date('2022-01-01'),as.Date('2023-12-31'),by='days')
datazoo_day <- zoo(day_pp$datazoo, order.by = datesdays)
plot(datazoo_day,xlab="Fecha",ylab="Precipitación",main="Precipitación diaria - Chachapoyas 2022-2023",
     cex.lab=1.7, cex.axis=2, cex.main=2.5,col="olivedrab3",lwd=2.5)
grid()

# Corregir la función aggregate para sumar la precipitación mensual
monthly_sum <- aggregate(day_pp$datazoo, by = list(format(index(datazoo_day), "%Y-%m")), FUN = sum)
names(monthly_sum) = c('Fecha','pp')

# Separar precipitacion de 2022 y 2023
pcp_2022 <- monthly_sum[1:12, ]
pcp_2023 <- monthly_sum[13:24, ]

# Corregir la conversión de las fechas a Date
monthly_sum$Fecha <- as.Date(paste0(monthly_sum$Fecha, "-01"))
# Graficar la precipitación mensual
plot(x = monthly_sum$Fecha, y = monthly_sum$pp,
     xlab = "Fecha", ylab = "Precipitación", 
     main = "Precipitación mensual - Chachapoyas 2022-2023",
     cex.lab = 1.7, cex.axis = 2, cex.main = 2.5,
     col = "olivedrab3", lwd = 2.5, type = "b") # type = "b" para líneas y puntos
grid()

# Realizar el test de t de Student
pp_test_result <- t.test(coredata(pcp_2022$pp), coredata(pcp_2023$pp), var.equal = TRUE)
print(pp_test_result)







