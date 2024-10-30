setwd("~/GitHub/Diseno_experimental/tarea8")
datos=read.table("datos.csv", header = T,  sep=";",  dec="."     )
head(datos)
str(datos)
# Gráfico de cajas
par(mfrow=c(1,1))
boxplot(datos[,1:4],main="Diagrama de cajas",
        xlab="Variables con diferentes unidades",
        ylab="Puntaje",
        col=c("springgreen1","yellow","tomato","pink"))
# Calculos y graficos de correlaciones
# Correlacion de Pearson
midato=cor(datos,method="pearson")
cat("Correlacion de Pearson\n")
print(midato)
# H0: Rho=0
prueba=cor.test(datos$Sabor,datos$AA,method="pearson")
print(prueba)
# De otra manera
plot(datos$AA,datos$Sabor,main="Grafica de Dispersion",col="blue")
segments(4,0,6.8,40,col="17")
cor(datos)
cor(datos)[1,2]
cor.test(datos$Sabor,datos$AA)
# Graficos de Correlacion
data2=datos

library(corrplot)
i=cor(data2,method="pearson")
corrplot(i,sig.level=0.05,type="lower")

