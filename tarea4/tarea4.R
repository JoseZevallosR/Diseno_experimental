setwd("D:/Doctorado/Experimental")
library(lmtest)
library(agricolae)

data = read.csv(file = "bauermcnett.csv",sep = ';')

str(data)

data$Especie <- as.factor(data$Especie)
data$Tratamiento <- as.factor(data$Tratamiento)
data$Malla <- as.factor(data$Malla)

str(data)

res.aov = aov(WRV ~ Especie + Tratamiento + Malla , data = data)
print(reas.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)

# Homosteasis
#se mantiene la hipotesis de homogeneidad en agrupamiento por especie
bartlett.test(WRV ~ Especie, data = data)
#se mantiene la hipotesis de homogeneidad en agrupamiento por tratamiento
bartlett.test(WRV ~ Tratamiento, data = data)
# si se puede rechazar la hipotesis nula de homogeneidad de varianzas
bartlett.test(WRV ~ Malla, data = data)

#Durbin-Watson test
lmtest::dwtest(res.aov)

#Tukey DBCA

TukeyMetodo <- HSD.test(res.aov, "Malla", alpha = 0.05, group = T)
TukeyMetodo

# Esta tabla muestra las medias de cada grupo (malla) junto con las letras asignadas que indican los grupos homogéneos según la prueba de Tukey:
# Malla 30 tiene una media de WRV de 0.839025 y está etiquetada con la letra "a", lo que significa que es significativamente diferente de los otros grupos.
# Mallas 50, 100, y 200 tienen medias significativamente más bajas y están etiquetadas con la letra "b", lo que indica que no hay diferencias significativas entre estos tres grupos.
