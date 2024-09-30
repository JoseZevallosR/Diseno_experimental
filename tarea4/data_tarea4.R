# Load necessary library
library(dplyr)
library(agricolae)

# Crear el dataframe original con los valores medios y la desviación estándar
data <- data.frame(
  Tratamiento = c("N0", "N20", "N40", "N60", "N80"),
  Nmin_mean = c(30.4, 29.0, 26.7, 26.9, 32.8),
  Nmin_sd = c(1.8, 2.3, 1.2, 1.9, 4.3),
  NO3_mean = c(4.8, 5.5, 4.4, 4.4, 6.5),
  NO3_sd = c(1.3, 0.5, 0.4, 1.1, 1.1),
  NH4_mean = c(25.6, 23.5, 21.3, 22.5, 26.3),
  NH4_sd = c(0.9, 1.9, 1.2, 1.2, 3.4)
)

# Crear un dataframe vacío para almacenar los resultados
data_expanded <- data.frame()

# Iterar sobre cada fila del dataframe original y crear las repeticiones
for (i in 1:nrow(data)) {
  # Para cada variable (Nmin, NO3-N, NH4+-N), generar las tres repeticiones
  temp <- data.frame(
    Fecha = rep("2018-05-29", 9),
    Tratamiento = rep(data$Tratamiento[i], 9),
    Variable = rep(c("Nmin", "NO3-N", "NH4+-N"), each = 3),
    Valor = c(
      data$Nmin_mean[i] - data$Nmin_sd[i], data$Nmin_mean[i], data$Nmin_mean[i] + data$Nmin_sd[i],
      data$NO3_mean[i] - data$NO3_sd[i], data$NO3_mean[i], data$NO3_mean[i] + data$NO3_sd[i],
      data$NH4_mean[i] - data$NH4_sd[i], data$NH4_mean[i], data$NH4_mean[i] + data$NH4_sd[i]
    )
  )
  # Añadir los datos temporales al dataframe expandido
  data_expanded <- rbind(data_expanded, temp)
}

data_expanded$Fecha = NULL
# Ordenar el dataframe expandido por la columna "Variable"
data_expanded <- arrange(data_expanded, Variable)
# Reorganizar las columnas para que "Variable" sea la primera
data_expanded <- select(data_expanded, Variable, everything())

# Mostrar el dataframe resultante
print(data_expanded)

# Asegurarse de que los factores estén correctamente definidos
data_expanded$Tratamiento <- as.factor(data_expanded$Tratamiento)
data_expanded$Variable <- as.factor(data_expanded$Variable)

#Anova para DBCA
res.aov <- aov(Valor ~ Tratamiento+Variable, data = data_expanded)
summary(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)

bartlett.test(Valor ~ Variable, data = data_expanded)
bartlett.test(Valor ~ Tratamiento, data = data_expanded)

#Durbin-Watson test
lmtest::dwtest(res.aov)

tukeyMetodo1 = HSD.test(res.aov,"Tratamiento", alpha =0.05, group=T)
plot(tukeyMetodo1,variation="IQR")

tukeyMetodo2 = HSD.test(res.aov,"Variable", alpha =0.05, group=T)
plot(tukeyMetodo2,variation="IQR")


# Cargar la librería ggplot2
library(ggplot2)

# Calcular los valores promedio por tratamiento y variable
intEf <- aggregate(Valor ~ Tratamiento + Variable, FUN = mean, data = data_expanded)

# Crear el gráfico con las unidades añadidas
effects_interaction <- ggplot(intEf, aes(x = Tratamiento, y = Valor, color = Variable)) +
  geom_point() + 
  geom_line(aes(group = Variable)) +
  labs(
    y = "Valor (kg N ha⁻¹)",
    x = "Tratamiento",
    color = "Variable (Unidad)"
  ) +
  scale_color_manual(
    values = c("NH4+-N" = "red", "Nmin" = "green", "NO3-N" = "blue"),
    labels = c(
      "NH4+-N (kg N ha⁻¹)", 
      "Nmin (kg N ha⁻¹)", 
      "NO3-N (kg N ha⁻¹)"
    )
  )

# Mostrar el gráfico
effects_interaction

#Anova para Factorial 2
res.aov <- aov(Valor ~ Tratamiento+Variable + Tratamiento*Variable, data = data_expanded)
summary(res.aov)