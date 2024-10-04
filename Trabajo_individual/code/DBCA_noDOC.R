# Cargar librerías necesarias
library(dplyr)
library(agricolae)
library(car) # Para la verificación de suposiciones

# Crear el dataframe original con los valores medios y la desviación estándar para cada variable
data <- data.frame(
  Intensidad_Pastoreo = c("G0", "G0.7", "G1.2", "G1.6"),
  P_mean = c(0.86, 1.74, 1.14, 1.42),
  P_sd = c(0.04, 0.08, 0.04, 0.12),
  NO3_mean = c(0.07, 0.12, 0.10, 0.08),
  NO3_sd = c(0.004, 0.006, 0.008, 0.01),
  NH4_mean = c(1.26, 1.13, 1.22, 1.37),
  NH4_sd = c(0.08, 0.06, 0.06, 0.11)
)

# Crear un dataframe vacío para almacenar los resultados
data_expanded <- data.frame()

# Iterar sobre cada fila del dataframe original y crear las repeticiones
for (i in 1:nrow(data)) {
  # Para cada variable (P, NO3-N, NH4+-N), generar las tres repeticiones
  temp <- data.frame(
    Fecha = rep("2013", 9),  # Ajuste de fecha para el año del estudio
    Intensidad_Pastoreo = rep(data$Intensidad_Pastoreo[i], 9),
    Psuelo = rep(c("P", "NO3-N", "NH4+-N"), each = 3),
    Valor = c(
      data$P_mean[i] - data$P_sd[i], data$P_mean[i], data$P_mean[i] + data$P_sd[i],
      data$NO3_mean[i] - data$NO3_sd[i], data$NO3_mean[i], data$NO3_mean[i] + data$NO3_sd[i],
      data$NH4_mean[i] - data$NH4_sd[i], data$NH4_mean[i], data$NH4_mean[i] + data$NH4_sd[i]
    )
  )
  # Añadir los datos temporales al dataframe expandido
  data_expanded <- rbind(data_expanded, temp)
}

# Mostrar el dataframe expandido
print(data_expanded)

data_expanded$Fecha = NULL
# Ordenar el dataframe expandido por la columna "Variable"
data_expanded <- arrange(data_expanded, Psuelo)
# Reorganizar las columnas para que "Variable" sea la primera
data_expanded <- select(data_expanded, Psuelo, everything())

# Mostrar el dataframe resultante
print(data_expanded)

# Asegurarse de que los factores estén correctamente definidos
data_expanded$Intensidad_Pastoreo <- as.factor(data_expanded$Intensidad_Pastoreo)
data_expanded$Psuelo <- as.factor(data_expanded$Psuelo)

# Anova para DBCA
res.aov <- aov(Valor ~ Intensidad_Pastoreo + Psuelo, data = data_expanded)
summary(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)
# QQ-plot para verificar normalidad
qqPlot(aov_residuals, main = "QQ-Plot de Residuos")

bartlett.test(Valor ~ Intensidad_Pastoreo, data = data_expanded)
bartlett.test(Valor ~ Psuelo, data = data_expanded)

# Durbin-Watson test
lmtest::dwtest(res.aov)

tukeyMetodo1 = HSD.test(res.aov, "Intensidad_Pastoreo", alpha = 0.05, group = T)
plot(tukeyMetodo1, variation = "IQR")
print(tukeyMetodo1)

tukeyMetodo2 = HSD.test(res.aov, "Psuelo", alpha = 0.05, group = T)
plot(tukeyMetodo2, variation = "IQR")
print(tukeyMetodo2)

# Cargar la librería ggplot2
library(ggplot2)

# Calcular los valores promedio por tratamiento y variable
intEf <- aggregate(Valor ~ Intensidad_Pastoreo + Psuelo, FUN = mean, data = data_expanded)

# Crear el gráfico con las unidades añadidas
effects_interaction <- ggplot(intEf, aes(x = Intensidad_Pastoreo, y = Valor, color = Psuelo)) +
  geom_point() + 
  geom_line(aes(group = Psuelo)) +
  labs(
    y = "Valor (kg N ha⁻¹)",
    x = "Intensidad de Pastoreo",
    color = "Variable (Unidad)"
  ) +
  scale_color_manual(
    values = c("P" = "green", "NO3-N" = "blue", "NH4+-N" = "red"),
    labels = c(
      "P (mg/L)", 
      "NO3-N (mg/L)", 
      "NH4+-N (mg/L)"
    )
  )

# Mostrar el gráfico
print(effects_interaction)

# Datos necesarios Tratamiento
alpha <- 0.05    # Nivel de significancia
a <- 4           # Número de tratamientos
df_error <- 30   # Grados de libertad asociados al error (ν)

# Obtención del valor crítico de q
q_critico <- qtukey(p = 1 - alpha, nmeans = a, df = df_error)

# Mostrar el valor crítico de q
q_critico

# Datos necesarios Bloque
alpha <- 0.05    # Nivel de significancia
a <- 3           # Número de bloques
df_error <- 30   # Grados de libertad asociados al error (ν)

# Obtención del valor crítico de q
q_critico <- qtukey(p = 1 - alpha, nmeans = a, df = df_error)

# Mostrar el valor crítico de q
q_critico

