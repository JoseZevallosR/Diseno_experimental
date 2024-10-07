# Cargamos las librerías necesarias
library(tidyr)
library(dplyr)
library(agricolae)
# Cargar la librería ggplot2
library(ggplot2)
library(lmtest)
library(car) # Para la verificación de suposiciones

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
datos_largos =  data_expanded
names(datos_largos) = c('Emisores','Tratamiento','N2O')

datos_largos$Emisores = as.factor(datos_largos$Emisores)
#Anova 
res.aov <- aov(N2O ~ Emisores, data = datos_largos)
summary(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals) #datos normales ya que p valor + 0.05
#QQ-plot para verificar normalidad
qqPlot(aov_residuals, main = "QQ-Plot de Residuos")

bartlett.test(N2O ~ Emisores, data = datos_largos) #homoceasticidad 

#Durbin-Watson test
lmtest::dwtest(res.aov) # no hay autocorrelacion

tukeyMetodo2 = HSD.test(res.aov,"Emisores", alpha =0.05, group=T)
plot(tukeyMetodo2,variation="IQR")

############################################################
################Transformacion de datos#####################
############################################################

################################
grouped_data <- datos_largos %>%
  group_by(Emisores) %>%
  summarise(y_bar_i = mean(N2O), S_i = sd(N2O))

# Apply the log transformation
grouped_data <- grouped_data %>%
  mutate(ln_y_bar_i = log(y_bar_i), ln_S_i = log(S_i))

# Plot ln(S_i) against ln(y_bar_i)
plot(grouped_data$ln_y_bar_i, grouped_data$ln_S_i, 
     xlab = expression(ln(bar(y)[i])), ylab = expression(ln(S[i])),
     main = "Plot of ln(S_i) against ln(y_bar_i)",
     pch = 19, col = "blue")

# Adding a linear regression line
model <- lm(ln_S_i ~ ln_y_bar_i, data = grouped_data)
abline(model, col = "red")
print(model)

# Extract coefficients
intercept <- round(coef(model)[1], 2)  # intercept
slope <- round(coef(model)[2], 2)      # slope

# Create the equation text
equation <- paste0("ln(S_i) = ", intercept, " + ", slope, " * ln(y_bar_i)")

# Add the equation to the plot
text(x = min(grouped_data$ln_y_bar_i), y = max(grouped_data$ln_S_i), 
     labels = equation, pos = 4, col = "red")

# Calculate alpha and lambda
alpha <- model$coefficients[2]
landa <- 1 - alpha
landa

datos_largos$transform = sqrt(datos_largos$N2O)

datos_largos$Emisores = as.factor(datos_largos$Emisores)

res.aov2 <- aov(transform ~ Emisores, data = datos_largos)
summary(res.aov2)

bartlett.test(transform ~ Emisores, data = datos_largos) #homoceasticidad 

#Durbin-Watson test
lmtest::dwtest(res.aov2) # no hay autocorrelacion

tukeyMetodo2 = HSD.test(res.aov2,"Emisores", alpha =0.05, group=T)
plot(tukeyMetodo2,variation="IQR")

# Crear el histograma de N2O, coloreando por el tipo de Emisores
ggplot(datos_largos, aes(x = N2O, fill = Emisores)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +  # Histograma con contorno negro y transparencia
  labs(title = "Histograma de N2O por Emisores", x = "N2O", y = "Frecuencia") +  # Etiquetas de ejes y título
  theme_minimal() +  # Tema limpio
  scale_fill_brewer(palette = "Set3")  # Paleta de colores para diferenciar emisores


# Crear el histograma de N2O, coloreando por el tipo de Emisores
ggplot(datos_largos, aes(x = transform, fill = Emisores)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +  # Histograma con contorno negro y transparencia
  labs(title = "Histograma de transform por Emisores", x = "N2O transformado", y = "Frecuencia") +  # Etiquetas de ejes y título
  theme_minimal() +  # Tema limpio
  scale_fill_brewer(palette = "Set3")  # Paleta de colores para diferenciar emisores
