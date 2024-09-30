setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea4/")
library(lmtest)
library(agricolae)
library(ggplot2) # Para la creación de gráficos
library(car) # Para la verificación de suposiciones

data = read.csv(file = "bauermcnett.csv",sep = ';')

str(data)

data$Especie <- as.factor(data$Especie)
data$Tratamiento <- as.factor(data$Tratamiento)
data$Malla <- as.factor(data$Malla)

str(data)

res.aov = aov(Peso ~ Especie + Tratamiento + Malla , data = data)
print(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)

# Homosteasis

bartlett.test(Peso ~ Especie, data = data)

bartlett.test(Peso ~ Tratamiento, data = data)

bartlett.test(Peso ~ Malla, data = data)

#Durbin-Watson test
lmtest::dwtest(res.aov)

#Tukey DBCA

TukeyMetodo <- HSD.test(res.aov, "Malla", alpha = 0.05, group = T)
print(TukeyMetodo)

# Esta tabla muestra las medias de cada grupo (malla) junto con las letras asignadas que indican los grupos homogéneos según la prueba de Tukey:
# Malla 30 tiene una media de WRV de 0.839025 y está etiquetada con la letra "a", lo que significa que es significativamente diferente de los otros grupos.
# Mallas 50, 100, y 200 tienen medias significativamente más bajas y están etiquetadas con la letra "b", lo que indica que no hay diferencias significativas entre estos tres grupos.
# Gráficos para reportar los resultados

# 1. Histograma de residuos
ggplot(data.frame(aov_residuals), aes(x = aov_residuals)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
  labs(title = "Histograma de Residuos", x = "Residuos", y = "Frecuencia") +
  theme_minimal()

# 2. QQ-plot para verificar normalidad
qqPlot(aov_residuals, main = "QQ-Plot de Residuos")

# 3. Gráfico de dispersión de residuos vs valores ajustados
fitted_values <- fitted(res.aov)
ggplot(data.frame(Fitted = fitted_values, Residuals = aov_residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Gráfico de Residuos vs Valores Ajustados", x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()

# 4. Gráfico de medias con intervalo de confianza para el análisis de Tukey
Tukey_plot_data <- data.frame(Treatment = rownames(TukeyMetodo$groups), 
                              Mean = TukeyMetodo$means[,1], 
                              Lower = TukeyMetodo$means[,1] - TukeyMetodo$means[,2], 
                              Upper = TukeyMetodo$means[,1] + TukeyMetodo$means[,2])

ggplot(Tukey_plot_data, aes(x = Treatment, y = Mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = "Intervalo de Confianza de Tukey para las Medias", x = "Tratamiento (Malla)", y = "Media Peso") +
  theme_minimal()
