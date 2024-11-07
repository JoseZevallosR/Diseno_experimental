# Librerías necesarias
library(corrplot)

# Datos de ejemplo
datos <- data.frame(
  Concentration = c(0.0, 9.5, 23.7, 47.5, 74.3, 94.9),
  Emission = c(0.000, 3114.369, 10892.702, 23051.704, 34534.168, 44575.015)
)

# Función para calcular y mostrar la correlación de Pearson
calcular_correlacion <- function(datos) {
  midato <- cor(datos, method = "pearson")
  cat("Correlación de Pearson\n")
  print(midato)
  return(midato)
}

# Función para realizar la prueba de hipótesis de correlación de Pearson
prueba_correlacion <- function(datos, var1, var2) {
  prueba <- cor.test(datos[[var1]], datos[[var2]], method = "pearson")
  print(prueba)
}

# Función para realizar regresión lineal y mostrar los resultados
regresion_lineal <- function(datos) {
  modelo <- lm(Emission ~ Concentration, data = datos)
  summary(modelo)
}

# Función para gráfico de dispersión con línea de regresión
grafico_dispersion <- function(datos) {
  plot(datos$Concentration, datos$Emission, main = "Gráfico de Dispersión con Línea de Regresión",
       xlab = "Concentration (μg/L)", ylab = "Emission (cps)", pch = 16, col = "blue")
  abline(lm(Emission ~ Concentration, data = datos), col = "red")
}

# Función para graficar la matriz de correlación con valores de correlación
graficar_corrplot <- function(matriz_cor) {
  corrplot(matriz_cor, sig.level = 0.05, type = "lower", 
           addCoef.col = "black")  # Agrega los valores de correlación en negro
}

# Uso de las funciones con los datos
matriz_cor <- calcular_correlacion(datos)    # Cálculo de correlación
prueba_correlacion(datos, "Concentration", "Emission")  # Prueba de correlación entre Concentration y Emission
regresion_lineal(datos)                      # Resumen de la regresión lineal
grafico_dispersion(datos)                    # Gráfico de dispersión con línea de regresión
graficar_corrplot(matriz_cor)                # Gráfico de correlación
