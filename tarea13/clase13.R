# Configuración del directorio de trabajo
setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea13")

# Diseño Box-Wilson (Diseño Central Compuesto Rotable)
# Dr. Christian R. Encina Zelada

# Cargar y preparar los datos
datos1 <- read.table("1. Fenol Nuez de Betel.csv",
                     header = TRUE, sep = ";", dec = ".") # Lectura de datos
datos1 <- datos1[2:5] # Selección de las columnas relevantes
print(datos1)

# Librería rsm para superficies de respuesta
library(rsm)

# Codificación de variables (centrado y escalado)
CR1 <- coded.data(datos1,
                  x1 ~ (temperatura - 50) / 10,
                  x2 ~ (tiempo - 40) / 10,
                  x3 ~ (ratio - 40) / 10)
print(CR1)
print(as.data.frame(CR1)) # Ver datos codificados

# Ajuste del modelo de superficie de respuesta de segundo orden
CR3.rsm <- rsm(fenolicos ~ SO(x1, x2, x3), data = CR1)
summary(CR3.rsm)

# Verificación de supuestos del modelo
## 1. Normalidad de los errores
Residuales <- CR3.rsm$residuals
shapiro_test_result <- shapiro.test(Residuales)
print(shapiro_test_result)

## 2. Homocedasticidad (Prueba Breusch-Pagan)
library(lmtest)
homocedasticity_test_result <- bptest(CR3.rsm)
print(homocedasticity_test_result)

## 3. Independencia de los errores
# a) Prueba Durbin-Watson
dw_test_result <- dwtest(CR3.rsm, alternative = "two.sided")
print(dw_test_result)

# b) Prueba de Breusch-Godfrey
bg_test_result <- bgtest(CR3.rsm)
print(bg_test_result)

# Nota sobre el valor óptimo
cat("El valor óptimo reportado es:", 164.7406, "\n")

nuevo=data.frame(x1=0.7970510,x2=0.1650555,x3=1.3336148)
predict.lm(CR3.rsm,nuevo)
