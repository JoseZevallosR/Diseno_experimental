#install.packages("rsm")
library(rsm)
library(rgl)
library(scatterplot3d)
library(ggplot2)
setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea12")

# Cargar los datos del sistema Kanban/CONWIP
datos_kanban <- read.csv("Simulated_Data_for_Response_Surfaces.csv", sep = ',')

# Seleccionar columnas relevantes
datos_seleccionados <- datos_kanban[, c("Number_of_Cards_A", "Capacity_B", "Flow_Time_E_Low")]

# Renombrar columnas para facilitar su uso
colnames(datos_seleccionados) <- c("Numero_Tarjetas", "Capacidad", "Tiempo_Flujo")

# Codificar las variables independientes
datos_codificados <- coded.data(datos_seleccionados,
                                x1 ~ (Numero_Tarjetas - mean(datos_seleccionados$Numero_Tarjetas)) / sd(datos_seleccionados$Numero_Tarjetas),
                                x2 ~ (Capacidad - mean(datos_seleccionados$Capacidad)) / sd(datos_seleccionados$Capacidad))

# Ajustar el modelo de primer orden (FO)
modelo_FO <- rsm(Tiempo_Flujo ~ FO(x1, x2), data = datos_codificados)
summary(modelo_FO)

# Dirección del ascenso más empinado
steepest(modelo_FO)

# Calcular los efectos principales a partir de los datos originales
efectos_principales <- aggregate(Tiempo_Flujo ~ Numero_Tarjetas + Capacidad,
                                 data = datos_seleccionados, FUN = mean)

# Filtrar para niveles clave de capacidad (por ejemplo: mínimo, medio, máximo)
niveles_clave <- c(min(efectos_principales$Capacidad),
                   median(efectos_principales$Capacidad),
                   max(efectos_principales$Capacidad))
datos_filtrados <- efectos_principales[efectos_principales$Capacidad %in% niveles_clave, ]

# Crear el gráfico mejorado
ggplot(datos_filtrados, aes(x = Numero_Tarjetas, y = Tiempo_Flujo, group = Capacidad, color = factor(Capacidad))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Capacidad (x2)") +
  labs(
    title = "Impacto de Número de Tarjetas y Capacidad sobre el Tiempo de Flujo",
    x = "Número de Tarjetas (x1)",
    y = "Tiempo de Flujo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Ajustar modelo de superficie de respuesta de segundo orden (SO)
modelo_SO <- rsm(Tiempo_Flujo ~ SO(x1, x2), data = datos_codificados)
summary(modelo_SO)

# Punto estacionario
canonical(modelo_SO)

# Instalar librerías necesarias
# install.packages("plotly")
library(plotly)

# Crear una cuadrícula de valores para x1 y x2
x1_vals <- seq(min(datos_codificados$x1), max(datos_codificados$x1), length.out = 50)
x2_vals <- seq(min(datos_codificados$x2), max(datos_codificados$x2), length.out = 50)
grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)

# Predecir el Tiempo de Flujo en la cuadrícula
grid$Tiempo_Flujo <- predict(modelo_SO, newdata = grid)

# Graficar la superficie de respuesta
fig <- plot_ly(
  x = ~grid$x1,
  y = ~grid$x2,
  z = ~grid$Tiempo_Flujo,
  type = "surface",
  colorscale = "Viridis"
)

# Añadir el punto estacionario
fig <- fig %>%
  add_markers(
    x = 14.69767, y = 10.04520, z = predict(modelo_SO, newdata = data.frame(x1 = 14.69767, x2 = 10.04520)),
    marker = list(color = 'red', size = 5),
    name = "Punto estacionario"
  ) %>%
  layout(
    title = "Superficie de Respuesta con Punto Estacionario",
    scene = list(
      xaxis = list(title = "Número de Tarjetas (x1)"),
      yaxis = list(title = "Capacidad (x2)"),
      zaxis = list(title = "Tiempo de Flujo")
    )
  )

# Mostrar el gráfico interactivo
fig


# Crear una cuadrícula de valores para las variables codificadas x1 y x2
x1_vals <- seq(-2, 2, length.out = 30) # Ajusta el rango según tus datos
x2_vals <- seq(-2, 2, length.out = 30)
grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)

# Predecir el Tiempo de Flujo en la cuadrícula
grid$Tiempo_Flujo <- predict(modelo_SO, newdata = grid)

# Convertir la cuadrícula en formato matriz para persp()
z_matrix <- matrix(grid$Tiempo_Flujo, nrow = length(x1_vals), ncol = length(x2_vals))

# Graficar la superficie de respuesta
persp(
  x = x1_vals,
  y = x2_vals,
  z = z_matrix,
  xlab = "x1 (Número de Tarjetas codificado)",
  ylab = "x2 (Capacidad codificada)",
  zlab = "Tiempo de Flujo",
  col = "lightblue",
  theta = 30,
  phi = 20,
  main = "Superficie de Respuesta - Modelo SO"
)
# Exportar resultados del modelo a un archivo
write.csv(summary(modelo_SO)$coefficients, file = "Resultados_Modelo_SO.csv")

