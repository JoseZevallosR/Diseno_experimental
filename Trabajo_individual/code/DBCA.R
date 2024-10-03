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
    values = c("DOC" = "blue", "P" = "green", "NO3-N" = "red", "NH4+-N" = "purple"),
    labels = c(
      "DOC (mg/L)", 
      "P (mg/L)", 
      "NO3-N (mg/L)", 
      "NH4+-N (mg/L)"
    )
  )

# Mostrar el gráfico
print(effects_interaction)


