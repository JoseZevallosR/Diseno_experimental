setwd("D:/Proyectos_GitHub/Diseno_experimental/trabajo_individual2")
library(ggplot2)
library(reshape2)

completar_datos_regresion_multiple <- function(data_xts) {
  # Guardar una copia de la serie original
  serie_original <- data_xts

  # Asegurarse de que los datos sean numéricos y excluir la columna de fechas
  if ("Fecha" %in% colnames(data_xts)) {
    rownames(data_xts) <- data_xts$Fecha
    data_xts <- data_xts[, -which(colnames(data_xts) == "Fecha")]
  }
  data_xts <- as.data.frame(lapply(data_xts, as.numeric))

  # Variables de seguimiento
  prev_na_count <- Inf
  estaciones_excluidas <- c()
  iteration <- 0
  errores_rmse <- list()

  repeat {
    iteration <- iteration + 1

    # Identificar valores faltantes
    nas_por_estacion <- colSums(is.na(data_xts))
    porcentaje_nas <- (nas_por_estacion / nrow(data_xts)) * 100

    # Filtrar estaciones válidas
    estaciones_validas <- names(porcentaje_nas[porcentaje_nas <= 30])
    estaciones_invalidas <- names(porcentaje_nas[porcentaje_nas > 30])

    # Registrar estaciones excluidas
    estaciones_excluidas <- unique(c(estaciones_excluidas, estaciones_invalidas))

    if (length(estaciones_validas) == 0) {
      cat("No quedan estaciones con menos del 30% de datos faltantes.\n")
      break
    }

    # Ordenar estaciones por número de valores faltantes
    estaciones_ordenadas <- sort(nas_por_estacion[estaciones_validas], decreasing = TRUE)

    # Verificar mejora
    total_nas <- sum(nas_por_estacion)
    if (total_nas == 0 || total_nas >= prev_na_count) {
      cat("No hay más mejoras o todos los NAs han sido completados.\n")
      break
    }

    prev_na_count <- total_nas

    # Completar datos faltantes por estación
    for (columna_a_completar in names(estaciones_ordenadas)) {
      if (nas_por_estacion[columna_a_completar] > 0) {
        # Seleccionar predictores
        predictores <- setdiff(estaciones_validas, columna_a_completar)
        if (length(predictores) == 0) next

        # Crear conjuntos de entrenamiento
        data_entrenamiento <- data_xts[complete.cases(data_xts[, c(columna_a_completar, predictores)]), ]
        if (nrow(data_entrenamiento) == 0) next

        X <- as.matrix(data_entrenamiento[, predictores])
        y <- data_entrenamiento[, columna_a_completar]

        # Dividir los datos en entrenamiento (80%) y prueba (20%)
        set.seed(42)
        train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))
        X_train <- X[train_indices, ]
        X_test <- X[-train_indices, ]
        y_train <- y[train_indices]
        y_test <- y[-train_indices]

        # Modelo de regresión múltiple
        modelo_multiple <- lm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)))
        tryCatch({
          # Calcular predicciones en el conjunto de prueba y el RMSE
          y_pred <- predict(modelo_multiple, newdata = as.data.frame(X_test))
          rmse <- sqrt(mean((y_test - y_pred)^2))
          errores_rmse[[columna_a_completar]] <- rmse
        }, error = function(e) {
          cat(sprintf("Error al ajustar modelo para %s: %s\n", columna_a_completar, e$message))
          next
        })

        # Predecir valores faltantes
        data_faltantes <- data_xts[is.na(data_xts[, columna_a_completar]), ]
        if (nrow(data_faltantes) > 0) {
          X_faltantes <- as.matrix(data_faltantes[, predictores])
          if (any(is.na(X_faltantes))) next
          predicciones <- predict(modelo_multiple, newdata = as.data.frame(X_faltantes))
          data_xts[rownames(data_faltantes), columna_a_completar] <- predicciones
        }
      }
    }

    cat(sprintf("Iteración %d - Total NAs restantes: %d\n", iteration, total_nas))
    if (iteration > 5) break
  }

  # Retornar datos completados, serie original, estaciones excluidas y errores RMSE
  list(
    SerieOriginal = serie_original,
    SerieCompletada = data_xts,
    EstacionesExcluidas = estaciones_excluidas,
    ErroresRMSE = errores_rmse
  )
}

# Cargar los datos y ejecutar la función
data_xts <- read.csv("datos_meses.csv")
resultado <- completar_datos_regresion_multiple(data_xts)

# Imprimir resultados
print(resultado$EstacionesExcluidas)
print(resultado$ErroresRMSE)
head(resultado$SerieOriginal)
head(resultado$SerieCompletada)

graficar_rmse_por_estacion <- function(errores_rmse) {
  # Verificar que haya datos para graficar
  if (length(errores_rmse) == 0) {
    cat("No hay valores de RMSE para graficar.\n")
    return(NULL)
  }

  # Convertir los datos a un data frame para ggplot2
  data_rmse <- data.frame(
    Estacion = names(errores_rmse),
    RMSE = as.numeric(errores_rmse)
  )

  # Crear el gráfico de barras
  ggplot(data_rmse, aes(x = Estacion, y = RMSE)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.8) +
    labs(
      title = "RMSE por Estación",
      x = "Estaciones",
      y = "RMSE"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      panel.grid.major.y = element_line(linetype = "dashed", color = "gray", size = 0.5)
    )
}


graficar_rmse_por_estacion(errores_rmse = resultado$ErroresRMSE)


# Calcular la matriz de correlación
calcular_matriz_correlacion <- function(data) {
  # Asegurarse de que todas las columnas sean numéricas
  data <- data[, sapply(data, is.numeric)]

  if (ncol(data) == 0) {
    stop("No hay columnas numéricas en los datos.")
  }

  # Eliminar columnas con demasiados valores faltantes
  data <- data[, colSums(!is.na(data)) > 1]

  if (ncol(data) == 0) {
    stop("No hay columnas suficientes con datos para calcular la correlación.")
  }

  # Calcular la matriz de correlación
  matriz_correlacion <- cor(data, use = "complete.obs")
  return(matriz_correlacion)
}

# Graficar la matriz de correlación
graficar_matriz_correlacion <- function(matriz_correlacion) {

  # Convertir la matriz de correlación a un formato largo para ggplot2
  matriz_larga <- melt(matriz_correlacion)

  # Crear el mapa de calor
  ggplot(matriz_larga, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab") +
    labs(title = "Matriz de Correlación entre Estaciones", x = "Estaciones", y = "Estaciones", fill = "Correlación") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      plot.title = element_text(size = 16, face = "bold")
    )
}

matriz_correlacion <- calcular_matriz_correlacion(resultado$SerieCompletada)
print(matriz_correlacion)

graficar_matriz_correlacion(matriz_correlacion)

graficar_comparacion <- function(resultado, estacion) {
  # Verificar que la estación exista en los datos
  if (!(estacion %in% colnames(resultado$SerieOriginal))) {
    cat(sprintf("La estación '%s' no existe en los datos.\n", estacion))
    return(NULL)
  }

  # Extraer las series original y completada
  serie_original <- resultado$SerieOriginal[, estacion, drop = FALSE]
  serie_completada <- resultado$SerieCompletada[, estacion, drop = FALSE]

  # Combinar ambas series en un solo data frame para ggplot2
  data_grafico <- data.frame(
    Fecha = resultado$SerieOriginal$Fecha,
    Original = as.numeric(serie_original[, 1]),
    Completada = as.numeric(serie_completada[, 1])
  )

  # Pasar a formato largo para ggplot2
  data_grafico <- reshape2::melt(data_grafico, id.vars = "Fecha", variable.name = "Tipo", value.name = "Valor")
  data_grafico$Fecha <- as.Date(data_grafico$Fecha)  # Asegurar que la fecha sea de tipo Date

  # Crear el gráfico
  ggplot(data_grafico, aes(x = Fecha, y = Valor, color = Tipo, linetype = Tipo)) +
    geom_line(size = 1) +
    labs(
      title = sprintf("Comparación de la Estación: %s", estacion),
      x = "Fecha",
      y = "Precipitación",
      color = "Serie",
      linetype = "Serie"
    ) +
    scale_color_manual(values = c("blue", "orange")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
}

graficar_comparacion(resultado, "chalaco")
graficar_comparacion(resultado, "huamarca")
graficar_comparacion(resultado, "huancabamba")
