#install.packages("rsm")
library(rsm)
library(rgl)
library(scatterplot3d)
setwd("~/GitHub/Diseno_experimental/tarea12")

datos1 = read.csv("BacabaEnPolvo.csv",sep = ';')
CR1 <- coded.data(datos1, x1~(Temperatura - 65)/10,
                  x2~(Maltodextrin - 20)/5,x3~(Velocidad-1.25)/0.05)


CR1 = as.data.frame(CR1)

#Rendimiento Rendimiento
CR0.rsm <- rsm(Rendimiento ~ FO(x1,x2,x3),
               data = CR1)
summary(CR0.rsm)

CR1.rsm <- rsm(Rendimiento ~ SO(x1,x2,x3),
               data = CR1)
summary(CR1.rsm)


########33
# Crear una cuadrícula para los valores de x1 y x2
x1_seq <- seq(-1, 1, length.out = 30)  # Rango para x1
x2_seq <- seq(-1, 1, length.out = 30)  # Rango para x2
x3_seq <- seq(-1, 1, length.out = 30)  # Rango para x2
# Generar la cuadrícula de puntos
# Fijar x3 en un valor constante, por ejemplo, 0
grid_fixed <- expand.grid(x1 = x1_seq, x2 = x2_seq, x3 = 0)

# Predecir valores de Rendimiento usando el modelo
grid_fixed$Rendimiento <- predict(CR1.rsm, newdata = grid_fixed)

# Transformar a matriz para el gráfico 3D
z_matrix_fixed <- matrix(grid_fixed$Rendimiento, nrow = length(x1_seq), ncol = length(x2_seq))

# Graficar
persp(x1_seq, x2_seq, z_matrix_fixed,
      theta = 30, phi = 30,          # Ángulos de rotación
      col = "lightblue",             # Color de la superficie
      xlab = "x1 (Temperatura)",     # Etiqueta eje x
      ylab = "x2 (Maltodextrina)",   # Etiqueta eje y
      zlab = "Rendimiento",          # Etiqueta eje z
      expand = 0.5)

# Graficar los puntos interactivos en 3D con rgl
plot3d(grid$x1, grid$x2, grid$Rendimiento,
       col = "blue", size = 3,
       xlab = "x1 (Temperatura)",
       ylab = "x2 (Maltodextrina)",
       zlab = "Rendimiento")


#Calculando el valor de Rendimiento con los puntos estacionarios codificados
nuevo=data.frame(x1=0.7326014,x2=0.6122960,x3=0.1025705)
predict.lm(CR1.rsm,nuevo)

#superficies de respuesta ajustada.
persp(CR1.rsm,x1~x2,zlab="Rendimiento",col="chartreuse1")
