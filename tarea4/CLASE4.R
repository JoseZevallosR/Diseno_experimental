setwd("C:/Users/Agatha/Desktop/DOCTORADO/2024 II/DISEÑO/CLASE 4")

my_data = read.csv("DBCA.csv",sep=';',header = T)

str(my_data)

my_data$metodo <- as.factor(my_data$metodo)
my_data$operador <- as.factor(my_data$operador)

res.aov = aov(resultados ~ metodo + operador , data = my_data)

print(res.aov)

bartlett.test(resultados ~ metodo , data = my_data)

#("lmtest")
#library(lmtest)
dwtest(res.aov2)

# Tuckey libreria AGRICOLAE
install.packages("agricolae")
library(agricolae)
TukeyMetodo <-
  
  str(my_data)

my_data$metodo <- as.factor(my_data$metodo)
my_data$operador <- as.factor(my_data$operador)

res.aov = aov(resultados ~ metodo + operador , data = my_data)

aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals)
print(res.aov)

# Realizar el test de Shapiro-Wilk
bartlett.test(resultados ~ metodo , data = my_data)

library(lmtest)
dwtest(res.aov)

# Tuckey DBCA
install.packages("agricolae")
library(agricolae)
TukeyMetodo <- HSD.test(res.aov, "metodo", alpha = 0.05, group = T)
TukeyMetodo
