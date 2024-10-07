setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea6")
# Cargamos las librerías necesarias
library(tidyr)
library(dplyr)


datos_largos = read.csv('../tarea4/datos.csv')
names(datos_largos) = c('Emisores','Tratamiento','N2O')
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

alpha = model$coefficients[2]
landa = 1 - alpha
landa

datos_largos$transform = sqrt(datos_largos$N2O)

datos_largos$Emisores = as.factor(datos_largos$Emisores)
#Anova 
res.aov <- aov(transform ~ Emisores, data = datos_largos)
summary(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals) #datos normales ya que p valor + 0.05
#QQ-plot para verificar normalidad
qqPlot(aov_residuals, main = "QQ-Plot de Residuos")

bartlett.test(transform ~ Emisores, data = datos_largos) #homoceasticidad 

#Durbin-Watson test
lmtest::dwtest(res.aov) # no hay autocorrelacion


