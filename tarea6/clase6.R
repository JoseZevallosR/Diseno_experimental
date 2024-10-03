setwd("D:/Proyectos_GitHub/Diseno_experimental/tarea6")
# Load necessary library
library(dplyr)
library(agricolae)
library(car) # Para la verificación de suposiciones

data = read.table("ascorbico.txt", header = T)

# Calculate mean (y_bar_i) and standard deviation (S_i) for each treatment (metodo)
library(dplyr)
grouped_data <- data %>%
  group_by(metodo) %>%
  summarise(y_bar_i = mean(ascorbico), S_i = sd(ascorbico))

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

data$transform = sqrt(data$ascorbico)

data$metodo = as.factor(data$metodo)
#Anova 
res.aov <- aov(transform ~ metodo, data = data)
summary(res.aov)

# Si hay normalidad en nuestros datos
aov_residuals = residuals(res.aov)
shapiro.test(aov_residuals) #datos normales ya que p valor + 0.05
#QQ-plot para verificar normalidad
qqPlot(aov_residuals, main = "QQ-Plot de Residuos")

bartlett.test(transform ~ metodo, data = data) #homoceasticidad 

#Durbin-Watson test
lmtest::dwtest(res.aov) # no hay autocorrelacion

mod2 = lm(transform~metodo,data = data)
#Score Test For Non-Constant Error Variance
ncvTest(mod2)
#Levene's Test for Homogeneity of Variance
leveneTest(mod2)

##################################################
ascorbico<-read.table("ascorbico.txt", header=T)
y<-ascorbico[,1]
metodo<-as.factor(ascorbico[,2])
mod1<-lm(y~metodo)
anova(mod1)

### SUPUESTOS
# 1. Homogeneity of variances
#Score Test For Non-Constant Error Variance
#install.packages("car")
library(car)
ncvTest(mod1)

#Levene's Test for Homogeneity of Variance
leveneTest(mod1)
leveneTest(y~metodo)

#Bartlett test of homogeneity of variances
bartlett.test(y~metodo)

##TRANSFORMACION DE DATOS
yp<-tapply(y,metodo,mean)
si<-tapply(y,metodo,sd)
lyp<-log(yp)
lsi<-log(si)
plot(lyp,lsi)
plot(lyp,lsi, xlab = "Log Mean",
     ylab = "Log SD", main="Grafica para obtener el valor de alpha")
abline(lm(lsi~lyp))

#Obtencion de la pendientes (alpha)
mod<-lm(lsi~lyp)
mod

##TRANSFORMACION RAIZ CUADRADA
# Transformación raíz
yt<-y^0.5
hist(yt)
mod2<-lm(yt~metodo)
anova(mod2)
### SUPUESTOS
# 1. Homogeneity of variances
#Bartlett test of homogeneity of variances
bartlett.test(yt~metodo)
#Score Test For Non-Constant Error Variance
ncvTest(mod2)
#Levene's Test for Homogeneity of Variance
leveneTest(mod2)

