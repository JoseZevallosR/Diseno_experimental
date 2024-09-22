setwd("D:/Proyectos_GitHub/Diseno_experimental/clase")

my_data = read.csv("DCA.csv",sep = ';')

anova(lm(PRODUCCION ~ METODOS, data = my_data))

res.aov <- aov(PRODUCCION ~ METODOS, data = my_data)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

# 1. Normality
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

plot(res.aov, 2)
# 2. Homogeneity of variances
result = bartlett.test(PRODUCCION ~ METODOS, data = my_data) 
# print the result 
print(result)
