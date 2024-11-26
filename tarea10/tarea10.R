d_mrs <- data.frame(
  Peptona = c(-1, -1, -1, 0, -1, -1, -1, 1, 0, 1, 0, 1, 1, -1, 1, 1, 1, -1, 1),
  Extracto_Carne = c(-1, 1, 1, 0, -1, 1, -1, -1, 0, 1, 0, -1, 1, -1, 1, -1, 1, 1, -1),
  Extracto_Levadura = c(-1, 1, 1, 0, 1, -1, -1, 1, 0, 1, 0, 1, -1, 1, 1, -1, -1, -1, -1),
  Lactosa = c(-1, -1, 1, 0, -1, 1, 1, 1, 0, -1, 0, -1, 1, 1, 1, 1, -1, -1, -1),
  KH2PO4 = c(-1, -1, 1, 0, 1, -1, 1, -1, 0, -1, 0, 1, -1, -1, 1, 1, 1, 1, -1),
  Biomasa = c(2.179, 1.811, 2.434, 2.584, 2.721, 2.407, 2.140, 4.250, 2.491, 2.963,
              2.407, 2.800, 3.393, 2.096, 3.565, 2.968, 2.582, 2.366, 2.588)
)

d_mrs <- subset(d_mrs, !(Peptona == 0 | Extracto_Carne == 0 | Extracto_Levadura == 0 |
                           Lactosa == 0 | KH2PO4 == 0))
# Conversión de factores a variables categóricas
d_mrs$Peptona <- as.factor(d_mrs$Peptona)
d_mrs$Extracto_Carne <- as.factor(d_mrs$Extracto_Carne)
d_mrs$Extracto_Levadura <- as.factor(d_mrs$Extracto_Levadura)
d_mrs$Lactosa <- as.factor(d_mrs$Lactosa)
d_mrs$KH2PO4 <- as.factor(d_mrs$KH2PO4)
d_mrs$Biomasa <- as.numeric(d_mrs$Biomasa)

attach(d_mrs)

anova_mrs <- aov(Biomasa ~ (Peptona + Extracto_Carne + Extracto_Levadura +
                              Lactosa + KH2PO4), data = d_mrs)
summary(anova_mrs)

# Graficar efectos principales
library(FrF2)
MEPlot(anova_mrs, main = "Efectos Principales - MRS", lwd = par("lwd"))

library(ggplot2)

# Extraer los efectos principales del modelo ANOVA
effects <- coef(anova_mrs)[-1]  # Excluir el intercepto
factors <- names(effects)       # Nombres de los factores

# Crear un data.frame para graficar
effects_df <- data.frame(Factor = factors, Effect = effects)

# Gráfico de efectos principales
ggplot(effects_df, aes(x = reorder(Factor, Effect), y = Effect)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Efectos Principales - MRS", x = "Factores", y = "Efecto") +
  theme_minimal()

d_m17 <- data.frame(
  Peptona = c(-1, -1, -1, 0, -1, -1, -1, 1, 0, 1, 0, 1, 1, -1, 1, 1, 1, -1, 1),
  Extracto_Carne = c(-1, 1, 1, 0, -1, 1, -1, -1, 0, 1, 0, -1, 1, -1, 1, -1, 1, 1, -1),
  Extracto_Levadura = c(-1, 1, 1, 0, 1, -1, -1, 1, 0, 1, 0, 1, -1, 1, 1, -1, -1, -1, -1),
  Lactosa = c(-1, -1, 1, 0, -1, 1, 1, 1, 0, -1, 0, -1, 1, 1, 1, 1, -1, -1, -1),
  Biomasa = c(5.558, 5.100, 4.627, 6.489, 0.840, 4.912, 3.866, 1.056, 1.045, 0.798,
              3.471, 7.782, 8.405, 2.945, 3.068, 2.489, 4.334, 3.889, 1.901)
)


d_m17 <- subset(d_m17, !(Peptona == 0 | Extracto_Carne == 0 | Extracto_Levadura == 0 | Lactosa == 0))

# Conversión de factores a variables categóricas
d_m17$Peptona <- as.factor(d_m17$Peptona)
d_m17$Extracto_Carne <- as.factor(d_m17$Extracto_Carne)
d_m17$Extracto_Levadura <- as.factor(d_m17$Extracto_Levadura)
d_m17$Lactosa <- as.factor(d_m17$Lactosa)

d_m17$Biomasa <- as.numeric(d_m17$Biomasa)

# Adjuntar el data frame
attach(d_m17)


anova_m17 <- aov(Biomasa ~ (Peptona + Extracto_Carne + Extracto_Levadura +
                              Lactosa), data = d_m17)
summary(anova_m17)

# Graficar efectos principales
library(FrF2)
MEPlot(anova_m17, main = "Efectos Principales - M17", lwd = par("lwd"))
