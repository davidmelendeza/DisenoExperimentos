db= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(db)
db$motor <- as.factor(db$motor)
head(db)

library(dplyr)

db %>%
  group_by(motor) %>%
  summarise(mean = mean(respuesta),
            sd = sd(respuesta)) 

#create boxplots
boxplot(respuesta ~ motor,
        data = db, # data2,
        main = "Tiempo de respuesta de cada motor",
        xlab = "Motor",
        ylab = "Tiempo de respuesta",
        col = "steelblue",
        border = "black")

#fit the one-way ANOVA model
model <- aov(respuesta ~ motor, data = db)
summary(model)

plot(model)


# Se toman los residuales del modelo
residuos <- residuals(model)
# Se crea un gráfico de Residuales vs Tiempo (variable obs)
plot(obs, residuos,
     main = "Residuales vs Tiempo",
     xlab = "Tiempo",
     ylab = "Residuales",
     pch = 20, # Tipo de punto
     col = "blue") # Color de los puntos
# Se añade una línea horizontal en y = 0 para facilitar la interpretación
abline(h = 0, col = "red", lty = 2)

# Se crea un gráfico de Residuales vs Tiempo (variable obs)
plot(residuos,
     main = "Residuales vs Tiempo",
     xlab = "Tiempo",
     ylab = "Residuales",
     pch = 20, # Tipo de punto
     col = "blue") # Color de los puntos

shapiro.test(model$residuals)

# Bartlett
bartlett.test(respuesta ~ motor, data = db)

#load car package
library(car)
leveneTest(respuesta ~ motor, data = db)

TukeyHSD(model, conf.level=.95) 

plot(TukeyHSD(model, conf.level=.95)) 

#install.packages("lsr")
library(lsr)
etaSquared(model, anova=TRUE)

power.anova.test(groups = 3, n = 30, between.var = 98.92, within.var = 139.56, sig.level = 0.05)

power.anova.test(groups = 3, between.var = 98.92, within.var = 139.56, power = 0.8, sig.level = 0.05)
