install.packages("tidyverse")

library(tidyverse)

redes = (read.csv(file.choose(), header=TRUE, encoding = "UTF-8"))
attach(redes)

head(redes, 10)

dist_largo <- redes %>% filter(distancia == "largo") %>% pull(tiempo)
dist_corto <- redes %>% filter(distancia == "corto") %>% pull(tiempo)
mean(dist_largo) - mean(dist_corto)

plot(redes$tiempo, axes = TRUE , ylab = "Tiempo observado", xlab = "Obser
vaciones")
abline(h = mean(redes$tiempo), col = "blue")


ggplot(redes,aes(x = tiempo)) +
  geom_histogram(aes(y = ..density.., colour = distancia)) +
  facet_grid(.~ distancia) +
  theme_bw() + theme(legend.position = "none") +
  theme (text = element_text(size=16))

par(mfrow = c(1, 2))
qqnorm(dist_largo, xlab = "", ylab = "",
       main = "Distancia larga", col = "firebrick")
qqline(dist_largo)
qqnorm(dist_corto, xlab = "", ylab = "",
       main = "Distancia corta", col = "springgreen4")
qqline(dist_corto)

shapiro.test(dist_corto)

shapiro.test(dist_largo)


ggplot(data = redes) +
  geom_boxplot(aes(x = distancia, y = tiempo, colour = distancia)) +
  theme_bw() + theme(legend.position = "none") +
  theme (text = element_text(size=16))

install.packages("car")
library(car)

bartlett.test(tiempo ~ distancia, data = redes)

t.test(tiempo ~ distancia, data = redes, var.equal = FALSE)

library(effsize)
cohen.d(formula = tiempo ~ distancia, data = redes, paired = FALSE)

# Desviaciones estándar de los grupos
sd_grupo1 <- sd(dist_largo)
sd_grupo2 <- sd(dist_corto)
# Tamaños de muestra
n1 <- length(dist_largo)
n2 <- length(dist_corto)
# Calcular la desviación estándar combinada
sd_combinada <- sqrt(((n1 - 1) * sd_grupo1^2 + (n2 - 1) * sd_grupo2^2) / (n1 + n2 - 2))
#
sd_combinada


delta_observado <- mean(dist_largo) - mean(dist_corto)
delta_observado

length(dist_largo)
length(dist_corto)

power.t.test(n = 83, delta = delta_observado, sd = sd_comun, sig.level = 0.05, type = "two.sample")

t.test(
  x = dist_largo,
  y = dist_corto,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)