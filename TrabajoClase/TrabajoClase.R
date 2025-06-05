IDE= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(IDE)

IDE$Experiencia <- factor (IDE$Experiencia,
        levels = c(0.5, 1, 2),
        labels = c("Nov", "Int", "Ava"))

  res.aov_inter <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
  summary(res.aov_inter)

summary(IDE)

TukeyHSD(res.aov_inter, which = "Experiencia")

library(tidyverse)

IDE %>%
  ggplot() +
  aes(x = Herramienta, color = Experiencia, group = Experiencia, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

install.packages("emmeans")
library(emmeans)

means_aov <- emmeans(res.aov_inter, ~ Herramienta * Experiencia)
print(means_aov)

contrast(means_aov, method = list("VSCode_Nov vs IntelliJ_Ava" = c(0, 1, 0, 0, -1, 0)))

contrast(means_aov,
         method = list("Interacción Novato - Intermedio" = c(1, -1, -1, 1, 0, 0 )))

contrast(means_aov,
         method = list("Interacción Intermedio - Avanzado" = c(0, 0, 1, -1, -1, 1)))

contrast(means_aov,
         method = list("Interacción Novato - Avanzado" = c(1, -1, 0, 0, -1, 1)))

model <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
means <- emmeans(model, ~ Herramienta * Experiencia)

interaction_contrasts <- list(
  "Interacción Nov vs Int" = c(1, -1, -1, 1, 0, 0), # Efecto en Nov vs Int
  "Interacción Nov vs Ava" = c(1, -1, 0, 0, -1, 1), # Efecto en Nov vs Ava
  "Interacción Int vs Ava" = c(0, 0, 1, -1, -1, 1)) # Efecto en Int vs Ava

interaction_results <- contrast(means, method = interaction_contrasts,
                                adjust = "none") # En este caso inicial no se hace ajuste.
print(interaction_results)

interaction_results <- contrast(means, method = interaction_contrasts,
                                adjust = "holm")
print(interaction_results)

