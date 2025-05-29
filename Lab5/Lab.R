library(tidyverse) 

IDE= (read.csv(file.choose(), header=T, encoding = "UTF-8")) 
attach(IDE) 

str(IDE) 

IDE$Experiencia <- factor (IDE$Experiencia, 
                           levels = c(0.5, 1, 2),         
                           labels = c("Nov", " Int", " Ava")) 

str(IDE) 

#1
table(IDE$Herramienta, IDE$Experiencia) 

#2
group_by(IDE, Experiencia) %>%
  summarise(
    count = n(),
    mean = mean(Duracion, na.rm = TRUE),
    var = var(Duracion, na.rm = TRUE),
    sd = sd(Duracion, na.rm = TRUE)
  )

group_by(IDE, Herramienta) %>%
  summarise(
    count = n(),
    mean = mean(Duracion, na.rm = TRUE),
    var = var(Duracion, na.rm = TRUE),
    sd = sd(Duracion, na.rm = TRUE)
  )


#3
group_by(IDE, Herramienta, Experiencia) %>%
  summarise(
    count = n(),
    mean = mean(Duracion, na.rm = TRUE),
    var = var(Duracion, na.rm = TRUE),
    sd = sd(Duracion, na.rm = TRUE)
  )

boxplot(Duracion ~ Herramienta, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab=" Duracion")

#4
boxplot(Duracion ~ Experiencia, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab=" Duracion")


# 6
boxplot(Duracion ~ Herramienta * Experiencia, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Duracion")
        
boxplot(Duracion ~ Experiencia * Herramienta, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Duracion")
        
# 7 y 8

IDE %>%
  ggplot() +
  aes(x = Herramienta, color = Experiencia, group = Experiencia, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

IDE %>%
  ggplot() +
  aes(x = Experiencia, color = Herramienta, group = Herramienta, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

#9
interaction.plot(x.factor = IDE$Experiencia, trace.factor = IDE$Herramienta,
                 response = IDE$Duracion, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Experiencia", ylab="Duración en horas",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

interaction.plot(x.factor = IDE$Herramienta, trace.factor = IDE$Experiencia,
                 response = IDE$Duracion, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Herramienta", ylab="Duración en horas",
                 pch=c(1,19, 17),  col = c("#00AFBB", "#E7B800", "#FC4E07"))


# 10
res.aov <- aov(Duracion ~ Herramienta + Experiencia, data = IDE)
summary(res.aov)

#11
res.aov_inter <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
summary(res.aov_inter)

res.aov_inter2 <- aov(Duracion ~ Herramienta + Experiencia + Herramienta:Experiencia, data = IDE)
summary(res.aov_inter2)

#12
TukeyHSD(res.aov_inter , which = "Experiencia")

par(mar = c(2, 6, 2, 2))
plot(TukeyHSD(res.aov, conf.level=.95, which = "Experiencia"), las = 1)

#13
TukeyHSD(res.aov_inter, which = "Herramienta")
par(mar = c(2, 6, 2, 2))  # Ajuste de márgenes
plot(TukeyHSD(res.aov_inter, conf.level = 0.95, which = "Herramienta"), las = 1)

#14
TukeyHSD(res.aov_inter)

#15
hist(res.aov_inter$residuals)

qqnorm(res.aov_inter$residuals)
qqline(res.aov_inter$residuals)

#16
shapiro.test(res.aov_inter$residuals)

#17
plot(res.aov_inter$residuals)

#18
plot(res.aov_inter, 1)

#19
inter <- interaction(IDE$Experiencia, IDE$Herramienta)
bartlett.test(Duracion ~ inter, data = IDE)

#20
library(lsr)
etaSquared(res.aov_inter, anova = TRUE)

#21
#install.packages("effectsize")
library(effectsize)
effectsize::eta_squared(res.aov_inter)

#install.packages("pwr2")
library(pwr2)
efectoHerr = 0.15 # se toma del eta.squared.part que usted obtuvo
efectoExp = 0.35 # se toma del eta.squared.part que usted obtuvo
fA = sqrt(efectoHerr / (1 - efectoHerr))
fB = sqrt(efectoExp / (1 - efectoExp))
print(fA)
print(fB)

effectsize::cohens_f(res.aov_inter)

pwr.2way(a=2, b=3, alpha=0.05, size.A=30, size.B=20, f.A=fA, f.B=fB)
