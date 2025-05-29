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

#3
group_by(IDE, Herramienta, Experiencia) %>%
  summarise(
    count = n(),
    mean = mean(Duracion, na.rm = TRUE),
    var = var(Duracion, na.rm = TRUE),
    sd = sd(Duracion, na.rm = TRUE)
  )

boxplot(Duracion ~ Herramienta, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab=" Duracion")

#4 y 5 tiene que hacerlas xd

# 6
boxplot(Duracion ~ Herramienta * Experiencia, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Duracion")
        
boxplot(Duracion ~ Experiencia * Herramienta, data=IDE, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Duracion")
        
# 7

IDE %>%
  ggplot() +
  aes(x = Herramienta, color = Experiencia, group = Experiencia, y = Duracion) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

#8 hay que hacerla

#9
interaction.plot(x.factor = IDE$Experiencia, trace.factor = IDE$Herramienta,
                 response = IDE$Duracion, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Experiencia", ylab="Duración en horas",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

# 10
res.aov <- aov(Duracion ~ Herramienta + Experiencia, data = IDE)
summary(res.aov)

#11
res.aov_inter <- aov(Duracion ~ Herramienta * Experiencia, data = IDE)
summary(res.aov_inter)

res.aov_inter2 <- aov(Duracion ~ Herramienta + Experiencia + Herramienta:Experiencia, data = IDE)
summary(res.aov_inter2)