library(tidyverse) 

IDE= (read.csv(file.choose(), header=T, encoding = "UTF-8")) 
attach(IDE) 

str(IDE) 

IDE$Experiencia <- factor (IDE$Experiencia, 
                           levels = c(0.5, 1, 2),         
                           labels = c("Nov", " Int", " Ava")) 

str(IDE) 

table(IDE$Herramienta, IDE$Experiencia) 

