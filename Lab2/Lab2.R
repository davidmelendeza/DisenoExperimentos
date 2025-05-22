df= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
attach(df)

#install libs
install.packages(c("dplyr",
                   "ggplot2",
                   "gridExtra",
                   "tidyr",
                   "reshape2",
                   "RColorBrewer",
                   "ggrepel"))

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)

df = tibble::as_tibble(df)
colnames(df)[25] <- "classification"
df$capture_rate <- as.numeric(df$capture_rate)
head(df)

df = select(df, name, classification, hp, weight_kg,
            height_m, speed, attack, defense,
            sp_attack, sp_defense, type1, type2,
            abilities, generation, is_legendary,
            capture_rate)
head(df)


# Resumen informativo de los datos - tendencias
summary(df)

# A1 Informaciónbásica
str(df)

summary(df$attack)

summary(df$defense)

summary(df$hp)

summary(df$weight_kg)

summary(df$height_m)

# A2 Histogramas

hist(df$attack)
hist(df$hp, main="Distribución de variable attack", xlab="ATTACK", col="lightblue", border="black")

hist(df$defense)
hist(df$defense, main="Distribución de variable defense", xlab="DEFENSE", col="lightblue", border="black")

hist(df$weight_kg)
hist(df$weight_kg, main="Distribución de variable weight", xlab="WEIGHT", col="lightblue", border="black")

hist(df$height_m)
hist(df$height_m, main="Distribución de variable height", xlab="HEIGHT", col="lightblue", border="black")

# A4 Gráficos de dispersión
plot(df$attack, df$hp)
plot(df$attack, df$hp, main="Ataque vs Vida", xlab="Ataque", ylab="Vida", col="darkred")

plot(df$defense, df$hp)
plot(df$defense, df$hp, main="Defensa vs Vida", xlab="Defensa", ylab="Vida", col="darkred")

# B4 Boxplots
box_plot_attr <- select(df, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
box_plot_attr_leg <- filter(box_plot_attr, is_legendary == 1)
box_plot_attr_nor <- filter(box_plot_attr, is_legendary == 0)
box_plot_attr_leg_long <- gather(box_plot_attr_leg, attribute, value, -c(type1, is_legendary))
box_plot_attr_nor_long <- gather(box_plot_attr_nor, attribute, value, -c(type1, is_legendary))
bp_leg <- ggplot(data = box_plot_attr_leg_long, aes(attribute, value)) + geom_boxplot(fill="green4")+ ggtitle("Pokemon Legendario")
bp_nor <- ggplot(data = box_plot_attr_nor_long, aes(attribute, value)) + geom_boxplot(fill ="yellow2") + ggtitle("Pokemon No Legendario")
grid.arrange(bp_leg, bp_nor,ncol=2)

#B5 Mapas de calor
hmap_attr <- select(df, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
hmap_attr_leg <- filter(hmap_attr, is_legendary == 0)
hmap_attr_leg <- group_by(hmap_attr_leg, type1)
hmap_attr_leg <- summarise(hmap_attr_leg, hp=median(hp), attack=median(attack),
                           defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense),
                           speed=median(speed))
hmap_attr_leg_m <- melt(hmap_attr_leg)
hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')
ggplot(data=hmap_attr_leg_m, aes(type1, variable)) + geom_tile(aes(fill=value)) + ggtitle("Pokémon
No legendarios: Type1 - Atributo") + scale_fill_gradientn(colours = hm.palette(100)) + theme(axis.text.x =element_text(angle=90, hjust=0)) + coord_equal()

# B6 Matriz de correlacion
df= (read.csv(file.choose(), header=T, encoding = "UTF-8"))
df = tibble::as_tibble(df)
colnames(df)[25] <- "classification"
df$capture_rate <- as.numeric(df$capture_rate)

df_fight_against <- select(df, type1, against_bug:against_water)
head(df_fight_against)

df_fight_against_g <- group_by(df_fight_against, type1)
df_fight_against_summ <- summarise(df_fight_against_g,
                                   against_bug = median(against_bug),
                                   against_dark = median(against_dark),
                                   against_dragon = median(against_dragon),
                                   against_electric = median(against_electric),
                                   against_fairy = median(against_fairy),
                                   against_fight = median(against_fight),
                                   against_fire = median(against_fire),
                                   against_flying = median(against_flying),
                                   against_ghost = median(against_ghost),
                                   against_grass = median(against_grass),
                                   against_ground = median(against_ground),
                                   against_ice = median(against_ice),
                                   against_normal = median(against_normal),
                                   against_poison = median(against_poison),
                                   against_psychic = median(against_psychic),
                                   against_rock = median(against_rock),
                                   against_steel = median(against_steel),
                                   against_water = median(against_water))

df_fight_against_long <- melt(df_fight_against_summ)
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')), space='Lab')
ggplot(data=df_fight_against_long, aes(type1, variable)) + geom_tile(aes(fill=value)) +
  scale_fill_gradientn(colours = hm.palette(100)) + coord_equal() +
  theme(axis.text.x=element_text(angle=90, hjust=0)) + ggtitle("Efectividad por tipo de Pokemon")
