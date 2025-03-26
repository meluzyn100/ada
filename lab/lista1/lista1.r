library(vcdExtra)
library(tidyverse)
library(ggplot2)

Detergent
Detergent.df <- data.frame(Detergent)
view(Detergent.df)
glimpse(Detergent.df)
str(Detergent)
names(Detergent.df)
levels(Detergent.df$Water_softness)
summary(Detergent.df)

apply(Detergent, "Temperature", sum)
#####Tempera
Detergent.df %>% group_by(Temperature) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Soft") %>% group_by(Temperature) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Medium") %>% group_by(Temperature) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Hard") %>% group_by(Temperature) %>% summarise(n = sum(Freq))

######Preference
Detergent.df %>% group_by(Preference) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Soft") %>% group_by(Preference) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Medium") %>% group_by(Preference) %>% summarise(n = sum(Freq))

Detergent.df %>% filter(Water_softness == "Hard") %>% group_by(Preference) %>% summarise(n = sum(Freq))

###############################
#zad2
ftable(Detergent, col.vars = "Temperature", row.vars = "Water_softness")

structable(Temperature ~ Water_softness, Detergent) %>% addmargins()



###############################
#zad3
A <- apply(Detergent, "Water_softness", sum)
barplot(A)
pie(A)

#zmienna 1 inne

#zmienna 2 preferense
mosaicplot(~Water_softness+Preference, data = Detergent)
mosaicplot(~M_User+Preference, data = Detergent)
mosaicplot(~Temperature+Preference, data = Detergent)

