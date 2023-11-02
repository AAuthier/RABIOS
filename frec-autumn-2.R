library(tidyverse)

##### 4.g. Emergencia otoño primera siembra - tabla de frecuencias #####

emergefrec <- read_csv("frec-emerg-2023-autumn-2 - densidad.csv")

emergefrec$`Mat+AC0-Treat`<-as.factor(emergefrec$`Mat+AC0-Treat`)
emergefrec$Ecotype <- as.factor(emergefrec$Ecotype)
str(emergefrec)


# Gráfico de densidad

# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Plot


ggplot(emergefrec, aes(x=Day, fill=`Mat+AC0-Treat`, weight=Day))+
  geom_density(alpha=0.5, adjust = 2)+
  theme_minimal()+
  labs(x="Tiempo (días)", y="N de plantas emergidas", fill = "Tratamientos Maternos")+
  xlim(0,20)+
  facet_wrap(Ecotype~.)+
  theme(legend.position = c(0.8, 0.2))

ggsave("density2.png", width = 7, height = 5)


