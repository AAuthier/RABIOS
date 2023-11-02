##### 4.g. Emergencia otoño primera siembra - tabla de frecuencias #####

emergefrec <- read_csv("frec-emerg-2023-autumn-2 - densidad.csv")

emergefrec$`Mat+AC0-Treat`<-as.factor(emergefrec$`Mat+AC0-Treat`)
emergefrec$Ecotype <- as.factor(emergefrec$Ecotype)
str(emergefrec)


# Gráfico de densidad

# library
library(tidyverse)
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

#### dataframe solo con los 3 ecotipos elegidos por igual longitud distinta latitud.

### Tu Ler Oy

library(dplyr)

emfre_tulerhoy <- emergefrec %>%
  filter(Ecotype %in% c("Tu", "Ler", "Oy")) %>%
  select(Ecotype, everything())
print(emfre_tulerhoy)

ggplot(emfre_tulerhoy, aes(x=Day, fill=`Mat+AC0-Treat`, weight=Day))+
  geom_density(alpha=0.5, adjust = 2)+
  theme_minimal()+
  labs(x="Tiempo (días)", y="N de plantas emergidas", fill = "Tratamientos Maternos")+
  xlim(0,55)+
  facet_grid(Ecotype~.)#+
  # theme(legend.position = c(0.8, 0.2))

library(dplyr)
library(ggplot2)

# Cambia el orden de las etiquetas en la columna Ecotype
emfre_tulerhoy <- emfre_tulerhoy %>%
  mutate(Ecotype = factor(Ecotype, levels = c("Oy", "Ler", "Tu")))

# Trama el gráfico
ggplot(emfre_tulerhoy, aes(x = Day, fill = `Mat+AC0-Treat`, weight = Day)) +
  geom_density(alpha = 0.5, adjust = 2) +
  theme_minimal() +
  labs(x = "Tiempo (días)", y = "N de plantas emergidas", fill = "Tratamientos Maternos") +
  xlim(0, 55) +
  facet_grid(Ecotype ~ .)
