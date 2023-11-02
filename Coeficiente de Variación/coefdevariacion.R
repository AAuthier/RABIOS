# https://statisticsbyjim.com/basics/coefficient-variation/

library(tidyverse)
library(plyr)
library(hrbrthemes)

data<- read.csv('F2-germ.csv')

#Le cambio el nombre a la columna 'mat.', ahora se llama 'Mat'
names(data)[names(data) == "mat."] <- "Mat"

str(data)

data$germ.1 <- as.integer(data$germ.1)
data$AF <- as.factor(data$AF)  #cambia estructura
data$BF <- as.factor(data$BF)
data$IT <- as.factor(data$IT)

data$GT = factor(data$GT, c("col ", "rdr2", "rdr6", "dcl234", "ago4", "nrpd2a", "rdd"))
#ordena los niveles del factor elegido

data$Mat = factor(data$Mat, c("18-18", "24-18", "18-24", "24-24"))

data$prop <- data$germ.18 / data$n #proporcion de germ

data$speed <- data$germ.1/1 +         #velocidad de germinacion, proxy para vigor
  (data$germ.2 - data$germ.1)/2 +
  (data$germ.3 - data$germ.2)/3 +
  (data$germ.4 - data$germ.3)/4 +
  (data$germ.5 - data$germ.4)/5 +
  (data$germ.6 - data$germ.5)/6 +
  (data$germ.7 - data$germ.6)/7 +
  (data$germ.8 - data$germ.7)/8 +
  (data$germ.9 - data$germ.8)/9 +
  (data$germ.10 - data$germ.9)/10 +
  (data$germ.11 - data$germ.10)/11 +
  (data$germ.12 - data$germ.11)/12 +
  (data$germ.13 - data$germ.12)/13 +
  (data$germ.14 - data$germ.13)/14 +
  (data$germ.15 - data$germ.14)/15 +
  (data$germ.16 - data$germ.15)/16 +
  (data$germ.17 - data$germ.16)/17 +
  (data$germ.18 - data$germ.17)/18

#Grafico de histograma de velocidad

hist(data$speed)  #Distribucion bimodal tipica de germ

#ddply es una funcion para calcular media, SE..está en el paquete plyr
dm <- ddply(data, c("GT", "BF", "AF", "IT", "Mat"), summarize,
           pm = mean(prop, na.rm=T),
           se = sd(prop, na.rm=T),
           sm = mean(speed, na.rm=T),
           sse = sd(speed, na.rm=T)
)

# coeficiente de variación

coefvar <- ddply(dm, c("GT", "BF", "AF", "IT", "Mat"), summarize,
                 cvprop = se/pm,
                 cvspeed = sse/sm)

png("coef de var - prop vs speed.png")
g1 <- coefvar %>%
  ggplot(aes(x = cvprop,y = cvspeed, shape = GT, color = Mat)) +
  geom_point(size = 4) +
  theme_ipsum()
print(g1)
dev.off()
