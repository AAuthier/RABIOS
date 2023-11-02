library(tidyverse)
library(plyr)
library(viridis)
library(viridisLite)
library(car)
library(carData)
library(patchwork)

##### 4. Ensayo de emergencia - semillas frescas campo INTA 2023 ######

##### 4.a. Carga de datos emergencia - primera siembra 2023 #####

emergencia <- read_csv("Emergencia-2023-1-autumn - pretormenta.csv")

emergencia <- emergencia %>%
  mutate(SubjID = row_number()) %>% 
  select(SubjID, everything())

str(emergencia)

# Transformar los datos para tener los valores de germinación en formato tidy
germinacion_transformada <- emergencia %>% 
  gather(dia, germ, D1:D19) %>% 
  mutate(dia = as.numeric(str_replace(dia, "D", "")))

# Mostrar los datos transformados
print(germinacion_transformada)
head(germinacion_transformada)

# Renombrar DataFrame y columnas

# Nuevos nombres para el DataFrame y las columnas
nuevo_nombre_df <- "germ_otono_2023_1"
nuevos_nombres_columnas <- c("ID", "Ecotipo", "TRAT", "Bloque_Rep"
                             , "Siembra", "N_seeds", "prop", "dia", "germ")

# Cambiar los nombres del DataFrame y las columnas
germ_otono_2023_1 <- germinacion_transformada %>% 
  rename_with(~ if_else(. %in% names(germinacion_transformada), nuevos_nombres_columnas[match(., names(germinacion_transformada))], .))

# Verificar los cambios en el nuevo DataFrame
print(germ_otono_2023_1)



# trabajo sobre el data frame indicando la clase de cada variable
germ_otono_2023_1$Ecotipo <- as.factor(germ_otono_2023_1$Ecotipo)
germ_otono_2023_1$Ecotipo <- factor(germ_otono_2023_1$Ecotipo
                                    , levels = c("Wa-1","Ra-0"
                                                 ,"Sha","Ler-0"
                                                 ,"Oy-0","Cvi-0"
                                                 ,"Tu-0","Ws-0"
                                                 ,"Cen-0","Ömö2-1"))
germ_otono_2023_1$TRAT <- as.factor(germ_otono_2023_1$TRAT)

str(germ_otono_2023_1)




########################RADAR PLOTS########################################################

emergencia1 <- ddply(germ_otono_2023_1, c("Ecotipo", "TRAT"), summarize,
              pm = mean(prop, na.rm=T),
              se = sd(prop, na.rm=T))

cvemergencia1 <- ddply(emergencia1, c("Ecotipo", "TRAT"), summarize,
               cvprop = se/pm)


#reshaping tablas de datos AMB TEMP
# 
# cvATp18<- read.csv('cvATprop.csv')
# cvATp18$GT<- as.factor(cvATp18$GT)
# cvATp18$IT<- as.factor(cvATp18$IT)
# cvATp18$Mat<- as.factor(cvATp18$Mat)
# cvATp18<- cvATp18[cvATp18$IT=="18",]
# cvATp18<- cbind(cvATp18[,1], cvATp18[,3:4])
# cvATp18<- droplevels(cvATp18)
# colnames(cvATp18)<- c("GT", "Mat", "CV")
# 
# cvATp24<-  read.csv('cvATprop.csv')
# cvATp24$GT<- as.factor(cvATp24$GT)
# cvATp24$IT<- as.factor(cvATp24$IT)
# cvATp24$Mat<- as.factor(cvATp24$Mat)
# cvATp24<-  cvATp24[cvATp24$IT=="24",]
# cvATp24<- cbind(cvATp24[,1], cvATp24[,3:4])
# cvATp24<- droplevels(cvATp24)
# colnames(cvATp24)<- c("GT", "Mat", "CV")
# 

library(reshape)

wcvATp18<- reshape(cvemergencia1, timevar= "Ecotipo", idvar= "TRAT", direction= "wide")
colnames(wcvATp18)<- c("TRAT","Wa","Ra", "Sha", "Ler", "Oy", "Cvi", "Tu", "Ws", "Cen", "Omo")
row.names(wcvATp18)<- wcvATp18$Mat
wcvATp18<- wcvATp18[,1:11]
wcvATp18[1]<- NULL
#wcvATp18 <- rbind(rep(2.5,8) , rep(0,8) , wcvATp18)

# wcvATp24<- reshape(cvATp24, timevar= "GT", idvar= "Mat", direction= "wide")
# colnames(wcvATp24)<- c("Mat","Col", "rdr2", "rdr6", "dcl234", "ago4", "nrpd2a", "rdd", "Ler")
# wcvATp24[3,5]<-"0"
# wcvATp24$dcl234 <- as.numeric(wcvATp24$dcl234)
# wcvATp24<- wcvATp24[,1:8]


#ESTE NO USE
library(fmsb)
library(ggplot2)
library(colorspace)
library(viridis)
library(viridisLite)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7", "#001422", "#008045")

radarchart(wcvATp18,
          
           cglcol="grey",
           pcol=cbPalette,
           )
legend("topright", bty = "n", pch=20 , col=cbPalette , text.col = "grey25", cex=1.2, pt.cex=3)

