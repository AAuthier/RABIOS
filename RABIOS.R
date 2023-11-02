# Load required libraries
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(ggrepel)
library(readxl)
library(dplyr)
# Cargar las librerías necesarias
library(ggplot2)
library(maps)
library(dplyr)
library(sf)
library(ggspatial)

# Replace 'your_excel_file.xlsx' with the actual file path
excel_file <- read_excel("Emergencia-2023-2-autumn.xlsx")

# Rename columns for clarity
colnames(excel_file) <- c("ID", "Ecotype", "Mat-Treat", "Block", "Sown_Date", "Total_Seeds", paste0("D", 1:53), "GermFinal")

# Create an empty data frame to store the transformed data
transformed_data <- data.frame()

# Iterate through each day column (D1, D2, etc.)
for (day in 1:53) {
  # Filter rows where the number of plants germinated on the day is greater than 0
  day_subset <- excel_file %>%
    select(Ecotype, `Mat-Treat`, paste0("D", day)) %>%
    rename(Frec = paste0("D", day)) %>%
    filter(Frec > 0)
  
  # Append the subset to the transformed data frame
  transformed_data <- bind_rows(transformed_data, day_subset)
}

# Create the frequency table
frequency_table <- transformed_data %>%
  group_by(Ecotype, `Mat-Treat`, Frec) %>%
  summarise(Count = n()) %>%
  ungroup()

# Print the frequency table
print(frequency_table)

# To export the frequency table to a CSV file
# write.csv(frequency_table, "frequency_table.csv", row.names = FALSE)

# Leer el archivo CSV
datos <- read.csv("datos-origen-ecotipos.csv")

datos$Ecotipo <- as.factor(datos$Ecotipo)
datos$Ecotipo <- factor(datos$Ecotipo, levels = c("Ler-0","Oy-0","Tu-0"))
head(datos)

datos <- datos %>% 
  filter(!is.na(Ecotipo))

# Definir los límites geográficos de la región que deseas mostrar
xmin <- -10  # Longitud mínima
xmax <- 25   # Longitud máxima
ymin <- 40   # Latitud mínima
ymax <- 65   # Latitud máxima

# Filtrar los datos del mapa para mostrar solo la región definida
map_data_filtered <- map_data("world") %>%
  filter(long >= xmin, long <= xmax, lat >= ymin, lat <= ymax)

# Crear el mapa de la región definida
mi_mapa <- ggplot() +
  geom_polygon(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", fill = "white", size = 0.1) +  # Dibujar fronteras con líneas más finas
  geom_path(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1) +  # Dibujar límites de países con líneas más finas
  geom_point(data = datos, aes(x = Long.Decim, y = Lat.Decima, colour = Ecotipo), size = 2) +  # Puntos con color
#  geom_text_repel(data = datos, aes(x = Long.Decim, y = Lat.Decima, label = Ecotipo), size = 3, hjust = 0, vjust = 0) +  # Etiquetas con el nombre
  labs(x = "Longitude", y = "Latitude") +
  scale_color_viridis(discrete = TRUE, name = "Ecotypes") +  # Establecer el título de la leyenda
  theme_minimal() +
  coord_fixed(ratio = 1) +  # Relación de aspecto 1:1 para evitar la distorsión
  xlim(xmin, xmax) +  # Establecer límites geográficos en el eje x
  ylim(ymin, ymax) +  # Establecer límites geográficos en el eje y
  theme(legend.position = "right")  # Mover la leyenda a la derecha

# Imprimir el mapa
print(mi_mapa)


# Definir los límites geográficos de la región que deseas mostrar
xmin <- 0  # Longitud mínima
xmax <- 80   # Longitud máxima
ymin <- 10   # Latitud mínima
ymax <- 65   # Latitud máxima

# Filtrar los datos del mapa para mostrar solo la región definida
map_data_filtered <- map_data("world")

# Crear el mapa de la región definida
mi_mapa <- ggplot() +
  geom_sf()+
  geom_polygon(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", fill = "white", size = 0.1) +  # Dibujar fronteras con líneas más finas
  geom_path(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1) +  # Dibujar límites de países con líneas más finas
  geom_point(data = datos, aes(x = Long.Decim, y = Lat.Decima, colour = Ecotipo), size = 2) +  # Puntos con color
#  geom_text_repel(data = datos, aes(x = Long.Decim, y = Lat.Decima, label = Ecotipo), size = 5, box.padding = 0.5) +  # Etiquetas con el nombre
  labs(x = "Longitude", y = "Latitude") +
  scale_color_viridis(discrete = TRUE, name = "Ecotypes")+
  theme_bw() +
  coord_fixed(ratio = 1) +  # Relación de aspecto 1:1 para evitar la distorsión
  xlim(xmin, xmax) +  # Establecer límites geográficos en el eje x
  ylim(ymin, ymax) +  # Establecer límites geográficos en el eje y
  theme(legend.position = "right",text = element_text(size = 20)) # Mover la leyenda a la derecha

mi_mapa + 
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    aspect.ratio = 9/16)



world <- ne_countries(scale = "medium", returnclass = "sf")

mi_mapa <- ggplot(data = world) +
  geom_sf(fill = "antiquewhite")+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "grid", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(xlim = c(0, 25), ylim = c(40, 65), expand = FALSE) +
  geom_polygon(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", fill = "white", size = 0.1) +  # Dibujar fronteras con líneas más finas
  geom_path(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1) +  # Dibujar límites de países con líneas más finas
  geom_point(data = datos, aes(x = Long.Decim, y = Lat.Decima, colour = Ecotipo), size = 1.5) +  # Puntos con color
  geom_text_repel(data = datos, aes(x = Long.Decim, y = Lat.Decima, label = Ecotipo), size = 3, box.padding = 0.4) +  # Etiquetas con el nombre
  labs(x = "Longitud", y = "Latitud") +
  scale_color_viridis(discrete = TRUE, name = "Ecotipos")+
  theme_bw() +
  # coord_fixed(ratio = 1) +  # Relación de aspecto 1:1 para evitar la distorsión
  #  xlim(xmin, xmax) +  # Establecer límites geográficos en el eje x
  #  ylim(ymin, ymax) +  # Establecer límites geográficos en el eje y
  theme(legend.position = "right",text = element_text(size = 20), # Mover la leyenda a la derecha
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue", world$mapcolor7))
print(mi_mapa)



### VERSION FINAL SIN COMENTARIOS

mi_mapa <- ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  coord_sf(xlim = c(0, 25), ylim = c(40, 65), expand = FALSE) +
  geom_polygon(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", fill = "antiquewhite", size = 0.1) +
 # geom_path(data = map_data_filtered, aes(x = long, y = lat, group = group), color = "gray50", size = 0.1) +
  geom_point(data = datos, aes(x = Long.Decim, y = Lat.Decima, colour = Ecotipo), size = 3) +
  geom_text_repel(data = datos, aes(x = Long.Decim, y = Lat.Decima, label = Ecotipo), size = 5, box.padding = 0.4) +
  labs(x = "Longitude", y = "Latitude") +
  ylim(40,65)+
  scale_color_viridis(discrete = TRUE, name = "Ecotypes") +
  theme_bw() +
  theme(legend.position = "right", text = element_text(size = 20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "grid", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering())
mi_mapa


ggsave(filename = "mapaeco.png",
       plot = mi_mapa, width = 365, height = 125,
       units = 'mm',
       limitsize = FALSE,
       scale = 4, dpi = 600)


world <- ne_countries(scale = "medium", returnclass = "sf")

# Crear un mapa a escala
mi_mapa <- ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = datos, aes(x = Long.Decim, y = Lat.Decima, color = Ecotipo), size = 3) +
  geom_text_repel(data = datos, aes(x = Long.Decim, y = Lat.Decima, label = Ecotipo), size = 5, box.padding = 0.4) +
  labs(x = "Longitud", y = "Latitud") +
  scale_color_viridis(discrete = TRUE, name = "Ecotipos") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(0, 20) + ylim(40, 65) +
  annotation_scale(location = "br", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", style = north_arrow_orienteering(), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))

mi_mapa

######## COEFICIENTE DE VARIACION #######


# Primero tengo que armar las tablas



