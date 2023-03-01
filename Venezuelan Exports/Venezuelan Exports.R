#Libraries
library(showtext)
library(tidymodels)
library(tidyverse)
library(readr)
library(readxl)
library(scales)
library(lubridate)
library(timeDate)
library(Metrics)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(glue)
library(ggrepel)
library(treemap)
library(treemapify)

# This is to avoid scientific notation
options(scipen = 999)

# Setting letter font to use in the charts

font.families()

font_paths()

font_files() %>% tibble() %>% filter(str_detect(family, "Montserrat"))

font_add(family = "Montserrat", regular = "Montserrat-Regular.otf" )

showtext_auto()


# Set working directory

setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica")
getwd()


# Importing files

# Getting the files names

files <- list.files(path = "Venezuelan Exports/",pattern = ".csv")
files

# Concatenating the working directory extension

files <- str_c("Venezuelan Exports/", files)

# Adding names to each element in the vector

names(files) <- 2013:2020
files
# Read all the excels

exports <- map_df(.x = files, .f = read_csv, .id = "year")

# lower case all columns names just to peace mi mind.

names(exports) <- tolower(names(exports))

# Minor transformations

exports <- exports %>%
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 2016) %>%
  rename("exports" = `gross export`)

View(exports)

# Translating because this a venezuelan focused image
exports <- exports %>% 
  mutate(., name = with(., case_when(
  (name == "Acyclic alcohols") ~ "Alcholes Acíclicos",
  (name == "Ferrous products from the reduction of iron ore") ~ "Productos Ferrosos",
  (name == "Iron ores and concentrates") ~ "Mineral de Hierro",
  (name == "Crustaceans") ~ "Crustáceos",
  (name == "Gold") ~ "Oro",
  (name == "Nitrogenous fertilizers") ~ "Fertilizantes",
  (name == "Ammonia") ~ "Amoníaco",
  (name == "Carbides") ~ "Carburo",
  (name == "Unwrought aluminum") ~ "Aluminio Bruto",
  (name == "Copper waste and scrap") ~ "Desechos de Cobre",
  (name == "Ferrous waste and scrap") ~ "Desechos Ferrosos",
  (name == "Aluminum wire") ~ "Alambre",
  (name == "Spirits < 80% alcohol") ~ "Licores < 80% de Alcohol",
  (name == "Other oild seeds"  ) ~ "Otros Aceites de Vegetal",
  (name == "Fuel wood"  ) ~ "Leña",
  (name == "Cocoa beans"  ) ~ "Cacao",
  (name == "Fish, excluding fillets"  ) ~ "Pescado, no Filetes",
  (name == "Cocoa beans"  ) ~ "Cacao",
  (name == "Prepared aquatic invertibrates"  ) ~ "Invertebrados Acuaticos Preparados",
  (name == "Frozen fish, excluding fillets"  ) ~ "Pescado Congelado, no filetes",
  (name == "Tanned hides of bovines or equines"  ) ~ "Cuero Curtido de Bovinos o Equinos",
  (name == "Plastic waste"  ) ~ "Desechos de Plástico",
  (name == "Coal"  ) ~ "Carbón",
  (name == "Transport"  ) ~ "Transporte",
  (name == "Travel and tourism"  ) ~ "Viaje y Turismo",
  (name == "ICT"  ) ~ "TIC",
  (name == "Petroleum oils, crude") ~ "Petróleo Crudo",
  (name == "Petroleum oils, refined") ~ "Petróleo Refinado",
  (name == "Petroleum coke") ~ "Coque de Petróleo",
  (name == "Other oil seeds") ~ "Otros Aceites Vegetales",
  (name == "Insulated electrical wire") ~ "Cable Eléctrico Aislado",
  (name == "Electrical energy") ~ "Enegía Eléctrica",
  (name == "Trade data discrepancies") ~ "Discrepancia en los datos",
  (name == "Commodities not specified according to kind") ~ "Mercancias no especificada",
  .default = name)
  ))




exports  <- exports %>% 
  mutate(., sector = with(., case_when(
  (sector == "Stone"  ) ~ "Piedras",
  (sector == "Metals"  ) ~ "Metales",
  (sector == "Agriculture"  ) ~ "Agricultura",
  (sector == "Chemicals"  ) ~ "Químicos",
  (sector == "Minerals"  ) ~ "Minerales",
  (sector == "Services"  ) ~ "Servicios",
  (sector == "Electronics"  )  ~ "Electrónica",
  (sector == "Other") ~ "Otros",
  (sector == "Vehicles") ~ "Vehículos",
  (sector == "Machinery") ~ "Maquinaria",
  .default = sector)
  ))




# minor transformation
exports2 <-  exports %>%
  filter(year >= 2016) %>% 
  group_by(name, sector) %>% 
  summarise( exports = sum(exports)) %>% 
  ungroup() %>% 
  mutate( oil_detector = str_detect(name, 'Petróleo')) %>% 
  filter( oil_detector == F  ) %>%
  mutate( oil_detector2 =    str_detect(name, 'petróleo')) %>% 
  filter( oil_detector2 == T  ) %>% 
 
  
  
  


h <- exports2 %>% 
  summarise(sum(exports)) %>% 
  pull()

# I want to have a columns with the % share of the exports for each item

exports3 <- exports2 %>% 
  mutate(share =  paste(  as.character(  round(exports/h*100, 0)  ) , "%",sep = "")  ) %>%  
  rename("Sector Económico"  = sector) 

# I combine the share of exports with the name o the item

lab <- exports3 %>% 
  glue_data("{name} \n{share}")

venezuelan_exports <- exports3 %>% 
  #ggplot
  ggplot(aes(area = exports, fill = `Sector Económico`, subgroup = `Sector Económico` ,
             label = lab))+
  #geoms
  geom_treemap(col = "#980000ff")+
  #geom_treemap_subgroup_text(col = "black", grow = T )+
  geom_treemap_subgroup_border(col = "#980000ff", size = 3)+
  geom_treemap_text( grow = T, col = "white" )+
  #themes

  theme(
    legend.title = element_markdown(face = "bold", size = 16, colour = "#980000ff"),
    legend.position = "bottom",
    legend.text = element_markdown( size = 16, colour = "#980000ff"),
    legend.background =  element_rect(fill = "lightgrey") ,
    legend.key = element_rect(colour = "#980000ff"),
    plot.title = element_textbox_simple(face = "bold", size = "40", colour = "#980000ff", 
                                        margin = margin(12,0,10,0), family = "Montserrat",
                                        lineheight = 0.1),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple( size = "30", colour = "#980000ff",
                                            margin = margin(2,0,6,0),family = "Montserrat",
                                            lineheight = 0.2),
    plot.caption = element_textbox_simple( size = "16", colour = "#980000ff", family = "Montserrat"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "lightgrey" )
  )+
  #labs
  labs(title = "Aparte del Petróleo ¿Qué Más Exporta Venezuela?",
       subtitle = "Exportaciones No Petroleas de Venezuela Entre 2016-2020",
       caption = "Fuente: Harvard Atlas of Economic Complexity, calculos propios.")



venezuelan_exports


ggsave(venezuelan_exports, filename = "venezuelan_exports.jpg",
        bg = "lightgrey"  ,units = "cm"  , width = 10, height = 10 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")
