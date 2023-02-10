library(readxl)
library(tidyverse)
library(stargazer)
library(ggthemes)
library(stringi)
library(stringr)
library(lubridate)
library(forcats)
library(scales)
library(ggplot2)
library(ggtext)
library(showtext)
library(glue)
library(ggrepel)
library(rio)
library(purrr)
library(showtext)
library(sf)
library(rnaturalearth)
library(countrycode)




# Estableciendo el working directory

getwd()
setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica")


# Anos de estudio

Anos = 2012:2021

# Buscar la fuente de letra que vamos a usar en la grafica

font_files() %>% tibble() %>% filter(str_detect(family, "Arial Rounded MT Bold"))

font_add(family = "ArialRounded", regular = "ARLRDBD.TTF" )


showtext_auto()


# Cargar data de salarios


EU_Salary <- read_excel("EuropeanSalary/Data/EU_Salary.xlsx", 
                        sheet = "Sheet 1", skip = 9, n_max = 45)



# Limpiado data
CEU_Salary <- EU_Salary %>%
  
    select("TIME...1", "TIME...2","2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
           "2020", "2021") %>% 
    filter(!is.na(`2012`)) %>% 
    rename("ISO2" = "TIME...1") %>% 
    rename("Country" = "TIME...2") %>% 
    pivot_longer(cols = 3:12,
                 names_to = "Year",
                 values_to = "Median_Salary") %>% 
  filter(Year == 2018)

# Confirmo nombre de las columnas  
colnames(CEU_Salary)


# Cargo la data del mundo


world <- ne_countries( scale = "small", returnclass = "sf")



# Creo visualizacion del mundo

world %>% 
  st_transform(crs = "+proj=robin") %>% 
  ggplot()+
  geom_sf()+
  coord_sf(datum = NA)+
  theme_minimal()

# Creo el bloque de data que voy a usar para la visualizacion



# Data para dibujar el mundo 

map <- map_data("world")
View(map)


# Combinar tablas del mundo con nuestra data de latinoamerica  

Europe_Salary <- map %>% 
  mutate(
    region = ifelse(region == "UK", "United Kingdom", region)) %>% 
  left_join(., CEU_Salary , by = c("region" = "Country")) %>% 
  filter(!is.na(Year)) %>% 
  mutate(
    Median_Salary =  as.numeric(Median_Salary),
    Median_Salary = Median_Salary/12)


unique(CEU_Salary$Country)
unique(Europe_Salary$region)
unique(map$region)


# Defino un punto medio para scale_fill_gradient2

mid <- median(Europe_Salary$Median_Salary)


# Creamos el chart


C_Europe_Salary <- 
  
  # Canvas
  
  ggplot(Europe_Salary, aes(x = long, y = lat, group = group)) +
  
  #Geoms
  
  geom_polygon(aes(fill = Median_Salary), color = "black")+
  
  # Scale
  
  scale_fill_gradient2(name = "Salario Medio<br> en Euros",
                       low = "#980000ff", high = muted("#008C28"),
                       space = "Lab", midpoint = mid)+
  coord_sf(datum = NA)+
  
  # Themes
  
  theme_minimal()+
  theme(#legend.position = "none",
    legend.text = element_text(colour = "#980000ff", size = 12),
    legend.title = element_markdown( family = "ArialRounded", colour = "#980000ff",lineheight = 0.2,
                                     size = 14),
    axis.title = element_text( colour = "#980000ff"),
    axis.text = element_textbox_simple(family = "ArialRounded", colour = "#980000ff"),
    axis.ticks = element_line( colour = "#980000ff"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(family = "ArialRounded", size = "40", colour = "#980000ff",
                                        vjust = 0, hjust = 0,  margin = margin(0,0,2,0), 
                                        lineheight = 0.2),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple( family = "ArialRounded", size = "20", colour = "#980000ff",
                                            lineheight = 0.2),
    plot.caption = element_textbox_simple(family = "ArialRounded", size = "14", colour = "#980000ff", hjust = 0),
    plot.caption.position = "plot")+
  
  # Labs
  
  labs(title = "Una Europa Desigual",
       subtitle = "Salario Medio Mensual en Europa para 2018",
       caption = "Fuente: Eurostat",
       x =element_blank(), y = element_blank())


ggsave(C_Europe_Salary, filename = "Europe_Salary.jpg",
       bg ="white", units = "cm"  , width = 10, height = 7 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")


Europe_Salary %>%
  select(region, Median_Salary) %>% 
  group_by(region)

# Ahora estudio el indice de precios

# Cargo la data

EU_Prices <- read_excel("European Salary/Data/EU_Prices.xls",
                        skip = 9, n_max = 35)



# Limpieza de los datos

CEU_Prices <- EU_Prices %>% 
  mutate(
    `2020` = ifelse(`2020` == ":",
                    yes = NA,
                    no = `2020`),
    `2021` = ifelse(`2020` == ":",
                    yes = NA,
                    no = `2021`),
    `2020` = as.numeric(`2020`),
    `2021` =  as.numeric(`2021`)) %>% 
  pivot_longer(cols = 2:11,
               names_to = "Year",
               values_to = "Price_Index") %>% 
  filter(Year == 2018) %>% 
  rename("Country" = `GEO/TIME`)


# Junto la tabla de los indices de precios con la tabla de los salarios medios

EU_Living <- CEU_Prices %>% 
  
  # Cruce de las tablas
  
  left_join(.,CEU_Salary, by = "Country") %>% 
  select(Country, Price_Index, Median_Salary, ISO2) %>% 
  
  # Modificaciones
  
  mutate( 
    ISO3  =  countrycode(Country, origin = "country.name", destination = "iso3c"),
    Median_Salary = as.numeric(Median_Salary)) %>%
  
  # Depuracion
  
  select(-ISO2) %>% 
  select(Country, ISO3, Price_Index, Median_Salary)
  

# Gradica Median Salary vs Price Index

CEU_Living <- EU_Living %>% 
  filter(ISO3 != "TUR") %>% 
  
  # Canvas
  
  ggplot(aes(y = Price_Index, x = Median_Salary/12*1.18 ))+
  #geom_point()+
  
  # Geoms
  
  geom_smooth(method='lm', formula= y~x, se = F)+
  geom_text_repel(aes(size = 8, label = Country)) +
  
  # Theme
  
  theme(
    legend.position = "none",
    axis.title = element_text( colour = "#980000ff", size = 14),
    axis.text = element_textbox_simple(family = "ArialRounded", colour = "#980000ff", size = 8),
    axis.ticks = element_line( colour = "#980000ff"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.border = element_rect( colour = "#980000ff",
                                 fill = NA),
    panel.background = element_rect( fill = "transparent"),
    plot.background = element_rect( fill = "transparent", color = NA),
    
    
    panel.grid.major.x = element_line(color = "#980000ff", size = 0.5, linetype = 2 ),
    plot.title = element_textbox_simple(family = "ArialRounded", size = "30", colour = "#980000ff",
                                        vjust = 0, hjust = 0,  margin = margin(0,0,2,0), 
                                        lineheight = 0.2),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(family = "ArialRounded", size = "20", colour = "#980000ff",
                                           vjust = 0, hjust = 0,  margin = margin(0,0,2,0), 
                                           lineheight = 0.2) ,
    plot.caption = element_textbox_simple(family = "ArialRounded", size = "12", colour = "#980000ff", hjust = 0),
    plot.caption.position = "plot")+
  
  
  
  # Labs
  
  labs( title = "Salario vs Costo de la Vida",
        subtitle = "Comparación de Indices de precios contra Salario Medio en Europa",
        y = "Indice de Precios", x = "Mediana Salarial Mensual",
        caption = "Nota: Salarios en dolares<br> 
        Fuente: Eurostat reporte 2018, calculos propios.")


ggsave(CEU_Living, filename = "CEU_Living.jpg",
       bg ="white", units = "cm"  , width = 8, height = 8 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")



?geom_smooth



  theme(
  axis.title = element_markdown( colour = "#980000ff"),
  axis.text = element_text( colour = "#980000ff"),
  axis.ticks = element_line( colour = "#980000ff"),
  axis.line = element_blank(),
  panel.border = element_rect( colour = "#980000ff",
                               fill = NA),
  panel.background = element_rect( fill = "transparent"),
  plot.background = element_rect( fill = "transparent", color = NA),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(color = "#980000ff", size = 0.5, linetype = 2 ),
  #panel.grid.minor = element_blank(),
  plot.title = element_textbox_simple(face = "bold", size = "14", colour = "#980000ff",
                                      margin = margin(0,0,1.5,0)),
  plot.title.position = "plot",
  plot.subtitle = element_textbox_simple( size = "10", colour = "#980000ff",
                                          margin = margin(0,0,5,0)),
  plot.caption = element_markdown( size = "6", colour = "#980000ff")
)



284+66+5


61+3+6


223+63+50

23+18