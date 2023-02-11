#Libraries

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
library(haven)





# Set working directory

setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica")
getwd()


# Cargar las fuentes de escritura
font.families()

font_paths()

font_files() %>% tibble() %>% filter(str_detect(family, "Montserrat"))

font_add(family = "Montserrat", regular = "Montserrat-Regular.otf" )

showtext_auto()


# Cargar data

harvard_trade <- read_dta("Oil Dependency/Country_Trade.dta")

weo_gdpp <- read_excel("Oil Dependency/WEO_GDPp.xlsx")



EU <- read_excel("Oil Dependency/EU.xlsx")
View(EU)

OECD <- read_excel("Oil Dependency/OECD.xlsx", 
                   col_types = c("text", "skip"))

# Crear bloque de paises arabes

ARAB <- c("SAU","ARE", "OMN", "BRN",  "KWT")

#Limpiar data del WEO

weo_gdpp <- weo_gdpp %>% 
  select( -Units, -Scale, -`Estimates Start After`) %>% 
  pivot_longer(
    cols = 3:30,
    names_to = "year",
    values_to = "gdpp"
  ) %>% 
  mutate(
    gdpp = ifelse(gdpp == "n/a", 0, gdpp),
    gdpp = as.numeric(gdpp),
    gdpp = gdpp) %>%
  filter(year == 2020) %>% 
  select(- year)




# databreaks para los income brakets

databreaks  <- data.frame(
  start = c(0, 1.035,4.045,12.535),
  end = c(1.035,4.045, 12.535, 40.000),
  ingresos = c('Bajo', 'Medio Bajo', 'Medio Alto', 'Alto')
)

# ahora lo que hago es ordernar la columna ingreso para que salga asi en la leyenda

databreaks$ingresos <- factor(databreaks$ingresos,
                              levels = c('Alto', 'Medio Alto', 'Medio Bajo','Bajo'))

Harvard_Trade <- harvard_trade %>% 
  # Limpieza y Calculos
  mutate(oil = ifelse(hs_product_code == 27,T,F)) %>% 
  select(year, export_value, location_code, oil) %>% 
  group_by(year, location_code, oil) %>% 
  summarise(export_value = sum(export_value)) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = oil,
    values_from = export_value
  ) %>% 
  rename("oil_exports"= `TRUE`) %>%
  rename("non_oil_exports" = `FALSE`) %>% 
  mutate(
    total_exports = non_oil_exports + oil_exports,
    oil_dependency = oil_exports/total_exports,
    is_dependent = ifelse(oil_dependency >= 0.6,T,F)) %>% 
  rename("ISO" = location_code) %>% 
  mutate(yiso = paste(year, ISO, sep = "")) %>%
  filter(year == 2020) %>% 
  #unir con la otra tabla
  left_join(., weo_gdpp, by = "ISO") %>% 
  mutate(gdpp = gdpp/1000) %>% 
  rename("iso" = ISO)  %>% 
  rename("country"= Country) %>%
  mutate(
    income_level = with(., case_when(
      (gdpp < 40 & gdpp >= 12.5)~ "Alto",
      (gdpp < 12.5 & gdpp >= 4)~ "Medio Alto",
      (gdpp < 4 & gdpp >= 1)~ "Medio Bajo",
      (gdpp < 1) ~ "Bajo"
    ))
  )

unique(Harvard_Trade$is_dependent)

str(Harvard_Trade)

Harvard_Trade$income_level <- factor(Harvard_Trade$income_level,
                              levels = c('Alto', 'Medio Alto', 'Medio Bajo','Bajo'))

Oil_Dependency <- Harvard_Trade %>% 
  filter(year == 2020) %>% 
  filter(is_dependent ==  T) %>%
  select(- is_dependent) %>% 
  mutate( country = ifelse(country == "Brunei Darussalam", "Brunei", country),
          gdpp = gdpp*1000) %>% 
  #ggplot
  ggplot()+
  #geom
  geom_label(aes(x = oil_dependency, y= gdpp,label = country),size = 4, hjust = -0.1 )+
  geom_point(aes(x = oil_dependency, y= gdpp, color = income_level))+
  #scales
  #coord_trans( y = "log2")+
  scale_y_continuous(
    trans = "log2",
    labels = dollar_format(prefix = "$", accuracy = 100),
    breaks =c( 1000,4000,12500))+
    #limits = c(0,40),
    #expand = c(0,0))+
  scale_color_manual(
    name = "Grupos de Ingreso",
    values = c("Bajo" = "#980000ff",
               "Medio Bajo" = "#FF5959" ,
               "Medio Alto" = "#39E66A",
               "Alto" = "#089931"))+
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0.6,1, by = 0.1),
    limits = c(0.6,1))+
  #theme
  theme(
    legend.key = element_rect( fill = "transparent", color = NA),
    legend.title = element_markdown( colour = "#980000ff",size = 16, face = "bold",
                                     family = "Montserrat"),
    legend.text =  element_markdown( colour = "#980000ff",size = 16, family = "Montserrat"),
    axis.title = element_markdown( colour = "#980000ff",size = 18, family = "Montserrat"),
    axis.text = element_markdown( colour = "#980000ff",size = 18, family = "Montserrat"),
    axis.ticks = element_line( colour = "#980000ff"),
    axis.text.x = element_text(angle = 90),
    panel.border = element_rect( colour = "#980000ff",
                                 fill = NA),
    panel.background = element_rect( fill = "transparent"),
    plot.background = element_rect( fill = "transparent", color = NA),
    panel.grid = element_blank(),
    panel.grid.major.y  = element_line(linetype = 2, colour ="grey" ),
    #panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(face = "bold", size = "34", colour = "#980000ff", 
                                        margin = margin(2,0,1,0), family = "Montserrat",
                                        lineheight = 0.1),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple( size = "20", colour = "#980000ff",
                                            margin = margin(2,0,6,0),family = "Montserrat",
                                            lineheight = 0.2),
    plot.caption = element_markdown( size = "14", colour = "#980000ff", family = "Montserrat"),
    plot.caption.position = "plot")+
  #labs
  labs( title = "Petroestados: Unos Más Ricos que Otros", 
        subtitle = "Ingresos vs Dependencia al Petróleo",
        x  = "Dependencia al petróleo en sus exportaciones", y = "Ingresos Per Cápita",
        caption = "Nota: Datos para el año 2020 y grupos de ingreso según World Bank. <br> Fuente: Atlas of Economic Complexity, World Economic Outlook, calculos propios.")



ggsave(Oil_Dependency, filename = "Oil_Dependency.jpg",
       bg ="white", units = "cm"  , width = 9, height = 9 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")





# Comparo los dos grupos pero con violines

weo <- weo_gdpp %>% 
  mutate( Block = ifelse( ISO %in% EU$ISO ,"UE",
                          ifelse( ISO %in% `ARAB`, "CCG-1",  F ))) %>% 
  filter( Block != F) %>%
  #ggplot
  ggplot(aes( x = Block, y = gdpp, fill = Block))+
  #geaoms
  geom_violin( alpha = 0.5)+
  stat_summary(fun = median, geom = "crossbar", alpha = 0.25)+ 
  #geom_jitter(width = 0.25, shape =21, color = "black")+
  geom_dotplot(binaxis = "y", stackdir = "center")+
  #scales
  scale_y_continuous(
    labels = dollar_format(prefix = "$"),
    expand = c(0,0),
    limits = c(0,120000))+
  scale_fill_manual(
    breaks = c("CCG-1","UE"),
    values = c("#980000ff", "#081499")
  )+
  #theme
  theme(
    legend.position = "none",
    axis.title = element_markdown( colour = "#980000ff",size = 18, family = "Montserrat"),
    axis.text = element_markdown( colour = "#980000ff",size = 18, family = "Montserrat"),
    axis.ticks = element_line( colour = "#980000ff"),
    panel.border = element_rect( colour = "#980000ff",
                                 fill = NA),
    panel.background = element_rect( fill = "transparent"),
    plot.background = element_rect( fill = "transparent", color = NA),
    panel.grid = element_blank(),
    panel.grid.major.y  = element_line(linetype = 2, colour ="grey" ),
    #panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(face = "bold", size = "34", colour = "#980000ff", 
                                        margin = margin(2,0,1,0), family = "Montserrat",
                                        lineheight = 0.1),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple( size = "20", colour = "#980000ff",
                                            margin = margin(2,0,6,0),family = "Montserrat",
                                            lineheight = 0.2),
    plot.caption = element_markdown( size = "14", colour = "#980000ff", family = "Montserrat"),
    plot.caption.position = "plot")+
  #labs
  labs( title = "Queda Camino por Delante", 
        subtitle = "Ingresos de la CCG-1 comparados con lo de la OCDE",
        x  = element_blank(), y = "Ingresos Per Cápita",
        caption = "Nota: Datos para el año 2020. Se excluye a Qatar del CCG <br> Fuente: World Economic Outlook, calculos propios.")


ggsave(weo, filename = "weo.jpg",
       bg ="white", units = "cm"  , width = 9, height = 9 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")



