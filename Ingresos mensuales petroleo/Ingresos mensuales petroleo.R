

# Ingresos del Estado por concepto de petroleo percapita

# Cargar  datos
library(readxl)
Merey_Prices <- read_excel("Ingresos mensuales petroleo/Data/Oil_Prices.xlsx", 
                           col_types = c("date", "numeric"), skip = 2)
View(Merey_Prices)

Venezuelan_Oil <- read_excel("Ingresos mensuales petroleo/Data/Veneuelan_Oil_Production.xlsx", 
                             col_types = c("date", "numeric"), skip = 2)



Venezuelan_Income <-  
  #Calculos
  left_join(Merey_Prices, Venezuelan_Oil, by = "Fecha") %>%
  mutate(Income = Price*Oil_Production*30/28704947*1000 ) %>% 
  mutate(Fecha = as.Date(Fecha, format = "%Y-%b-%d")) %>% 
  select(Fecha, Income) %>% 
  #ggplot
  ggplot(aes(x = Fecha,y = Income))+
  geom_line()+
  geom_point()+
  #scales
  scale_y_continuous(
    breaks = seq(0,70, by = 10),
    limits = c(0,70),
    expand = c(0,0))+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")




# Hisorical Venezuela Exports

Exports <- read_excel("Ingresos mensuales petroleo/Data/World Oil Exports.xlsx")

# Limpiar data para tener solo las exportaciones de Venezuela

colnames(Exports)

Exports <- Exports %>% 
  filter(`Section 5 - Oil trade` == "Venezuela")

# Cambiar nombres de las columnas

colnames(Exports)<- c("Country", 1980:2021)

# Hacer alargar la data

Venezuelan_Exports <- Exports %>% 
  pivot_longer(
    cols = 2:43,
    names_to = "Year",
    values_to = "Oil_Exports",
    values_transform = as.numeric
  ) %>% 
  select(-Country)

Venezuelan_Exports

# Historical venezuelan oil prices


Oil_Prices <- read_excel("Ingresos mensuales petroleo/Data/Historical Oil Prices.xlsx")

Venezuelan_Oil_Prices <- Oil_Prices %>% 
  filter(`Section 7 - Oil prices` == "Venezuela - Merey") %>% 
  mutate( N = seq(1, length(`Section 7 - Oil prices`), by = 1)) %>% 
  filter(N == 3)  %>% 
  select(-N) %>% 
  pivot_longer( cols =  2:253,
                names_to = "Year",
                values_to = "Merey_Prices") %>% 
  filter(Merey_Prices != "na") %>% 
  mutate( Year = 2005:2021) %>% 
  select(- `Section 7 - Oil prices`) %>% 
  mutate(
    Year = as.character(Year),
    Merey_Prices = as.numeric(Merey_Prices))

Venezuelan_Oil_Prices



stargazer(as.data.frame(Venezuelan_Oil_Prices), type = "text")

# Cargar la poblacion de venezuela

library(readxl)

World_Population <- read_excel("Ingresos mensuales petroleo/Data/World Population.xls", 
                               skip = 2)
View(World_Population)

Venezuelan_Population <- World_Population %>% 
  filter(`Country Code` == "VEN") %>% 
  select(- `Country Code`, -`Indicator Code`, - `Indicator Name`) %>% 
  pivot_longer(
    cols = 2:63,
    names_to = "Year",
    values_to = "Population"
  ) %>% 
  select(-`Country Name`)

Venezuelan_Population

# Juntar la data de los precios con la data de las exportaciones

Venezuelan_Income <- left_join(Venezuelan_Exports, Venezuelan_Oil_Prices, by = "Year") 

# Hacer los calculos

Venezuelan_Income_Chart <- 
  #Calculos Previos
  left_join(Venezuelan_Income, Venezuelan_Population, by = "Year") %>% 
  filter(!is.na(Merey_Prices)) %>% 
  mutate(
    Oil_Exports = Oil_Exports*1000*365,
    Monthly_Income_PerCapita = Oil_Exports*Merey_Prices/12/Population
  ) %>% 
  #ggplot
  ggplot(aes(group = 1))+
  #geoms
  geom_line(aes(x= Year, y = Monthly_Income_PerCapita), col = "#980000ff")+
  geom_point(aes(x= Year, y = Monthly_Income_PerCapita), col = "#980000ff")+
  geom_vline( xintercept = "2017", col = "#4B4B4B", lwd = 1)+
  #scales
  scale_y_continuous(
    labels = dollar_format(prefix = "$"),
    breaks = seq(25,200, by = 25),
    limits = c(0,200),
    expand = c(0,0))+
  #theme
  theme(
    axis.title = element_text( colour = "#980000ff"),
    axis.text = element_textbox_simple( colour = "#980000ff"),
    axis.ticks = element_line( colour = "#980000ff"),
    axis.text.x = element_text(angle = 90),
    panel.border = element_rect( colour = "#980000ff",
                                 fill = NA),
    panel.background = element_rect( fill = "transparent"),
    plot.background = element_rect( fill = "transparent", color = NA),
    panel.grid = element_blank(),
    panel.grid.major.y  = element_line(linetype = 2, colour ="grey" ),
    #panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(face = "bold", size = "18", colour = "#980000ff", 
                                        margin = margin(0,0,2,0)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(face = "bold", size = "10", colour = "#980000ff",
                                           margin = margin(0,0,6,0)),
    plot.caption = element_markdown( size = "6", colour = "#980000ff"),
    plot.caption.position = "plot")+
  labs(
    title = "No Hay Tanto Para Repartir",
    subtitle = " Valor de Exportaciones Petroleras Venezolanas Mensuales Per C?pita",
    caption = "<span style='color:#4B4B4B;'> Nota: L?nea gris se?ala comienzo de sanciones con efecto sobre PDVSA.</span> 
    <br><br>Fuete: OPEP, World Bank, calculos propios.",
    y = element_blank(), x = element_blank())

Venezuelan_Income_Chart  


ggsave(Venezuelan_Income_Chart, filename = "venezuelan_oil_income.jpg",
       bg ="white", units = "cm"  , width = 14, height = 14 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")



6803/12


sqrt(45.4516752)


0.522752/0.1889091

Sys.setenv("LANGUAGE"="ESP")

Sys.setlocale("LC_ALL", "Spanish")




ggsave(BV1, filename = "bv1.jpg",
       bg ="white", units = "cm"  , width = 10, height = 10 ,
       path = "C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Presentaciones/Charts")

