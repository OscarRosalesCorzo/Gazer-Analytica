# Libraries

library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(readxl)
library(lubridate)
library(reshape2)
library(scales)





# Data

setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Panel Economico Venezuela")
getwd()

inflacion <- read_excel("Data/PEV_Data.xlsx", 
                        sheet = "Inflacion", col_types = c("date", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "skip", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric"), skip = 2)



tipo_cambio <- read_excel("Data/PEV_Data.xlsx", 
                          sheet = "Tipo de Cambio", col_types = c("date", 
                                                                  "numeric", "numeric", "skip"), skip = 2)



remuneracion <- read_excel("Data/PEV_Data.xlsx", 
                           sheet = "Remuneracion_Trabajadores", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric"), skip = 2)


petroleo <- read_excel("Data/PEV_Data.xlsx", 
                       sheet = "Petroleo", col_types = c("date", 
                                                         "numeric", "numeric"), skip = 2)



ui <- fluidPage( theme = shinytheme("spacelab"),
                 navbarPage(
                   "Panel Económico de Venezuela",
                   tabPanel("Tipo de Cambio",
                            
                            sidebarPanel(
                              # Hacer el dropdown
                              selectInput(inputId = "typetc", label = strong("Ver el tipo de cambio como:"),
                                          choices = c("Precio de la Divisa","Diferncia Porcentual: Oficial vs Paralelo"),
                                          selected = "Precio de la Divisa"),
                              # Hacer el selecionador de fechas rango de fechas
                              dateRangeInput("fechastc", "Rango de Fechas", start = ("2020-03-30") , end = Sys.Date() ,
                                             format = "yyyy-mm-dd",language = "es" , separator = "a", width = NULL)
                            ),
                            mainPanel(
                              h1("Tipo de Cambio"),
                              plotOutput("chart_divisa"),
                              
                            )
                   ),
                   tabPanel("Inflación",
                            sidebarPanel(
                              # Hacer el dropdown
                              selectInput(inputId = "typein", label = strong("Ver la inflación como:"),
                                          choices = c("Mensual","Interanual", "Acumulada"),
                                          selected = "Mensual"),
                              #sliderInput("fechasin",
                              #            "Rango de Fechas",
                              #            min = as.Date("2018-01-01","%Y-%m-%d"),
                              #            max = as.Date("2023-02-01","%Y-%m-%d"),
                              #            value=as.Date(c ("2019-06-01","2020-06-01") ),
                              #            timeFormat="%Y-%m")
                              # ),
                              # Hacer el selecionador de fechas rango de fechas
                              dateRangeInput("fechasin", "Rango de Fechas", start = ("2017-01") , end = Sys.Date() ,
                                             format = "yyyy-mm",language = "es" , separator = "a", width = NULL),
                              #Poner nota
                              helpText("Recomendación: Seleccionar el primero de cada mes.")
                            ),
                            mainPanel(
                              h1("Inflación"),
                              plotOutput("chart_inflacion")
                            )
                   ),
                   tabPanel("Remuneraciones a trabajadores",
                            sidebarPanel(
                              # Check box para seleccionar el tipo de trabajador a mostrar
                              checkboxGroupInput("typere", "Tipo de Trabajador",
                                                 choices = list("Promedio General" = "Promedio General",
                                                                "Gerencial" = "Gerencial",
                                                                "Profesionales y técnicos" = "Profesionales y técnicos",
                                                                "Obreros y operadores" = "Obreros y operadores"),
                                                 selected = "Promedio General"),
                              # Hacer el selecionador de fechas rango de fechas
                              dateRangeInput("fechasre", "Rango de Fechas", start = ("2021-01") , end = ("2022-12") ,
                                             format = "yyyy-mm",language = "es" , separator = "a", width = NULL),
                              #Poner nota
                              helpText("Recomendación: Seleccionar el primero de cada mes.")
                            ),
                            mainPanel(
                              h1("Remuneraciones"),
                              plotOutput("chart_remuneraciones")
                            )
                   ),
                   tabPanel("Petróleo",
                            #Dropdwon con las posibilidades
                            sidebarPanel(
                              selectInput(inputId =  "typepe", label = strong("Datos Petroleros:"),
                                          choices = c("Producción Petrolera", "Precio del Barril Venezolano",
                                                      "Posibles Ingresos por Petróleo"),
                                          selected = "Producción Petrolera"),
                              # Rango de Fechas
                              dateRangeInput("fechaspe", "Rango de Fechas", start = ("2020-01") , end = ("2022-12") ,
                                             format = "yyyy-mm",language = "es" , separator = "a", width = NULL)
                            ),
                            mainPanel(
                              h1("Petroleo"),
                              plotOutput("chart_petroeleo")
                            )
                   )
                 )
)

server <- function(input, output) {
  
  # Fechas dinamicas para el tipo de cambio
  
  TC <- reactive({
    tipo_cambio %>% 
      filter(Fecha >=input$fechastc[1] & Fecha<=input$fechastc[2])
  })
  
  # Capturar el input seleccionado en tipo de cambio
  
  xtc <- reactive({input$typetc})       
  
  output$chart_divisa <- renderPlot({
    
    # Usar el input selecionado para escoger grafica de tipo de cambio  
    
    if ( xtc() == "Precio de la Divisa" ) {
      
      
      TC() %>% 
        pivot_longer(cols = -Fecha,
                     names_to = "fuente",
                     values_to = "precio") %>% 
        #ggplot   
        ggplot(aes(x = Fecha))+
        #geoms  
        geom_line(aes(y = precio , col = fuente)) +
        #theme
        theme(legend.position = "bottom")+
        theme_classic()+
        #scales
        scale_y_continuous(
          labels = dollar_format(prefix = "Bs"),
          limits = c(0,NA))+
        #labs
        labs(title = "Valor Nominal del Dólar",
             y = element_blank(),
             caption = "Fuente: Dolar Today, Monitor Dolar, BCV")  
      
    }
    
    else {
      
      #Calcuculos previos
      TC() %>% 
        mutate(porcentual = (Dolar_Monitor/Dolar_BCV) - 1) %>% 
        #ggplot
        ggplot(aes(x = Fecha))+
        #geoms
        geom_line(aes(y= porcentual))+
        #scale
        scale_y_continuous(labels = scales::percent)+
        #theme
        theme_classic()+
        #labs
        labs(title = "Diferencia Porcentual Entre La Tasa Oficial y Tasa Paralela", 
             y = element_blank(),
             caption = "Fuente: Dolar Today, Monitor Dolar, BCV, calculos propios.")
      
    }
    
  })
  
  # Fechas dinamicas para la inflacion
  
  IN <- reactive({
    inflacion %>% 
      filter(Fecha >=input$fechasin[1] & Fecha<=input$fechasin[2])
  })    
  
  
  # Capturar el input seleccionado en inflacion
  
  xin <- reactive({ input$typein })  
  
  
  output$chart_inflacion <- renderPlot({
    
    if (  xin() == "Mensual" ) {
      
      IN() %>% 
        #Data manipulation
        select(Fecha, Inflacion_Mensual_OVF) %>%
        #ggplot
        ggplot()+
        #geoms
        geom_line(aes( x = Fecha  ,  y = Inflacion_Mensual_OVF))+
        #scales
        scale_y_continuous(labels = scales::percent,
                           limits = c(0,NA))+
        #theme
        theme_classic()+
        #labs
        labs(title = "Inflación Mensual", y = element_blank(),
             caption = "Fuente: Observatorio Venezolano de Finanzas")
      
    } 
    else {
      if( xin() == "Interanual" ) {
        
        IN() %>% 
          #Data manipulation
          select(Fecha , Inflacion_Interanual_OVF) %>% 
          #ggplot
          ggplot()+
          #geom
          geom_line(aes( x =  Fecha, y = Inflacion_Interanual_OVF ))+
          #scales
          scale_y_continuous(labels = scales::percent,
                             limits = c(0,NA))+
          #theme
          theme_classic()+
          #labs
          labs(title = "Inflación Interanual", y = element_blank(), 
               caption = "Fuente: Observatorio Venezolano de Finanzas")
        
        
      }
      else {
        
        IN() %>% 
          #Data manipulation
          select(Fecha, Inflacion_Acumulada_OVF) %>% 
          mutate( ano =  as.character(year(Fecha)) ,
                  mes =   month(Fecha, label = T),
                  mesn =  month(Fecha),
                  a = 2020,
                  uno = 1,
                  Fecha = as.Date( paste(a, mesn, uno, sep = "/"  ), format = "%Y/%m/%d"  )  )  %>%
          #ggplot
          ggplot()+
          #geom
          geom_line(aes( x= Fecha, y = Inflacion_Acumulada_OVF, col =ano ) )+
          #scales
          scale_y_continuous(labels = scales::percent, 
                             limits = c(0,NA))+
          scale_x_date(date_labels = "%b",
                       date_breaks = "1 month")+
          #theme
          theme_classic()+
          #labs
          labs(title = "Inflación Acumulada", y = element_blank(),
               caption = "Fuente: Observatorio Venezolano de Finanzas")
        
        
      } 
    }    
    
  })   
  
  
  
  # Fechas dinamicas para las remuneraciones justo a los salarios necesarios segun los checkbox
  
  RE <- reactive({  remuneracion[ ,c("Fecha", input$typere)  ] %>%  
      filter(Fecha >=input$fechasre[1] & Fecha<=input$fechasre[2])  })
  
  
  output$chart_remuneraciones <- renderPlot({
    
    RE() %>% 
      #Data manipulation
      melt(., id.vars = "Fecha", variable.name = "Salarios" ) %>%
      #ggplot
      ggplot()+
      #geom
      geom_line(aes( x = Fecha, y = value, col = Salarios))+
      #scales
      scale_color_manual(
        breaks = c("Gerencial", "Profesionales y técnicos", "Obreros y operadores", "Promedio General"),
        values = c("red",'blue',"green","purple"))+
      scale_y_continuous(
        labels = dollar_format(prefix = "$"),
        limits = c(0,NA))+
      #theme
      theme_classic()+
      #labs
      labs(title = "Remuneraciones a Trabajadores", y = element_blank(),
           caption = "Fuente: Observatorio Venezolano de Finanzas y ANOVA")
    
    
  })
  
  # Recolectar la informacion del dropdpown
  
  xpe <- reactive({ input$typepe })
  
  # recolectar informacion de las fechas
  
  
  PE <- reactive({
    petroleo %>% 
      filter(Fecha >=input$fechaspe[1] & Fecha<=input$fechaspe[2])
  })
  
  
  output$chart_petroeleo <- renderPlot({
    
    
    if( xpe() == "Producción Petrolera" ) {
      
      PE() %>% 
        #ggplot
        ggplot()+
        #geom
        geom_line(aes( x= Fecha, y = Produccion))+
        #scale
        scale_y_continuous(limits = c(0, NA))+
        #theme
        theme_classic()+
        #labs
        labs(title = "Producción Petrolera", subtitle = "En Miles de Barriles Diarios",
             y = element_blank(),
             caption = "Fuente: OPEP")
      
    }
    else {
      if ( xpe( ) == "Precio del Barril Venezolano"  ) {
        
        PE() %>% 
          #ggplot
          ggplot()+
          #geom
          geom_line(aes( x= Fecha, y = Precio))+
          #scale
          scale_y_continuous(
            labels = dollar_format(prefix = "$"),
            limits = c(0, NA))+
          #theme
          theme_classic()+
          #labs
          labs(title = "Precio del Barril Venezolano (Merey)", y = element_blank(),
               caption = "Fuente: OPEP")
        
        
      }
      else{
        
        PE() %>% 
          #manipulacion de la data
          mutate(Ingresos = Precio*Produccion ) %>% 
          #ggplot
          ggplot()+
          #geom
          geom_line(aes( x= Fecha, y = Ingresos))+
          #scale
          scale_y_continuous(
            labels = dollar_format(prefix = "$"),
            limits = c(0, NA))+
          #theme
          theme_classic()+
          #labs
          labs(title = "Posibles Ingresos Venezolanos por Venta de Petróleo", subtitle = "En miles de USD",
               y = element_blank(),
               caption = "Fuente: OPEP, calculos propios.")
        
      }
    }
    
  })
  
}                 



Sys.setenv("LANGUAGE"="ESP")

Sys.setlocale("LC_ALL", "Spanish")          

shinyApp(ui = ui, server = server)                 

library(shiny)
runApp("PEV")
