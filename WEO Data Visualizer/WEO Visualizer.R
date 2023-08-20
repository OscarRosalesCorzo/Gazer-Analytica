# Libraries

library(tidyverse)
library(readxl)
library(ggplot2)
library(shiny)
library(data.table)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(ggthemes)
library(stringr)
library(scales)
library(ggtext)
library(stargazer)
library(gt)


WEO_Data %>% 
  distinct(`Metric-Unit`) %>% 
  head(1)

WEO_Data %>% 
  group_by(Country) %>% 
  filter(`Metric-Unit` == "GDP, constant prices - Percent change" ) %>% 
  summarise(
    
    mean = round (mean(Value, na.rm = T),3),
    min = min(Value)
  ) %>% 
  gt( ) %>% 
  tab_header(
    title = md("Summary Table")
  )




install.packages("gt")
# data


?stargazer

#setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica/WEO Data Visualizer")
#getwd()

WEO_Data <- read_excel("WEO_Data.xlsx")
colnames(WEO_Data)

# To have a better sence of the data. I detect that most of columns are characters and few are numeric.
# Given the momnent it will be easier to have all columns characters when pivoting.

skim(WEO_Data)



# Transforming data to have it in correct formant

WEO_Data <- WEO_Data %>%  
  select( -`Country/Series-specific Notes`, - `Estimates Start After`) %>% 
  rename(Metric = "Subject Descriptor") %>%
  mutate( `2019` =  as.character(`2019`)) %>% 
  pivot_longer(cols = 6:48,
               names_to = "Year",
               values_to = "Value") %>% 
  mutate(Value = ifelse(Value == "n/a", NA, Value),
         Year = as.numeric(Year),
         Value = as.numeric(Value)) 

# Country Regions

Countries_Regions <- read_csv("Countries Regionscsv.csv")

# I clean it a little to just get what I want

colnames( Countries_Regions)


Countries_Regions <- Countries_Regions %>%
  select(`alpha-3`, name, region, `sub-region`) %>% 
  rename(ISO = `alpha-3` )

# Turning to title the vairbles names of the new dataframe

colnames(Countries_Regions) <- stringr::str_to_title(colnames(Countries_Regions) )

# nonthe less, I need to turn one varible into all uppercar

Countries_Regions <- Countries_Regions %>% 
  rename(ISO = `Iso` )

# I join the WEO data with the regions

WEO_Data <- WEO_Data %>% 
  left_join(Countries_Regions, by = "ISO") %>% 
  select(-Name)



Selected_Metrics <- c("Gross domestic product, constant prices - Percent change", "Gross domestic product, current prices - U.S. dollars",
                      "Gross domestic product, current prices - Purchasing power parity; international dollars", "Gross domestic product per capita, constant prices - Purchasing power parity; 2017 international dollar",
                      "Gross domestic product per capita, current prices - U.S. dollars", "Output gap in percent of potential GDP - Percent of potential GDP",
                      "Gross domestic product based on purchasing-power-parity (PPP) share of world total - Percent", "Total investment - Percent of GDP",
                      "Gross national savings - Percent of GDP", "Inflation, average consumer prices - Percent change", "Inflation, end of period consumer prices - Percent change",
                      "Unemployment rate - Percent of total labor force", "Population - Persons", "General government revenue - Percent of GDP",
                      "General government total expenditure - Percent of GDP", "General government net lending/borrowing - Percent of GDP", "General government structural balance - Percent of potential GDP",
                      "General government primary net lending/borrowing - Percent of GDP", "General government net debt - Percent of GDP", "General government gross debt - Percent of GDP",
                      "Current account balance - U.S. dollars",  "Current account balance - Percent of GDP") 

# I will delete from Units filter to only get the time series that I want

WEO_Data <- WEO_Data %>% 
  mutate(`Metric-Unit` = paste(Metric, Units, sep = " - "))%>% 
  filter(`Metric-Unit` %in% Selected_Metrics) %>% 
  mutate(Metric = str_replace(Metric, pattern = "Gross domestic product", "GDP"),
         Units = str_replace(Units, pattern = "Purchasing power parity; 2017 international dollar", "PPP"),
         Units = str_replace(Units, pattern = "Purchasing power parity; international dollars", "PPP")) %>% 
  mutate(`Metric-Unit` = paste(Metric, Units, sep = " - "),
         `Metric-Unit` = ifelse(`Metric-Unit` == "Output gap in percent of potential GDP - Percent of potential GDP", "Output gap in percent of potential GDP", `Metric-Unit`),
         `Metric-Unit` = ifelse(`Metric-Unit` == "GDP based on purchasing-power-parity (PPP) share of world total - Percent","GDP based on PPP - share of world total", `Metric-Unit`  ),
         #`Metric-Unit` = ifelse( `Metric-Unit` == "Inflation, end of period consumer prices - Percent change", "Inflation, end of period consumer prices", `Metric-Unit` ),
         #`Metric-Unit` = ifelse( `Metric-Unit` == "Inflation, average consumer prices - Percent change", "Inflation, average consumer prices", `Metric-Unit` ),
         `Metric-Unit` = ifelse(`Metric-Unit` == "Population - Persons", "Population",`Metric-Unit`  ) )

  

WEO_Data <- WEO_Data %>% 
  mutate(Date = paste( Year, "01-01", sep  = "-"),
         Date = as.Date(Date)) %>% 
  select(-Year)



RealMetrics <- c("GDP, constant prices - Percent change", "GDP, current prices - PPP", "GDP, current prices - U.S. dollars","GDP per capita, constant prices - PPP", "GDP per capita, current prices - U.S. dollars" ,
                 "GDP based on PPP - share of world total", "Gross national savings - Percent of GDP", "Total investment - Percent of GDP", "Inflation, average consumer prices - Percent change",
                 "Inflation, end of period consumer prices - Percent change")



ExternealMetrics <- c("Current account balance - U.S. dollars", "Current account balance - Percent of GDP")

SocioecnomicMetrics <- c("Population", "Unemployment rate - Percent of total labor force")


FiscalMetrics <- c("General government revenue - Percent of GDP", "General government total expenditure - Percent of GDP", "General government net lending/borrowing - Percent of GDP",
                   "General government structural balance - Percent of potential GDP", "General government primary net lending/borrowing - Percent of GDP",
                   "General government net debt - Percent of GDP", "General government gross debt - Percent of GDP")

unique(WEO_Data$Metric) 

Countries <- unique(WEO_Data$Country)  


percent_metrics <- WEO_Data %>%
  mutate( x =  str_detect(Units ,pattern = "Percent") ) %>% 
  filter( x  == T) %>% 
  distinct(`Metric-Unit`) %>% 
  pull(`Metric-Unit`)


percapita_metrics <- WEO_Data %>%
  mutate( x =  str_detect(`Metric-Unit` ,pattern = "per capita") ) %>% 
  filter( x  == T) %>% 
  distinct(`Metric-Unit`) %>% 
  pull(`Metric-Unit`)


billions_metrics <- WEO_Data %>%
  mutate( x =  str_detect(Scale ,pattern = "Billions") ) %>% 
  filter( x  == T) %>% 
  distinct(`Metric-Unit`) %>% 
  pull(`Metric-Unit`)


?multiInput

# Building the shiny app

# Define user interface

ui <- fluidPage( theme = shinytheme("spacelab"),
                 navbarPage(
                   "World Economic Outlook Visualizer",
                   tabPanel("Real Economy",
                              
                              sidebarPanel(
                              # code the dropdown
                              selectInput(inputId = "realmetric", label = strong("Select Metric"),
                                 choices = RealMetrics),
                              # code the multiple selection country
                              multiInput(inputId =  "realcountry",  label = strong("Select Country"),
                                         choices = Countries, selected = "Venezuela"),
                              # code dates selecter
                              dateRangeInput("realdates", "Dates Range", start = ("1980-01-01") , end = ("2022-01-01")  ,
                                    format = "yyyy-mm-dd",language = "en" , separator = "to", width = NULL)
                              
                   ),
                   
                    mainPanel(
                      textOutput("title_realmetric"),
                      tags$head(tags$style("#title_realmetric{
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                      )),
                      plotOutput("chart_realmetric", width = "500px", height = "400px"),
                      gt_output("realtable"),
                      width = 3
                     
                   )),
                   
                   tabPanel("External Sector",
                     sidebarPanel(
                       # code the dropdown
                       selectInput(inputId = "externalmetric", label = strong("Select Metric"),
                                   choices = ExternealMetrics),
                       # code the multiple selection country
                       multiInput(inputId =  "externalcountry",  label = strong("Select Country"),
                                  choices = Countries, selected = "Venezuela"),
                       # code dates selecter
                       dateRangeInput("externaldates", "Dates Range", start = ("1980-01-01") , end = ("2022-01-01")  ,
                                      format = "yyyy-mm-dd",language = "en" , separator = "to", width = NULL)
                       
                       
                   ),
                   
                     mainPanel(
                       textOutput("title_external"),
                       tags$head(tags$style("#title_external{
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                       )),
                       plotOutput("chart_externalmetric", width = "1200px", height = "600px"),
                       gt_output("externaltable")
                     
                   )),
                   
                   tabPanel("Fiscal Sector",
                      sidebarPanel(      
                     # code the dropdown
                     selectInput(inputId = "fiscalmetric", label = strong("Select Metric"),
                                 choices = FiscalMetrics),
                     # code the multiple selection country
                     multiInput(inputId =  "fiscalcountry",  label = strong("Select Country"),
                                choices = Countries, selected = "Venezuela"),
                     # code dates selecter
                     dateRangeInput("fiscaldates", "Dates Range", start = ("1980-01-01") , end = ("2022-01-01")  ,
                                    format = "yyyy-mm-dd",language = "en" , separator = "to", width = NULL
                                    
                    )),
                    
                        mainPanel(
                          textOutput("title_fiscalmetric"),
                          tags$head(tags$style("#title_fiscalmetric{
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                          )),
                          plotOutput("chart_fiscalmetric", width = "1200px", height = "600px"),
                          gt_output("fiscaltable")
                      
                    )),
                    
                    tabPanel("Socioeconomic",
                      sidebarPanel(
                      # code the dropdown
                      selectInput(inputId = "sociometric", label = strong("Select Metric"),
                                  choices = SocioecnomicMetrics),
                      # code the multiple selection country
                      multiInput(inputId =  "sociocountry",  label = strong("Select Country"),
                                 choices = Countries, selected = "Venezuela"),
                      # code dates selecter
                      dateRangeInput("sociodates", "Dates Range", start = ("1980-01-01") , end = ("2022-01-01")  ,
                                     format = "yyyy-mm-dd",language = "en" , separator = "to", width = NULL)),
                      
                   mainPanel(
                     textOutput("title_sociometric"),
                     tags$head(tags$style("#title_sociometric{
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                     )),
                     plotOutput("chart_sociometric", width = "1200px", height = "600px"),
                     gt_output("sociotable")
                   
                     )
                   ))
                   
                   )
                   


# Define server
                   
server <- function(input, output){
  
  # Input Real Metric
  
  # Dynamic chart formatting in real metrics
  
  
  ru <- reactive({
    WEO_Data %>% 
      filter(Date >=input$realdates[1] & Date<=input$realdates[2]) %>% 
      filter(`Metric-Unit` == input$realmetric) %>% 
      filter(Country %in% input$realcountry) %>% 
      distinct(`Metric-Unit`) %>% 
      pull(`Metric-Unit`)
    
    
  })
  
  # Dynamic dates in real metric
  
  rm <- reactive({
    WEO_Data %>% 
      filter(Date >=input$realdates[1] & Date<=input$realdates[2]) %>% 
      filter(`Metric-Unit` == input$realmetric) %>% 
      filter(Country %in% input$realcountry)
      
    
  })

  # Output real
  
  output$title_realmetric <- renderText({
    
    ru()
    
  })
  
  output$chart_realmetric <- renderPlot({
    
    
    if( ru() %in% percent_metrics ) {
    
        rm() %>% 
        #Data manipulation
        mutate(Value = Value/100) %>% 
        #ggplot
        ggplot()+
        #geoms
        geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
        geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #theme
        theme_classic()+
        theme(
          axis.text = element_textbox_simple( size = 14),
          legend.text = element_text( size = 14),
          legend.title = element_text(size = 14),
          panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
        )+
        #scales
        scale_y_continuous(labels = scales::percent,
                           expand = c(0,0))+
        #labs
        labs(y = element_blank(), x = element_blank())
        
    }
    
    else{
      
      if(ru() %in% percapita_metrics  ){
        
        rm() %>% 
          
          #ggplot
          ggplot()+
          #geoms
          geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
          #theme
          theme_classic()+
          theme(
            axis.text = element_textbox_simple( size = 14),
            legend.text = element_textbox( size = 14),
            legend.title = element_text(size = 14),
            panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
          )+
          #scales
          scale_y_continuous(labels = dollar_format(prefix = "$"),
                             limits = c(0, NA),
                             expand = c(0,0))  +
          #labs
          labs(y = element_blank(), x = element_blank())
        
      } 
    else  {
      
      if(ru() %in% billions_metrics  ){
      
        rm() %>% 
          #ggplot
          ggplot()+
          #geoms
          geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
          theme_classic()+
          theme(
            axis.text = element_textbox_simple( size = 14),
            legend.text = element_text( size = 14),
            legend.title = element_text(size = 14),
            panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
          )+
          #scales
          scale_y_continuous(labels = dollar_format(prefix = "$", suffix = " Billions"),
                                      limits = c(0, NA),
                                      expand = c(0,0))+ 
          #labs)  
        labs(y = element_blank(), x = element_blank())
        
        
    }
      
    }  
      
    }
    
    
  })
  
  
  output$realtable <- render_gt({
    
    
    
    # Create the table
    
    
    if(ru() %in% percent_metrics  ) {
      
      
      # Create the table
      
      rm() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round (mean(Value, na.rm = T),2),
          Min = round(min(Value,na.rm = T), 2),
          Max = round(max(Value, na.rm = T),2)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
    }
    
    else {
      
      rm() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round (mean(Value, na.rm = T), 3),
          Min =  min(Value,na.rm = T),
          Max =  max(Value, na.rm = T)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
      
      
    }
    
    
  })
  
  
  
  # Input External Metric
  
  # Dynamic chart formatting in External Metric
  
  
  ref <- reactive({
    WEO_Data %>% 
      filter(Date >=input$externaldates[1] & Date<=input$externaldates[2]) %>% 
      filter(`Metric-Unit` == input$externalmetric) %>% 
      filter(Country %in% input$externalcountry) %>% 
      distinct(`Metric-Unit`) %>% 
      pull(`Metric-Unit`)
    
    
  })
  
  # Dynamic dates in External Metric
  
  ret <- reactive({
    WEO_Data %>% 
      filter(Date >=input$externaldates[1] & Date<=input$externaldates[2]) %>% 
      filter(`Metric-Unit` == input$externalmetric) %>% 
      filter(Country %in% input$externalcountry)
    
    
  })
  
  # Output real
  
  output$title_external <- renderText({
    
    ref()
    
  })
  
  output$chart_externalmetric <- renderPlot({
    
    
    if( ref() %in% percent_metrics ) {
      
      ret() %>% 
        #Data manipulation
        mutate(Value = Value/100) %>% 
        #ggplot
        ggplot()+
        #geoms
        geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
        geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #theme
        theme_classic()+
        theme(
          axis.text = element_textbox_simple( size = 14),
          legend.text = element_text( size = 14),
          legend.title = element_text(size = 14),
          panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
        )+
        #scales
        scale_y_continuous(labels = scales::percent,
                           expand = c(0,0))+
        #labs
        labs(y = element_blank(), x = element_blank())
      
    }
    
    else{
      
      if(ref() %in% percapita_metrics  ){
        
        ret() %>% 
          
          #ggplot
          ggplot()+
          #geoms
          geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
          #theme
          theme_classic()+
          theme(
            axis.text = element_textbox_simple( size = 14),
            legend.text = element_textbox( size = 14),
            legend.title = element_text(size = 14),
            panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
          )+
          #scales
          scale_y_continuous(labels = dollar_format(prefix = "$"),
                             limits = c(0, NA),
                             expand = c(0,0))  +
          #labs
          labs(y = element_blank(), x = element_blank())
        
      } 
      else  {
        
        if(ref() %in% billions_metrics  ){
          
          ret() %>% 
            #ggplot
            ggplot()+
            #geoms
            geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
            geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
            #theme
            theme_classic()+
            theme(
              axis.text = element_textbox_simple( size = 14),
              legend.text = element_text( size = 14),
              legend.title = element_text(size = 14),
              panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
            )+
            #scales
            scale_y_continuous(labels = dollar_format(prefix = "$", suffix = " Billions"),
                               limits = c(NA, NA),
                               expand = c(0,0))+ 
            #labs)  
            labs(y = element_blank(), x = element_blank())
          
          
        }
        
      }  
      
    }
    
    
  })
  
  
  output$externaltable <- render_gt({
    
    
    # Create the table
    
    
    if(ref() %in% percent_metrics  ) {
      
      
      # Create the table
      
      ret() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round (mean(Value, na.rm = T),2),
          Min = round(min(Value,na.rm = T), 2),
          Max = round(max(Value, na.rm = T),2)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
    }
    
    else {
      
      ret() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round( mean(Value, na.rm = T), 3) ,
          Min =  min(Value,na.rm = T),
          Max =  max(Value, na.rm = T)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
      
      
    }
    
    
  })
  
  

  # Input Fiscal Metric
  
  # Dynamic chart formatting in External Metric
  
  
  rff <- reactive({
    WEO_Data %>% 
      filter(Date >=input$fiscaldates[1] & Date<=input$fiscaldates[2]) %>% 
      filter(`Metric-Unit` == input$fiscalmetric) %>% 
      filter(Country %in% input$fiscalcountry) %>% 
      distinct(`Metric-Unit`) %>% 
      pull(`Metric-Unit`)
    
    
  })
  
  # Dynamic dates in External Metric
  
  rft <- reactive({
    WEO_Data %>% 
      filter(Date >=input$fiscaldates[1] & Date<=input$fiscaldates[2]) %>% 
      filter(`Metric-Unit` == input$fiscalmetric) %>% 
      filter(Country %in% input$fiscalcountry)
    
    
  })
  
  
  # Output real
  
  output$title_fiscalmetric <- renderText({
    
    rff()
    
  })
  
  output$chart_fiscalmetric <- renderPlot({
    
    
    if( rff() %in% percent_metrics ) {
      
      rft() %>% 
        #Data manipulation
        mutate(Value = Value/100) %>% 
        #ggplot
        ggplot()+
        #geoms
        geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
        geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #theme
        theme_classic()+
        theme(
          axis.text = element_textbox_simple( size = 14),
          legend.text = element_text( size = 14),
          legend.title = element_text(size = 14),
          panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
        )+
        #scales
        scale_y_continuous(labels = scales::percent,
                           expand = c(0,0))+
        #labs
        labs(y = element_blank(), x = element_blank())
      
    }
    
    else{
      
      if(rff() %in% percapita_metrics  ){
        
        rft() %>% 
          
          #ggplot
          ggplot()+
          #geoms
          geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
          #theme
          theme_classic()+
          theme(
            axis.text = element_textbox_simple( size = 14),
            legend.text = element_textbox( size = 14),
            legend.title = element_text(size = 14),
            panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
          )+
          #scales
          scale_y_continuous(labels = dollar_format(prefix = "$"),
                             limits = c(0, NA),
                             expand = c(0,0))  +
          #labs
          labs(y = element_blank(), x = element_blank())
        
      } 
      else  {
        
        if(rff() %in% billions_metrics  ){
          
          rft() %>% 
            #ggplot
            ggplot()+
            #geoms
            geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
            geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
            #theme
            theme_classic()+
            theme(
              axis.text = element_textbox_simple( size = 14),
              legend.text = element_text( size = 14),
              legend.title = element_text(size = 14),
              panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
            )+
            #scales
            scale_y_continuous(labels = dollar_format(prefix = "$", suffix = " Billions"),
                               limits = c(NA, NA),
                               expand = c(0,0))+ 
            #labs)  
            labs(y = element_blank(), x = element_blank())
          
          
        }
        
      }  
      
    }
    
    
  })
  
  
  output$fiscaltable <- render_gt({
    
    
    # Create the table
    
    
    if(rff() %in% percent_metrics  ) {
      
      
      # Create the table
      
      rft() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round (mean(Value, na.rm = T),2),
          Min = round(min(Value,na.rm = T), 2),
          Max = round(max(Value, na.rm = T),2)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
    }
    
    else {
      
      rft() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean =round(  mean(Value, na.rm = T), 3)  ,
          Min =  min(Value,na.rm = T),
          Max =  max(Value, na.rm = T)
        ) %>% 
        gt( ) %>% 
        tab_header(
          title = md("Summary Table")
        )
      
      
      
      
    }
    
    
    
  })
  
  
  # Input Socioeconomic Metric
  
  # Dynamic chart formatting in External Metric
  
  
  rsef <- reactive({
    WEO_Data %>% 
      filter(Date >=input$sociodates[1] & Date<=input$sociodates[2]) %>% 
      filter(`Metric-Unit` == input$sociometric) %>% 
      filter(Country %in% input$sociocountry) %>% 
      distinct(`Metric-Unit`) %>% 
      pull(`Metric-Unit`)
    
    
  })
  
  # Dynamic dates in External Metric
  
  rset <- reactive({
    WEO_Data %>% 
      filter(Date >=input$sociodates[1] & Date<=input$sociodates[2]) %>% 
      filter(`Metric-Unit` == input$sociometric) %>% 
      filter(Country %in% input$sociocountry)
    
    
  })
  
  
  # Output real
  
  output$title_sociometric <- renderText({
    
    rsef()
    
  })
  
  output$chart_sociometric <- renderPlot({
    
    
    if( rsef() %in% percent_metrics ) {
      
      rset() %>% 
        #Data manipulation
        mutate(Value = Value/100) %>% 
        #ggplot
        ggplot()+
        #geoms
        geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
        geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #theme
        theme_classic()+
        theme(
          axis.text = element_textbox_simple( size = 14),
          legend.text = element_text( size = 14),
          legend.title = element_text(size = 14),
          panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
        )+
        #scales
        scale_y_continuous(labels = scales::percent,
                           expand = c(0,0))+
        #labs
        labs(y = element_blank(), x = element_blank())
      
    }
    
    else{
      
      if(rsef() %in% percapita_metrics  ){
        
        rset() %>% 
          
          #ggplot
          ggplot()+
          #geoms
          geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
          #theme
          theme_classic()+
          theme(
            axis.text = element_textbox_simple( size = 14),
            legend.text = element_textbox( size = 14),
            legend.title = element_text(size = 14),
            panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
          )+
          #scales
          scale_y_continuous(labels = dollar_format(prefix = "$"),
                             limits = c(0, NA),
                             expand = c(0,0))  +
          #labs
          labs(y = element_blank(), x = element_blank())
        
      } 
      else  {
          
          rset() %>% 
            #ggplot
            ggplot()+
            #geoms
            geom_line( aes(x= Date, y = Value, color = Country), size = 2)+
            #theme
            theme_classic()+
            theme(
              axis.text = element_textbox_simple( size = 14),
              legend.text = element_text( size = 14),
              legend.title = element_text(size = 14),
              panel.grid.major.y  = element_line(linetype = 2, colour ="grey" )
            )+
            #scales
            scale_y_continuous(labels = dollar_format(suffix = " Millons"),
                               limits = c(0, NA),
                               expand = c(0,0))+ 
            #labs)  
            labs(y = element_blank(), x = element_blank())
          
          
        }
        
      }  
      
    }
    
    
  )
  
  
  output$sociotable <- render_gt({
    
    
    if(rsef() %in% percent_metrics  ) {
      
      
      # Create the table
      
      rset() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round (mean(Value, na.rm = T),2),
          Min = round(min(Value,na.rm = T), 2),
          Max = round(max(Value, na.rm = T),2)
          ) %>% 
            gt( ) %>% 
            tab_header(
              title = md("Summary Table")
            )
      
      
    }
    
    else {
      
      rset() %>%
        
        select(Country, Value) %>%
        group_by(Country) %>% 
        summarise(
          
          Mean = round( mean(Value, na.rm = T), 3),
          Min =  min(Value,na.rm = T),
          Max =  max(Value, na.rm = T)
          ) %>% 
            gt( ) %>% 
            tab_header(
              title = md("Summary Table")
            )
          
      
      
    
    } 
    
  })
    
  
}



shinyApp(ui = ui, server = server)    

