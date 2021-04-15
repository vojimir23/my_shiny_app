library(shiny)
library(dashboard)
library(shinydashboard)
library(plotly)
library(shinythemes)
library("readxl")
library(gapminder)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(leaflet)


linebreaks <- function(n){HTML(strrep(br(), n))}
Dataset <- read_excel("data.xlsx") 
names <- names(Dataset)
DatasetTS <- gapminder #Second data with timeseries.


ui <- fluidPage(theme = shinytheme("readable"),dashboardPage( 
  dashboardHeader(title="Vojimir's app"),
  dashboardSidebar(sidebarMenu(
    menuItem("Data view",tabName = "First", icon =icon("calendar")),
    menuItem("Graphical representation",tabName = "Second", icon = icon("map")),
    menuItem("GDP comparison", tabName="Third", icon =icon("square")),
    menuItem("World map", tabName="Fourth", icon =icon("circle")))),
  
  dashboardBody(
    tabItems(
      tabItem("First",actionButton("show","Click to see the dataset"),hr(),dataTableOutput("dataset")
      ),
      tabItem("Second",div(h1("World's data"),linebreaks(1),style="width:300px;height:50px;padding-left:15px;"),sidebarPanel(div(selectInput("xcol","Choose the first variable (X)",names[c(3:9)]),
                                    selectInput("ycol","Choose the second variable (Y)",names[c(3:9)])),style="width:400px;height:250px;padding-left:15px;"),
              div(plotlyOutput("myplot"),style="width:1000px;padding-left:450px;"),div(h5("Summary statistics of chosen variables:"),verbatimTextOutput("sum1"),verbatimTextOutput("sum2"),style="width:400px;height:20px;padding-left:20px;"),
                              linebreaks(5),      div(p(h3("Short description of variables:"),br(),
                                                         strong("Income")," - Gross domestic product per person adjusted for differences in purchasing power in fixed dollars, fixed 2011 prices.",br(),
                                                         strong("CO2 emissions")," - Carbon dioxide emissions from the burning of fossil fuels ( tonnes of CO2 per person).",br(),
                                                         strong("Child mortality"), " - Death of children under five years of age per 1000 live births.",br(),
                                                         strong("Babies per woman")," - Total fertility rate. The number of children that would be born to each woman with prevailing age-specific fertility rates.",br(),
                                                         strong("Democracy index (EIU)"), " - Democracy Index is using the data from EIU to express the quality of democracies as a number between 0 - 100.",br(), 
                                                         strong("Percentage of women in parliaments")," - Percentage of national parliamentary seats held by women. Lower and upper houses combined."),style="width:1300px;padding-left:450px;")),
      
      
      
      tabItem("Third",div(h1("GDP per Capita comparison in time"),style="width:1000px;height:50px;padding-left:30px;"),linebreaks(1),div(sidebarPanel(selectInput("countryI","Choose the GDP per Capita of the first country",unique(DatasetTS[,1])),
                                        selectInput("countryII","Choose the GDP per Capita of the second country",unique(DatasetTS[,1]))
      ),style="width:1000px;height:250px;padding-left:15px;"), div(plotOutput("distPlot"),style="width:1000px;height:250px;padding-left:30px;")),
      tabItem("Fourth", div(leafletOutput("world"),style="width:2000px;height:200px;padding-left:5px;"),linebreaks(10), sliderInput("range","Please choose the income range",min=500,max=120000,value=c(10000,90000),
                                                                                                                      step = 1000), div(textOutput("text")))
              )
      
     
      
      
    )
  )
  )
  
  
  
  






server <- function(input, output) {
  observeEvent(input$show, {output$dataset <- renderDataTable(Dataset)})
  output$myplot <- renderPlotly({
    plot <-plot_ly(Dataset,x=~get(input$xcol),y= ~get(input$ycol), type="scatter",mode = 'markers',
                   size= ~Population, colors='Paired', color=~Country,
                   sizes=c(12,52),
                   marker = list(opacity = 0.5, sizemode = 'diameter', symbol="circle",line=list(width=1,color='#FFFFFF')),
                   text=~paste('Country:',Country,'<br>Population (in millions):', Population/1000000))
    plot<-plot %>% layout(title="Graph between variables of interests", autosize=F,width=800,height=550,showlegend = FALSE,
                          xaxis=list(title=~input$xcol, 
                                     zerolinewidth = 1,
                                     ticklen = 2,
                                     gridwidth = 2),
                          yaxis = list(title = input$ycol,
                                       zerolinewidth = 1,
                                       ticklen = 2,
                                       gridwith = 2),
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)')
    
  } )
  
  output$sum1 <- renderPrint({
    
    summary( Dataset[paste(input$xcol)])
    
    
  })
  output$sum2 <- renderPrint({
    
    summary( Dataset[paste(input$ycol)])
    
    
  })
  
  output$distPlot <- renderPlot({
    subset1 <- filter(DatasetTS, country == input$countryI)
    subset2 <- filter(DatasetTS, country == input$countryII)
    
    
    ggplot(subset1,aes(x=year,y=gdpPercap))+geom_line(aes(color="GDP per capita of the first country "),size=3)+
      geom_line(data=subset2,aes(color="GDP per capita of the second country "  ),size=3)+
      xlab("Years") + ylab("GDP per capita") +
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1),axis.text = element_text())
  })
  
  newData<- reactive({filter(Dataset, Income>input$range[1] & Income<input$range[2])})
    
  
  
  
  output$world <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(data=newData(),lng= ~Longitude, lat= ~ Latitude,  popup = ~paste(paste('<b>', 'Name of country:', '</b>', Country), paste('<b>', 'Capital city:', '</b>', Capital_City),
                                                                                paste('<b>',  'Population ( in millions):', '</b>', Population/1000000),
                                                                                
                                                                                sep = '<br/>'))
    
  
  })
  
  output$text <- renderText({paste("Currently, you can see all the countries that have income between ",input$range[1],"$", " and ",input$range[2],"$.")})

  
  
  
}

shinyApp(ui, server)

