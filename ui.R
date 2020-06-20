
library(shiny)
library(DT)
library(lubridate)
library(ggplot2)
library(markdown)


data <- data.frame()
val_data <- data.frame()
ui <- fluidPage(
  
  navbarPage("OutBreak Detector",inverse = T, id= "navbarset",
   tabPanel("Uploading Files",
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        actionButton("sample_button", label = "Run with example Data", class = "btn-info"),
        
        actionButton("data_validation", label = "Validate the Data", class = "btn-primary")
        
      ),
         mainPanel(
             dataTableOutput('contents'))
    )
),
      tabPanel("Validated Data", value = "val_data_page",
               dataTableOutput('table'),
               
               
      actionButton("go_arima_page", label = "Go to Arima Page", class = "btn-success" )),




      tabPanel("Arima Helper", value ="arima_run_page", tabsetPanel(type = "tabs",
                                                                    tabPanel("Arima",
                                                                             fluidRow(
                                                                               column(4,numericInput("train", "Number of Years to Train", 1, min = 1, max = 9)),
                                                                               column(4,selectInput("slide", "Sliding Window",
                                                                                                    c("7" = 7,
                                                                                                      "14" = 14,
                                                                                                      "21" = 21), selectize = F)),
                                                                               column(4,selectInput("cusmet", "Cusum Parameters",
                                                                                                    c("Aggresive" = "aggressive",
                                                                                                      "Moderate" = "moderate",
                                                                                                      "Routine" = "routine"),selectize = F))),
                                                                             p("After 20 seconds, you can click  the tabs above for the results"),
                                                                             actionButton("get_results", label = "Get Results", class = "btn-warning"),   ),
                                                                    tabPanel("Arima Data", dataTableOutput('arima_data')),
                                                                    tabPanel("Residual Plot", plotOutput('arima_summary')),
                                                                    tabPanel("Final Plots", plotOutput('final_plot'))))
    )                                                 
)
                    

server <- function(input, output, session) {
  
  #----------------------------------------------------------------------------
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    data <<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote)

  })
  output$contents <- renderDataTable(getData(),
                                     options = list(
                                       searching = FALSE, 
                                       pageLength = 15))
  #output$contents <- renderTable(getData())
  
  
  #-------------------------------------------------------------------------
  
  observeEvent(input$sample_button,{
    data <<- read.csv("./OutbreakDetector_sampledata.csv",header=input$header, sep=input$sep, 
                      quote=input$quote)

    output$contents <- renderDataTable(read.csv("./OutbreakDetector_sampledata.csv"),options = list(
      searching = FALSE, 
      pageLength = 15))
  })
  
  
  #-------------------------------------------------------------------------
  
  
  
  observeEvent(input$data_validation, {
    data <<- data[,-4] 
    colnames(data) <<- c("Date","Count","Holiday")
    source('data_validation.R')
    val_data <<- input.date.check(input.date.check(data))
    #output$table <- renderText({input.sanity.check(val_data)})
    #output$table <- renderDataTable(data[1])
    output$table <- renderDataTable(val_data,
                                    options = list(
                                      pageLength = 15,
                                      searching = FALSE)
    )
    updateTabsetPanel(session, "navbarset",
                      selected = "val_data_page")
    
  })
  
  observeEvent(input$go_arima_page,{
               updateTabsetPanel(session, "navbarset",
                                 selected = "arima_run_page")
  })
  
  

  #-------------------------------------------------------------------------
  observeEvent(input$get_results,{
    
    source('03_arima_yardimci.R')
    results<<- arima_run(val_data,input$train,input$slide,input$cusmet)
    
    
    
    output$arima_summary <- renderPlot(ggtsdisplay(residuals(results[[2]],type ="normalized"), main= "ARIMA errors"))
    output$arima_data <- renderDataTable(results[[1]],
                                         options =list(
                                           pageLength = 10,
                                           searching = F)
                                         )
    output$final_plot <- renderPlot(plot(results[[5]]))
                })
}

shinyApp(ui, server)
