# makecondition -----------

library(econR)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)



sample <- read.csv(. %//% "support/SampleData.csv")
sample$TARGET <- as.character(sample$TARGET)

sample2 <- read.csv(. %//% "support/SampleData2.csv")
sample2$TARGET <- as.character(sample2$TARGET)

input <- "AMT_ANNUITY"
input2 <- "CONTRACT_TYPE"


# server --------

server <- function(input, output) {
  
  
  select1 <- reactive({
    select(sample, input$varible1)
  })
  select2 <- reactive({
    select(sample, input$varible2)
  })
  
  
  select21 <- reactive({
    select(sample, input$varible21)
  })
  select22 <- reactive({
    select(sample, input$varible22)
  })
  
  
  output$mytable1 = renderDT(
    sample #, options = list(lengthChange = FALSE)
  )
  output$mytable2 = renderDT(
    sample2#, options = list(lengthChange = FALSE)
  )
  output$mytable3 = renderDT(
    sample, options = list(lengthChange = FALSE)
  )
  
  
  
  ##################
  
  output$plot1 <- plotly::renderPlotly({
    if (is.character(select1()[1, 1])) {
      if (is.character(select2()[1, 1])) {
        plotOutcome <- ggplot(data = sample) +
          geom_count(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample) +
          geom_col(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
    }
    else {
      if (is.character(select2()[1, 1])) {
        plotOutcome <- ggplot(data = sample) +
          geom_col(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample) +
          geom_smooth(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
    }
    plotly::ggplotly(plotOutcome)
  })
  
  
  output$plot2 <- plotly::renderPlotly({
    if (is.character(select21()[1, 1])) {
      if (is.character(select22()[1, 1])) {
        plotOutcome <- ggplot(data = sample2) +
          geom_count(aes(
            x = get(input$varible21),
            y = get(input$varible22)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample2) +
          geom_col(aes(
            x = get(input$varible21),
            y = get(input$varible22)
          ))
      }
    }
    else {
      if (is.character(select22()[1, 1])) {
        plotOutcome <- ggplot(data = sample2) +
          geom_col(aes(
            x = get(input$varible21),
            y = get(input$varible22)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample2) +
          geom_smooth(aes(
            x = get(input$varible21),
            y = get(input$varible22)
          ))
      }
    }
    plotly::ggplotly(plotOutcome)
  })
  
  ##################
}

shinyApp(
  ui = {tags$body(
    fluidPage(
      
      # Application title
      titlePanel("Data Visualization by Shiny"),
      
      
      # Show a plot of the generated distribution
      #mainPanel(
        
        tabsetPanel(
          id = 'dataset',
          tabPanel(
            
          "sample1", mainPanel(plotlyOutput("plot1")),
                   sidebarPanel(
                     selectInput("varible1", "Choose a varible1 (x axis):",
                                 list(`Discrete` = list("CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY"),
                                      `Continuous` = list("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY"),
                                      `Target` = list("TARGET")),
                                 width = "200px")
                   ),
                   
                   sidebarPanel(
                     selectInput("varible2", "Choose a varible2 (y axis):",
                                 list(`Discrete` = list("CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY"),
                                      `Continuous` = list("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY"),
                                      `Target` = list("TARGET")),
                                 width = "200px")
                   ),
                   
            fluidRow(
              column(12,div(style = "height:2;background-color: yellow;", "have some space"))),
            
                   DT::dataTableOutput("mytable1")
                   ),
          
          tabPanel(
            
          "sample2", mainPanel(plotlyOutput("plot2")),
                sidebarPanel(
                  selectInput("varible21", "Choose a varible1 (x axis):",
                          list(`Discrete` = list("CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY"),
                               `Continuous` = list("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY"),
                               `Target` = list("TARGET")),
                          width = "200px")
            ),
            
            sidebarPanel(
              selectInput("varible22", "Choose a varible2 (y axis):",
                          list(`Discrete` = list("CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY"),
                               `Continuous` = list("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY"),
                               `Target` = list("TARGET")),
                          width = "200px")
            ),
            
            fluidRow(
              column(12,div(style = "height:2;background-color: yellow;", "have some space"))),
            
            DT::dataTableOutput("mytable2")
          )
          
        )
        # tabsetPanel(
        #   tabPanel("Plot1", plotlyOutput("plot1")),
        #   tabPanel("Plot2", plotlyOutput("plot2"))
        # )
      #)
      
    ))}
  ,
  server
)
