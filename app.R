library(shiny)
library(googlesheets)
library(dplyr)
library(httr)

#The options vector will house the continuously growing master list of options. This should be updated manually as new options
#are added to the set (for now)
options <- c('Choose An Option!', 'A', 'B', 'C', 'D', 'E')

#Storage setup
sheet_key <- 
  extract_key_from_url('https://docs.google.com/spreadsheets/d/1hrVLwoqleFXwr0jcHwIsCOYJg2LYUQtrsLDECLJ4hHs/edit?usp=sharing')

sheet_registration <- sheet_key %>%
  gs_key()

sheet_data <- sheet_registration %>%
  gs_read(ws = 'Sheet1')

#Code for UI

ui <- fluidPage(
  titlePanel('User Input'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('Option1', h5('First Option'), choices = options, selected = 1),
      sliderInput('Rating1', h5('Rate the First Option!'), min = 1, max = 5, value = 3),
      selectInput('Option2', h5('Second Option'), choices = options, selected = 1),
      sliderInput('Rating2', h5('Rate the Second Option!'), min = 1, max = 5, value = 3),
      selectInput('Option3', h5('Third Option'), choices = options, selected = 1),
      sliderInput('Rating3', h5('Rate the Third Option!'), min = 1, max = 5, value = 3),
      dateInput('Date', label = h5('When did you encounter this option?'), value = '2019-01-01'),
             
             hr(),
             fluidRow(
               column(6, verbatimTextOutput('value'))
             ),
             actionButton('submit', label = 'Submit!')
      ),

    mainPanel(
      tableOutput('xyz')
      #plotOutput('bar', 'height' = 500)
    )
  )
)



#Code for server

server <- function(input, output, session) {
  
  data_structure <- data.frame(Option1 = c('A','B'), Option2 = c('D','E'), Option3 = c('F','G'),
                               Rating1 = c(1,5), Rating2 = c(2,3), Rating3 = c(4,3),
                               Date = c('2019-05-04','2019-06-01'))
  
  observeEvent(input$submit, {
    
    test_df <- data.frame('Option1' = input$Option1, 'Option2' = input$Option2, 'Option3' = input$Option3,
                          'Rating1' = input$Rating1, 'Rating2' = input$Rating2, 'Rating3' = input$Rating3,
                          'Date' = input$Date)
    if(!is.null(input$Option1)){
      abc <- rbind(data_structure, test_df)
    } else {
      abc <- test_df
    }
    
    output$xyz <- renderTable({ head(abc)})
    
    #output$bar <- renderPlot({
      #barplot(colSums(abc[,c('Options')]))
    #})
  })
}

#Code to run app
shinyApp(ui = ui, server = server)

