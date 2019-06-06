library(shiny)
library(ggplot2)
library(googlesheets)
library(dplyr)

#The options vector will house the continuously growing master list of options. This should be updated manually as new options
#are added to the set (for now)
options <- c('Choose An Option!', 'Option1', 'Option2', 'Option3', 'Option4', 'Option5')

#Code for UI

ui <- navbarPage('User Form',
  
  tabPanel('User Input',
           mainPanel(
             fluidRow(
               column(4, selectInput('Option1', h5('First Option'), choices = options, selected = 1)),
               column(4, selectInput('Option2', h5('Second Option'), choices = options, selected = 1)),
               column(4, selectInput('Option3', h5('Third Option'), choices = options, selected = 1))
             ),
             fluidRow(
               column(4, sliderInput('Rating1', h5('Rate the First Option!'), min = 1, max = 5, value = 3)),
               column(4, sliderInput('Rating2', h5('Rate the Second Option!'), min = 1, max = 5, value = 3)),
               column(4, sliderInput('Rating3', h5('Rate the Third Option!'), min = 1, max = 5, value = 3))
             ),
             dateInput('Date', label = h5('When did you encounter this option?'), value = '2019-01-01'),
             
             hr(),
             fluidRow(
               column(6, verbatimTextOutput('value'))
             ),
             actionButton('submit', label = 'Submit!')
           )
      ),
  tabPanel('Data',
           tableOutput('xyz')
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
  })
}

#Code to run app
shinyApp(ui = ui, server = server)

