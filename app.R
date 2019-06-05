library(shiny)

#The options vector will house the continuously growing master list of options. This should be updated manually as new options
#are added to the set (for now)
options <- c('Choose An Option!', 'Option1', 'Option2', 'Option3', 'Option4', 'Option5')

#The app will only allow users to rank up to 3 options for now
fields <- c('Option1', 'Option2', 'Option3', 'Rating1','Rating2','Rating3')

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
           dataTableOutput('responses')
  )
)

#Code for server
#This half works, but needs a lot of updating to acutally render user input. 

server <- function(input, output, session) {
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists('responses')) {
      response <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists('responses')) {
      responses
    }
  }
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  output$responses <- renderDataTable({
    input$submit
    loadData()
  })
}

#Code to run app
shinyApp(ui = ui, server = server)
