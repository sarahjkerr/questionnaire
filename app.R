
library(shiny)
library(ggplot2)
library(googlesheets)



cheeses <- c("Choose a Cheese!", "Rogue Caveman Blue", "Bella Vitano Merlot", "Drunken Goat", "Spanish Idiazabal", "Raw Milk Manchego", "Aged Emmenthaler")
fields <- c("cheese1","cheese2","chhes3","cheese4","cheese1_ranking","cheese2_ranking","cheese3_ranking","cheese4_ranking")
responses <- file.path("~/Program Misc/Test/responses")
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

gs_auth(new_user = TRUE)

ui <- fluidPage( theme = "www/bootstrap1.css",
    titlePanel("User Form"),
    
    mainPanel(
      fluidRow(
        column(3, selectInput("cheese1", h5("First Cheese"), choices = cheeses, selected = 1)),
        column(3, selectInput("cheese2", h5("Second Cheese"), choices = cheeses, selected = 1)),
        column(3, selectInput("cheese3", h5("Third Cheese"), choices = cheeses, selected = 1)),
        column(3, selectInput("cheese4", h5("Fourth Cheese"), choices = cheeses, selected = 1))
      ),
      fluidRow(
        column(3, sliderInput("cheese1_rating", h5("Rate the Second Cheese!"), min = 1, max = 5, value = 3)),
        column(3, sliderInput("cheese2_rating", h5("Rate the Second Cheese!"), min = 1, max = 5, value = 3)),
        column(3, sliderInput("cheese3_rating", h5("Rate the Third Cheese!"), min = 1, max = 5, value = 3)),
        column(3, sliderInput("cheese4_rating", h5("Rate the Fourth Cheese!"), min = 1, max = 5, value = 3))
      ),
      actionButton("submit", label = "All Gouda!"),
      hr(),
      fluidRow(column(2, verbatimTextOutput("value")))
    )
  )


server <- function(input, output) {
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  saveData <- function(data) {
    fileName <- sprintf("%s.csv",
                        humanTime())
    
    write.csv(x = data, file = file.path(responses, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  observeEvent(input$submit, {
    saveData(formData())
  })
}

shinyApp(ui = ui, server = server)
