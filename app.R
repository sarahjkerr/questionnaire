library(shiny)
library(googlesheets)
library(dplyr)
library(data.table)
library(ggplot2)
library(shinyjs)
library(stringi)
library(ggthemes)

#The options vector will house the continuously growing master list of options. This should be updated manually as new options
#are added to the set (for now-> use API and auto-populate down the road?)
options <- c('Choose An Option!', 'Bella Vitano Merlot', 'Drunken Goat', 'Rogue Caveman Blue', 'Truffle Tartufo', 
             'Spanish Idiazabal', 'Truffle Tremor', 'Raw Milk Manchego')

#Storage setup
sheet_key <- 
  extract_key_from_url('https://docs.google.com/spreadsheets/d/1hrVLwoqleFXwr0jcHwIsCOYJg2LYUQtrsLDECLJ4hHs/edit?usp=sharing')

sheet_registration <- sheet_key %>%
  gs_key()

#Code for UI
ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel('Cheese Rater'),
  
  sidebarLayout(
    sidebarPanel(
      dateInput('Date', label = h5('When did you eat these cheeses?'), value = '2019-04-01'),
      
      hr(),
      fluidRow(
        column(6, verbatimTextOutput('value'))
      ),
      selectInput('Option1', h5('First Cheese'), choices = options, selected = 1),
      sliderInput('Rating1', h5('Rate the First Cheese!'), min = 1, max = 5, value = 3),
      selectInput('Option2', h5('Second Cheese'), choices = options, selected = 1),
      sliderInput('Rating2', h5('Rate the Second Cheese!'), min = 1, max = 5, value = 3),
      selectInput('Option3', h5('Third Cheese'), choices = options, selected = 1),
      sliderInput('Rating3', h5('Rate the Third Cheese!'), min = 1, max = 5, value = 3),
      actionButton('submit', label = 'Submit!'),
      hidden(
        textInput('ID', label = h5('Submission ID'), value = (stri_rand_strings(1,10)))
      )
      ),

    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Plot of Cheese Ratings', plotOutput('bar', 'height' = 500)),
                  tabPanel('See Raw Cheese Data',tableOutput('xyz'))
      )
    )
  )
)

#Code for server
server <- function(input, output, session) {
  sheet_data <- sheet_registration %>%
    gs_read(ws = 'Sheet1')

  observeEvent(input$submit, {
    
    
    
    framework_df <- data.frame('Option1' = input$Option1, 'Option2' = input$Option2, 'Option3' = input$Option3,
                          'Rating1' = input$Rating1, 'Rating2' = input$Rating2, 'Rating3' = input$Rating3,
                          'Date' = input$Date, 'ID' = input$ID)
    if(!is.null(input$Option1)){
      final_df <- rbind(sheet_data, framework_df)
    } else {
      final_df <- sheet_data
    }
    
    output$xyz <- renderTable({(final_df)})
    
    gs_add_row(sheet_registration, ws = 'Sheet1', input = framework_df, verbose = TRUE)
    
    data_to_plot <- melt(setDT(final_df), measure=patterns(c('^Option', '^Rating')),
                    value.name = c('Option','Rating'))[, variable:=NULL][] %>%
      group_by(Option) %>%
      summarise(avg_rating = mean(Rating, na.rm = TRUE))
    
    output$bar <- renderPlot({
      ggplot(data_to_plot, aes(x = Option, y = avg_rating)) + 
        geom_bar(aes(fill=Option),
                 stat = 'identity',
                 color = 'black') +
        ggtitle('Mean Rating per Option') +
        xlab('Options') +
        ylab('Average Rating') +
        theme_gdocs() +
        scale_color_gdocs()
        
    })
    
  })
}

#Code to run app
shinyApp(ui = ui, server = server)
